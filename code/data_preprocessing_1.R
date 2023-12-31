# install.packages("tibbletime")
# Necessary imports
library(tidyverse) 
library(tibbletime)
library(janitor)
library(lsa)

# Read in dataset
df <- read_csv("./datasets/event_master_file_D10_R500_RNG1000_sprint2_shou.csv") 

# Preliminary data tidying and organization before collapsing stopping rows. We must account for "stopping at students" 
# behavior, so we have to 
df <- df %>%
  mutate(start = timestamp, end = 0) %>% # Make start and ending columns
  select(-timestamp) %>%
  relocate(start, .after = "periodID") %>%
  relocate(end, .after = "start") %>% 
  # Set all ending timestamps of instantaneous events to their starting timestamps
  mutate(end = if_else( (event == "Talking to class: ON-task" | event == "Talking to class: OFF-task" |
                           event == "Talking to student: ON-task" | event == "Talking to student: OFF-task" |
                           event == "Monitoring class: Moving" | event == "Monitoring class: Fixed" |
                           event == "Monitoring student" | event == "Correct attempt" |
                           event == "Incorrect attempt" | event == "Raising hand" | event == "Hint Request" | 
                           event == "Inactive" | event == "Talking to small group: ON-task" | 
                           event == "Talking to small group: OFF-task" | event == "Questioning: On-Task" |
                           event == "Questioning: Off-Task" | event == "Hint request"), start, 0)) %>%
  # Extract locations
  mutate(locationStart = str_locate(content, "\\("), locationEnd = str_locate(content, "\\)")) %>%
  mutate(locationStart = locationStart[,"start"], locationEnd = locationEnd[,"start"]) %>%
  mutate(location =  case_when(event == "Stopping" ~ str_sub(content, locationStart+1, locationEnd-1), 
                               event != "Stopping" ~ NA)  ) %>%
  select(-c(locationStart, locationEnd)) %>%
  arrange(start) %>%
  mutate(students = case_when((str_detect(subject, "Stu_") & event == "Stopping") ~ subject)) %>% 
  group_by(location, students) %>% 
  mutate(group_id = cur_group_id()) %>%
  nest() 

# A dataframe of items to join later
not_stopping <- df$data[[1]] 
# Delete the first row of our data; it contains all rows that aren't stopping. Now, we are just left with stopping data
df <- df[-1,] 

# Define a function to impute start and end given a dataframe using the timestamp of the first item in the group and 
# the last item in the group
time_collapser <- function(df) {
  first_row_ts <- head(df, n = 1)$start
  last_row_ts <- tail(df, n = 1)$start
  
  df["start"] <- first_row_ts
  df["end"] <- last_row_ts
  
  distinct(df)
}

# Now, our dataframe is a list of dataframes. Use rowwise to pass each dataframe to time collapser. Then, unnest the new 
# dataframes, rejoin them to the old dataset, and prepare the data to collapse moving rows 
df <- df %>%
  rowwise() %>%
  mutate(new = list(time_collapser(data))) %>%
  select(location, new) %>%
  unnest(cols = c(new)) %>%
  relocate(location, .after = modality) %>%
  full_join(not_stopping) %>%  # Join the new stopping dataset with the non-stopping dataset
  ungroup() %>%
  select(-c(group_id, students)) %>%
  mutate(is_moving = case_when(event == "Moving" ~ TRUE, event != "Moving" ~ FALSE)) %>%
  mutate(group_id = 0) %>%
  arrange(start, subject) 

# It is particularly difficult to group the moving data now. We could group all moving data, but we want to group by each
# continuous, uninterrupted sequence of moving rows. And, unlike the stopping rows, we don't have locations to base our grouping
# on. As such, we will find each index in which the event isn't moving  and store it as the vector "idx". Then, we'll take 
# the next number from our sequence "numbers"  (starting at 0), use that as the group id of our next uninterrupted sequence 
# of moving rows, and then have the rows from that index to the previous index in the "idx" vector by placing a sequence 
# of the group id to the corresponding number of rows calculated in "difference". 
numbers <- 1:10000 # An arbitrary sequence of numbers, enough to encapsulate all the groups of moving data
idx <- which(df$is_moving == FALSE) # Store which indices aren't moving
# Prepend 1 to the vector of indices, since there is no non-moving event preceding it and the dataframe starts with moving
idx <- append(idx, 1, after = 0)
difference <- diff(idx) # Store a vector of the differences between indices; these will be the number of rows for each group
df$group_id <- rep(numbers[seq(idx)], c(difference <- diff(idx), nrow(df) - sum(difference))) # Place group IDs

# Save a dataset of non-moving rows to join later
non_moving <- df %>% 
  filter(is_moving == FALSE) %>%
  select(-is_moving)

# Get all moving rows, nest them into datasets depending on their groups, perform a rowwise time collapse operation on them, 
# and deselect the now-irrelevant column
moving <- df %>%
  filter(is_moving == TRUE) %>%
  group_by(group_id) %>%
  nest() %>%
  rowwise() %>%
  mutate(new = list(time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  select(-is_moving) 

# Join the non-moving and moving rows back together again
df <- full_join(non_moving, moving) %>%
  select(-group_id)

# Now, we're just worrying about entering, exiting gaming state; entering, exiting idle state; entering, exiting misuse state;
# entering, exiting struggle state.

# Save a dataset of rows that don't include relevant "state" events
non_states <- df %>%
  filter(end != 0)

# Save a dataset of rows that include "state" events
states <- df %>%
  filter(end == 0)

# Split states dataset into gaming, idle, misuse, struggle
gaming <- states %>%
  filter(event == "Entering gaming State" | event ==  "Exiting gaming State")

idle <- states %>%
  filter(event == "Entering idle State" | event ==  "Exiting idle State")

misuse <- states %>%
  filter(event == "Entering misuse State" | event ==   "Exiting misuse State")

struggle <- states %>%
  filter(event == "Entering struggle State" | event ==   "Exiting struggle State")

# Group gaming sequences. This groups the pairs into groups that are unique to each day, period, and actor. 
gaming <- gaming %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

# The above groups aren't good enough, they don't represent each pair of entering and exiting the gaming state. 
# Write a mathematical lambda function to generate relevant pair indices
gaming$pairs_gaming <- c(1,1) + rep(seq(0, 275, 1), each = 2)

# Nest gaming
gaming <- gaming %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_gaming) %>%
  nest() 

# Define a function like time_collapser, except for pairs rather than whole datasets
pairs_time_collapser <- function(data) {
  data[1, ]$end <- data[2, ]$start
  data <- head(data, n = 1)
}

# Perform the time collapsing rowwise on each nested dataframe, unnest and ungroup, deselect the pair ID column, and rename
# the event to something clearer
gaming <- gaming %>%
  rowwise() %>%
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  ungroup() %>% 
  select(-pairs_gaming) %>%
  mutate(event = "Gaming State") 

# Repeat this process for the other 3 states
idle <- idle %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

idle$pairs_idle <- c(1,1) + rep(seq(0,93,1), each = 2)

idle <- idle %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_idle) %>%
  nest() %>%
  rowwise() %>%
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  ungroup() %>% 
  select(-pairs_idle) %>%
  mutate(event = "Idle State") 

misuse <- misuse %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

misuse$pairs_misuse <- c(1,1) + rep(seq(0,167,1), each = 2)

misuse <- misuse %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_misuse) %>%
  nest() %>%
  rowwise() %>%
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  ungroup() %>% 
  select(-pairs_misuse) %>%
  mutate(event = "Misuse State") 

struggle <- struggle %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

struggle$pairs_struggle <- c(1,1) + rep(seq(0,36,1), each = 2)

struggle <- struggle %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_struggle) %>%
  nest() %>%
  rowwise() %>%
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  ungroup() %>% 
  select(-pairs_struggle) %>%
  mutate(event = "Struggle State") 

# Rejoin all partitioned dataframes
gaming_idle <- full_join(gaming, idle)
misuse_struggle <- full_join(misuse, struggle)
states <- full_join(gaming_idle, misuse_struggle) 
df <- full_join(non_states, states) 

# Read in the locations dataset 
df_locations <- read_csv("./datasets/teacher_position_sprint1_shou (1).csv") 
# Remove rows with duplicated timestamps
df_locations <- df_locations[!duplicated(df_locations[ , "time_stamp"]), ] 

# Relocate relevant columns to join with. Verify that important variables are numeric and rounded. 
df_locations <- df_locations %>%
  mutate(time_stamp = as.numeric(time_stamp), dayID = as.numeric(dayID), periodID = as.numeric(periodID)) %>%
  relocate(periodID, .before = "time_stamp") %>%
  relocate(dayID, .before = "periodID") %>%
  arrange(time_stamp) %>%
  mutate(time_stamp = round(time_stamp, digits = 0))

# Verify that important variables are numeric and rounded. Join locations, deselect irrelevant columns, pull relevant locations
# out of relevant columns and concatenate them in a readable format, then attach them if the rows have a teacher actor and 
# aren't stopping events
df <- df %>%
  mutate(start = as.numeric(start), end = as.numeric(end), dayID = as.numeric(dayID), periodID = as.numeric(periodID)) %>%
  mutate(start = round(start, digits = 0)) %>%
  mutate(end = round(end, digits = 0)) %>% 
  left_join(df_locations, by = c("dayID", "periodID", "start" = "time_stamp")) %>%
  select(-c(tag19_X, tag20_X, tag19_Y, tag20_Y, tag19_score, tag20_score))  %>%
  mutate(imp_loc = str_c(as.character(chosen_X), ", ", as.character(chosen_Y))) %>%
  mutate(silly = if_else(actor == "teacher" & event != "Stopping", TRUE, FALSE))  %>%
  mutate(location_temp = location) %>%
  mutate(location = case_when((silly == TRUE) ~ imp_loc, (silly == FALSE) ~ location_temp)) %>%
  select(-c(chosen_X, chosen_Y, imp_loc, silly, location_temp)) %>%
  ungroup() %>%
  arrange(dayID, periodID, start)

# Impute teacher location based on the mean of the adjacent time points (the location of the teacher right before, the location 
# of the teacher right after). Instances when location is NA twice in a row occur at the same timestamp; thus, we can have the same 
# imputation for both instances. Note: This section of the code is redacted later on, but not removed so as to not 
# harm functionality

# Define a function to impute teacher locations
teacher_imputer <- function(df) {
  
  # Make a copy of the dataframe 
  df_copy <- data.frame(df)
  # Get all indices in which the teacher locations are NA 
  idx_list <- which(is.na(df_copy[,"location"]))
  
  # Save the indices that have NA locations before and after; these are special cases. Thus, look for consecutive values
  # and save them.
  # Iterate over the list, skipping the last element and the first element (these are confirmed non-consecutive)
  special_idx <- c()
  for (i in 2:(length(idx_list)-1)) {
    if ((idx_list[i-1] == idx_list[i] - 1) | (idx_list[i+1] == idx_list[i] + 1)) {
      special_idx <- append(special_idx, idx_list[i])
    }
  }
  
  # Remove the special indices from the idx_list 
  idx_list <- idx_list[-which(idx_list %in% special_idx)]
  
  # First, impute the rows from idx_list 
  for (j in idx_list) {
    df_copy$V1[j] <- (df_copy$V1[j-1] + df_copy$V1[j+1])/2
    df_copy$V2[j] <- (df_copy$V1[j-1] + df_copy$V2[j+1])/2
    df_copy$location[j] <- str_c(as.character(df_copy$V1[j]), ", ", as.character(df_copy$V2[j]))
  }
  
  # Then, impute the rows from special_idx
    # Split special_idx into consecutive sequences. Data wasn't too big, hardcoded this for time's sake
    sequence_1 <- list(269, 270)
    sequence_2 <- list(829, 830, 831, 832, 833, 834)
    sequence_3 <- list(988, 989)
    sequence_4 <- list(1465, 1466, 1468, 1469, 1470, 1471, 1472, 1473, 1474, 1475)
    sequence_5 <- list(1711, 1712)
    sequence_6 <- list(1805, 1806)
    sequence_7 <- list(2144, 2145)
    sequence_8 <- list(2154, 2155)
    sequence_9 <- list(2240, 2241)
    sequence_10 <- list(2275, 2276)
    sequence_11 <- list(2765, 2766, 2767)
    list_sequences <- list(sequence_1, sequence_2, sequence_3, 
                          sequence_4, sequence_5, sequence_6,
                          sequence_7, sequence_8, sequence_9, sequence_10, sequence_11)
    # For all sequences
    for (x in list_sequences) {
      # Get first idx in the sequence
      first_idx <- x[[1]]
      # Get last idx in the sequence
      last_idx <- x[[length(x)]]
      # Find previous row in teacher dataframe that isn't NA
      previous_x <- df_copy$V1[first_idx - 1]
      previous_y <- df_copy$V2[first_idx - 1]
      # Find next row that isn't NA 
      next_x <-  df_copy$V1[last_idx + 1]
      next_y <- df_copy$V2[last_idx + 1]
      # Take the average of those two locations, x and y
      x_result <- (previous_x + next_x) / 2
      y_result <- (previous_y + next_y) / 2
      location_result <- str_c(as.character(x_result), ", ", as.character(y_result))
      # Impute those locations for everything in that sequence, x and y 
      for (i in x) {
        df_copy$location[i] <- location_result
      }
    }
      
  # Return new dataframe
  return(df_copy)
}

# To join later 
df_not_teacher <- df %>%
  filter(actor != "teacher")

# Split teacher df away from old df
df_teacher <- df %>%
  filter(actor == "teacher") 

# Before calculating the means and using the teacher_imputer, split location into its x and y coords 
df_teacher[, 11:12] <- str_split_fixed(df_teacher$location, ", ", n = 2)

# Make sure V1 and V2 split are numeric
df_teacher <- df_teacher %>%
  mutate(V1 = as.numeric(V1), V2 = as.numeric(V2)) 

# Impute values and deselect irrelevant columns
df_teacher <- teacher_imputer(df_teacher) %>%
  select(-c(V1, V2))

# Merge teacher and non_teacher data
df <- full_join(df_not_teacher, df_teacher) %>%
  arrange(dayID, periodID, start)

# Read in CSVs containing student locations and student metdata; use only the learning markers for the student metadata
student_locations <- read_csv("./datasets/student_position_sprint1_shou.csv") %>%
  select(-SeatNum) 
student_learning <- read_csv("./datasets/meta-data-aied.csv") %>%
  select(anon_student_id, ck_pre, ck_lg, pk_pre, pk_lg)

# Join the student locations and student metadata to see which student IDs have missing data in either dataset
check_df <- full_join(student_locations, student_learning, by = c("actual_user_id" = "anon_student_id"))

# Make a dataframe with all the distinct students in the original dataframe 
students_df <- df %>%
  select(c(actor, subject)) %>%
  filter(actor != "teacher" & actor != "tutor") %>%
  select(-subject) %>%
  distinct() %>%
  mutate(bool = str_detect(actor, "Stu")) %>%
  filter(bool) %>%
  select(-bool) %>%
  rename(students = actor)

silly <- check_df %>%
  select(actual_user_id, anon_user_id) %>%
  drop_na()

# Strip check_df of unnecessary columns
check_df <- check_df %>%
  select(-c(actual_user_id))

# Join check_df with students_df
students_df <- left_join(students_df, check_df, by = c("students" = "anon_user_id"), multiple ="all") %>%
  distinct() %>%
  arrange(students, `DayID`, `PeriodID`) %>%
  mutate(location_dataset = case_when((!is.na(X) & !is.na(Y)) ~ TRUE, (is.na(X) | is.na(Y)) ~ FALSE)) %>%
  mutate(metadata_dataset_ck_pre = case_when(!is.na(ck_pre) ~ TRUE, is.na(ck_pre) ~ FALSE)) %>%
  mutate(metadata_dataset_pk_pre = case_when(!is.na(pk_pre) ~ TRUE, is.na(pk_pre) ~ FALSE)) %>%
  mutate(metadata_dataset_ck_lg = case_when(!is.na(ck_lg) ~ TRUE, is.na(ck_lg) ~ FALSE)) %>%
  mutate(metadata_dataset_pk_lg = case_when(!is.na(pk_lg) ~ TRUE, is.na(pk_lg) ~ FALSE)) %>%
  select(students, location_dataset, metadata_dataset_ck_pre, metadata_dataset_ck_lg, metadata_dataset_pk_pre, 
         metadata_dataset_pk_lg) %>%
  distinct()

# Make a dataframe that counts the number of T/F occurrences in all columns; used for data discovery
location_counts <- students_df %>%
  group_by(location_dataset) %>%
  summarize(total_location= n())

ck_pre_counts <- students_df %>%
  group_by(metadata_dataset_ck_pre) %>%
  summarize(total_ck_pre = n())

ck_lg_counts <- students_df %>%
  group_by(metadata_dataset_ck_lg) %>%
  summarize(total_ck_lg = n())

pk_pre_counts <- students_df %>%
  group_by(metadata_dataset_pk_pre) %>%
  summarize(total_pk_pre = n())

pk_lg_counts <- students_df %>%
  group_by(metadata_dataset_pk_lg) %>%
  summarize(total_pk_lg = n())

counts_students_df_temp1 <- cbind(ck_pre_counts, ck_lg_counts)
counts_students_df_temp2 <- cbind(pk_pre_counts, pk_lg_counts)
counts_students_df_temp3 <- cbind(location_counts, counts_students_df_temp1)
counts_students_df <- cbind(counts_students_df_temp3, counts_students_df_temp2) %>%
  select(c(location_dataset, total_location, total_ck_pre, total_ck_lg, total_pk_pre, total_pk_lg)) %>%
  pivot_longer(location_dataset, names_to = "data", values_to = "missing") %>%
  select(-data) %>%
  relocate(missing, .before = total_location) %>%
  rename(location = `total_location`, ck_pre = `total_ck_pre`, pk_pre = `total_pk_pre`, 
         ck_lg = `total_ck_lg`, pk_lg = `total_pk_lg`, not_missing = `missing`)


# Join anon "coloranimal" IDs through silly
students_df <- left_join(students_df, silly, by = c("students" = "anon_user_id")) %>%
  distinct() %>%
  rename(animal_id = actual_user_id ) %>%
  relocate(animal_id, .after = students)

# Output csv for counts_student_df
write.csv(counts_students_df, "./datasets/metadata_counts_students.csv", row.names = FALSE)

# Output csv, arranged by dayID, periodID, and timestamp.
write.csv(df, "./datasets/collapsed_AI_classroom_data.csv", row.names = FALSE)

# Run other R script, that depends on above written CSV
source("./code/data_preprocessing_2.R")

# Output csv for students_df, a dataframe that checks whether or not individual students in df are in the student location df
# and/or the student metadata df
write.csv(students_df, "./datasets/students.csv")

# Read in CSV again; more data organization before model 0 
df <- read.csv("./datasets/collapsed_AI_classroom_data.csv")

# Pivot wider distilled_event columns 
df <- df %>%
  mutate(yesno = 1) %>%
  distinct() %>%
  pivot_wider(names_from = "distilled_event", values_from = "yesno", values_fill = 0) %>%
  arrange(dayID, periodID, start)

# Get rid of "no student seated" rows
df <- df %>%
  mutate(no_stud_seated = ifelse((str_detect(actor, "no student seated") | str_detect(subject, "no student seated")) & (!is.na(actor) & !is.na(subject)), TRUE, FALSE)) %>%
  filter(no_stud_seated != TRUE) %>%
  select(-no_stud_seated)

# Get rid of Stu_ad3644e04bb6c4380286ec229aa48df1, Stu_4ae0ba339faed21609e388896a3ca39a
df <- df %>%
  filter(!str_detect(actor,"Stu_ad3644e04bb6c4380286ec229aa48df1") | is.na(actor)) %>%
  filter(!str_detect(subject,"Stu_ad3644e04bb6c4380286ec229aa48df1") | is.na(subject)) %>%
  filter(!str_detect(actor,"Stu_4ae0ba339faed21609e388896a3ca39a") | is.na(actor)) %>%
  filter(!str_detect(subject,"Stu_4ae0ba339faed21609e388896a3ca39a") | is.na(subject))

# Join student locations for remaining students (students that have them) 

  # In the student_locations dataframe, add a column X, Y for student locations. Rename DayID -> dayID and PeriodID -> periodID
  student_locations <- student_locations %>%
    mutate(student_loc = str_c(as.character(X), ", ", as.character(Y))) %>%
    rename(dayID = DayID) %>%
    rename(periodID = PeriodID)
  
  # If actor is a student, pull it out into a column "anon_user_id" 
  df <- df %>%
    mutate(anon_user_id = case_when(str_detect(actor, "Stu") ~ actor)) 
  
  # Then, join the dataframes by anon_user_id, dayID, and periodID
  df <- left_join(df, student_locations, by = c("periodID" = "periodID", "dayID" = "dayID", "anon_user_id" = "anon_user_id"))
  
  # Impute cases, when the anon_user_id is not NA, location as the new joined column. Deselect unnecessary columns
  df <- df %>%
    mutate(location = case_when(!is.na(anon_user_id) ~ student_loc, is.na(anon_user_id) ~ location)) %>%
    select(-c(anon_user_id, X, Y, student_loc, actual_user_id))
  
# Impute distances between student and teacher and facing the screen features
  
# Code repatriated from AIED position analytics, authored by Conrad Borchers
# https://github.com/conradborchers/position-analytics/blob/main/main.R
# Screen facing vectors, given that desk is top left
SCREEN_FACE <- tribble(
  ~seat_num, ~x_face, ~y_face,
  1, 0, 1, # Up
  2, 0, 1, 
  3, 1, 0, # right
  4, -1, 0, # left
  5, 0, 1,
  6, 0, 1,
  7, 0, 1,
  8, 0, 1, 
  9, 1, 0,
  10, -1, 0,
  11, 0, 1, 
  12, 0, 1,
  13, 1, 0,
  14, -1, 0,
  15, 0, 1,
  16, 0, 1,
  17, 1, 0,
  18, -1, 0,
  19, 0, 1,
  20, 0, 1,
  21, 1, 0,
  22, -1, 0,
  23, 0, 1,
  24, 0, 1,
  25, 0, 1,
  26, 1, 0,
  27, -1, 0,
  28, 0, 1, 
  29, 0, 1
)
  
d_teacher_pos <- read_csv("./datasets/teacher_position_sprint1_shou (1).csv") %>% 
  janitor::clean_names() %>% 
  select(period_id, day_id, time_unix = time_stamp, x = chosen_x, y = chosen_y)

d_student_pos <- read_csv("./datasets/student_position_sprint1_shou.csv") %>% 
  janitor::clean_names() %>% 
  select(period_id, day_id, seat_num, anon_student_id = anon_user_id, x_obj = x, y_obj = y)

d_seats <- d_student_pos %>% 
  distinct(seat_num, x_obj, y_obj) %>% # there are no duplicates, so this can be joined unambiguously by crosswalks
  arrange(seat_num)

euclid <- function(x1, y1, x2, y2) {
  ans <- sqrt((x1-x2)**2 + (y1-y2)**2)
  return(ans)
}

for (i in d_seats$seat_num) {
  xx = d_seats$x_obj[d_seats$seat_num==i]
  yy = d_seats$y_obj[d_seats$seat_num==i]
  d_teacher_pos[paste('seat', i, '_dist', sep='')] <- euclid(d_teacher_pos$x, d_teacher_pos$y, xx, yy)
}

d_teacher_pos['x_traj'] <- d_teacher_pos$x - lag(d_teacher_pos$x, 1)
d_teacher_pos['y_traj'] <- d_teacher_pos$y - lag(d_teacher_pos$y, 1)

# Findings robust when calculating and averaging rolling average with varied k

# Add facing screen alignment
for (i in SCREEN_FACE$seat_num) {
  cat('Processing seat #', i, '\n')
  xx = SCREEN_FACE$x_face[SCREEN_FACE$seat_num==i]
  yy = SCREEN_FACE$y_face[SCREEN_FACE$seat_num==i]
  varname = paste('seat', i, '_screenalign', sep='')
  d_teacher_pos <- d_teacher_pos %>% 
    rowwise() %>% 
    mutate(tmp = as.numeric(lsa::cosine(c(x_traj, y_traj), c(xx, yy))))
  names(d_teacher_pos)[names(d_teacher_pos) == 'tmp'] <- varname 
}

d_teacher_pos['velocity'] <- 
  euclid(d_teacher_pos$x, d_teacher_pos$y, lag(d_teacher_pos$x, 1), lag(d_teacher_pos$y, 1))

# Join new teacher vars to student data
# Step 1: Wide to long
d_teacher_pos_long <- d_teacher_pos %>% 
  pivot_longer(!matches('period_id|day_id|time|x|y|velocity')) %>% 
  separate(name, sep = '_', into = c('num', 'type')) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(seat_num = str_remove(num, 'seat') %>% as.numeric()) %>% 
  select(-num)

# Step 2: Closest timestamp for each student to teacher data timestamp

join_this <- d_teacher_pos_long %>% 
  left_join(d_student_pos %>% select(period_id, day_id, seat_num, anon_student_id), by = c('period_id', 'day_id', 'seat_num')) %>% 
  filter(!is.na(anon_student_id)) %>% 
  filter(!is.na(velocity)) %>% 
  select(anon_student_id, time_unix, dist, velocity, screenalign) %>% 
  distinct(anon_student_id, time_unix, .keep_all = TRUE)

# Write Dataset 
write.csv(df, "./datasets/collaped_AI_classroom_data.csv", row.names = FALSE)

# For simplicity, have the write be above, keep organizing below, and rewrite later 

# Make base rate table 
base_rates <- df[, 11:24] 
base_rates <- as.data.frame(colSums(base_rates)) %>%
  rename(sums = `colSums(base_rates)`)
len_df <- nrow(df)
base_rates$sums <- base_rates$sums / len_df

# Write base rate table
write.csv(base_rates, "./datasets/base_rates_table.csv")

# When defining the spatial-temporal function, use [REDACTED]'s distance-velocity-screenalignment data to use as parameters
# This dataset is called "join_this". Don't worry about the student and teacher location columns.

# If actor or subject is a student, pull it out into a column "anon_student_id"
df <- df %>%
  mutate(anon_student_id = case_when(str_detect(actor, "Stu") ~ actor, str_detect(subject, "Stu") ~ subject))

# Before joining data, split rows in which there are more than one anon_student_id into two rows 
df <- df %>% 
  separate_rows(anon_student_id, sep = "; ")

# Join for start_dist
df <- df %>%
  left_join(join_this, by = c("start" = "time_unix", "anon_student_id"), suffix = c("_start", "_end")) %>%
  select(-velocity) %>%
  rename(start_dist = dist, start_screenalign = screenalign)

# Join for end_dist
df <- df %>%
  left_join(join_this, by = c("end" = "time_unix", "anon_student_id"), suffix = c("_start", "_end")) %>%
  select(-velocity) %>%
  rename(end_dist = dist, end_screenalign = screenalign)
# %>%
  # mutate(student_teacher_distance = (start_dist + end_dist) / 2, teacher_screenalign = (start_screenalign + end_screenalign) / 2) 

# If there is an anon_student_id with NA, find the timestamp of the last time the student appears in join_this, 
# the timestamp of the next time the student appears in join_this, and average them 

# First, get the rows in which this is NA
start_dist_na_rows <- which(!is.na(df$anon_student_id) & is.na(df$start_dist))
end_dist_na_rows <-  which(!is.na(df$anon_student_id) & is.na(df$end_dist))

# Define a function that imputes student_teacher_distance and teacher_screenalign by seraching in join_this for the last and next 
# occurrence of a specific student id 
impute_dist_and_screenalign_rows <- function(impute_df, bad_start_rows, bad_end_rows, joiner_df) {
  for (x in bad_start_rows) {
    row <- impute_df[x, ]
    time_start <- row$start
    student_id <- row$anon_student_id
    
    student_df <- joiner_df[joiner_df$anon_student_id == student_id, ]
    
    init_time_start <- time_start
    # Decrement time_start until a matching timestamp is found or goes below the available range
    while (!(time_start %in% student_df$time_unix) && time_start >= init_time_start - 180) { # Tweak this? 
      time_start <- time_start - 1
    }
    
    # If time has gone below the available range, check if we can go forwards a bit to get the correct time 
    while (!(time_start %in% student_df$time_unix) && time_start <= init_time_start + 180) { # Tweak this? 
      time_start <- time_start + 1
    } 
    
    if (time_start %in% student_df$time_unix) {
      new_start_dist <- student_df$dist[student_df$time_unix == time_start]
      new_start_screenalign <- student_df$screenalign[student_df$time_unix == time_start]
      
      impute_df[x, "start_dist"] <- new_start_dist
      impute_df[x, "start_screenalign"] <- new_start_screenalign
    } else {
      # Handle the case when no matching timestamp is found within the available range
      # You can choose an appropriate action here, such as setting the values to NA or a default value
      impute_df[x, "start_dist"] <- NA
      impute_df[x, "start_screenalign"] <- NA
    }
  }
  
  for (y in bad_end_rows) {
    row <- impute_df[y, ]
    time_end <- row$end
    student_id <- row$anon_student_id
    
    student_df <- joiner_df[joiner_df$anon_student_id == student_id, ]
    
    time_end_init <- time_end
    # Increment time_end until a matching timestamp is found or goes above the available range
    while (!(time_end %in% student_df$time_unix) && time_end <= time_end_init + 180) { # Tweak this? 
      time_end <- time_end + 1
    } 
    
    # If time has gone above the available range, check if we can go backwards a bit to get the correct time 
    while (!(time_end %in% student_df$time_unix) && time_end >= time_end_init - 180) { # Tweak this? 
      time_end <- time_end - 1
    } 
    
    if (time_end %in% student_df$time_unix) {
      new_end_dist <- student_df$dist[student_df$time_unix == time_end]
      new_end_screenalign <- student_df$screenalign[student_df$time_unix == time_end]
      
      impute_df[y, "end_dist"] <- new_end_dist
      impute_df[y, "end_screenalign"] <- new_end_screenalign
    } else {
      # Handle the case when no matching timestamp is found within the available range
      # You can choose an appropriate action here, such as setting the values to NA or a default value
      impute_df[y, "end_dist"] <- NA
      impute_df[y, "end_screenalign"] <- NA
    }
  }
  
  return(impute_df)
}

df <- impute_dist_and_screenalign_rows(df, start_dist_na_rows, end_dist_na_rows, join_this) # Call function to impute bad rows in df with entries from join_this


# If there is no difference between start and end df, and start or end is NA, take the stuff from end and put it into start and take
# the stuff from start and put it into end
df <- df %>%
  mutate(
    start_dist = ifelse(end - start == 0 & !is.na(end_dist) & is.na(start_dist), end_dist, start_dist),
    start_screenalign = ifelse(end - start == 0 & !is.na(end_screenalign) & is.na(start_screenalign), end_screenalign, start_screenalign)
  )

df <- df %>%
  mutate(
    end_dist = ifelse(end - start == 0 & !is.na(start_dist) & is.na(end_dist), start_dist, end_dist),
    end_screenalign = ifelse(end - start == 0 & !is.na(start_screenalign) & is.na(end_screenalign), start_screenalign, end_screenalign)
  )

new_missing_starts <-  which(!is.na(df$anon_student_id) & is.na(df$start_dist))
new_missing_ends <- which(!is.na(df$anon_student_id) & is.na(df$end_dist))
new_missing <- which(!is.na(df$anon_student_id) & (is.na(df$start_screenalign) | is.na(df$end_screenalign)))
# new_missing_starts
# new_missing_ends

# Even after handling all cases, and checking + or - 2 min in join_this, 
# there are ~ 250 rows that simply do not have existing distance and/or screenalignment data. Should I just get rid of them? 

# + or - 1 min -- ~ 360
# + or - 2 min -- ~ 250
# + or - 3 min -- ~ 190
# + or - 4 min -- ~ 190 
# + or - 5 min -- ~ 190

# It looks like after + or - 3 min, we stop improving too much. I would say that we try to impute from start or end, and then remove 
# all these fellows
df <- df %>% 
  mutate(
    start_dist = ifelse(!is.na(end_dist) & is.na(start_dist), end_dist, start_dist),
    start_screenalign = ifelse(!is.na(end_screenalign) & is.na(start_screenalign), end_screenalign, start_screenalign)
  ) %>%
  mutate(
    end_dist = ifelse(!is.na(start_dist) & is.na(end_dist), start_dist, end_dist),
    end_screenalign = ifelse(!is.na(start_screenalign) & is.na(end_screenalign), start_screenalign, end_screenalign)
  )
  
# Filter the rows out that have a non-na student ID but are missing start screenalign and end screenalign
df <- df %>%
  filter(!(!is.na(anon_student_id) & (is.na(start_screenalign) | is.na(end_screenalign))))

# Verify that there are none left 
new_missing <- new_missing <- which(!is.na(df$anon_student_id) & (is.na(df$start_screenalign) | is.na(df$end_screenalign)))
if (length(new_missing) != 0) {
  print(FALSE)
} else {
  print(TRUE)
}

# Now that there are no NA entries for start_dist, end_dist, start_screenalign, or end_screenalign, calculate an average accross 
# the time interval for an event to get the average distance and screenalignment for each student-teacher interaction
df <- df %>%
  mutate(
    student_teacher_distance = (start_dist + end_dist) / 2, 
    teacher_screenalignment = (start_screenalign + end_screenalign) / 2
  ) %>%
  select(-c("anon_student_id", "start_dist", "end_dist", "start_screenalign", "end_screenalign"))

join_this_dist <- join_this %>%
  select(time_unix, anon_student_id, dist) %>%
  pivot_wider(names_from = `anon_student_id`, values_from = `dist`)

# Why is it the case that some students aren't present in the data when others are? We know their position, right? 
df <- df %>%
  select(-c(location, student_teacher_distance))

# Reconstruct distance data to join, imputing student location at all timestamps 

d_teacher_pos <- read_csv("./datasets/teacher_position_sprint1_shou (1).csv") %>% 
  janitor::clean_names() %>% 
  select(period_id, day_id, time_unix = time_stamp, x = chosen_x, y = chosen_y)

d_student_pos <- read_csv("./datasets/student_position_sprint1_shou.csv") %>% 
  janitor::clean_names() %>% 
  select(period_id, day_id, seat_num, anon_student_id = anon_user_id, x_obj = x, y_obj = y)

d_seats <- d_student_pos %>% 
  distinct(seat_num, x_obj, y_obj) %>% # there are no duplicates, so this can be joined unambiguously by crosswalks
  arrange(seat_num)

student_counts <- d_student_pos %>%
  select(anon_student_id) %>%
  unique()

euclid <- function(x1, y1, x2, y2) {
  ans <- sqrt((x1-x2)**2 + (y1-y2)**2)
  return(ans)
}

for (i in d_seats$seat_num) {
  xx = d_seats$x_obj[d_seats$seat_num==i]
  yy = d_seats$y_obj[d_seats$seat_num==i]
  d_teacher_pos[paste('seat', i, '_dist', sep='')] <- euclid(d_teacher_pos$x, d_teacher_pos$y, xx, yy)
}

impute_missing_rows <- function(data) {
  # Sort the dataframe by dayID, periodID, and time
  data <- data[order(data$dayID, data$periodID, data$time), ]
  
  # Create a new dataframe to store the imputed rows
  imputed_data <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(imputed_data) <- colnames(data)
  
  # Iterate over each unique combination of dayID and periodID
  unique_combinations <- unique(data[, c("dayID", "periodID")])
  for (i in 1:nrow(unique_combinations)) {
    combination <- unique_combinations[i, ]
    dayID <- combination$dayID
    periodID <- combination$periodID
    
    # Subset the data for the current dayID and periodID
    subset_data <- data[data$dayID == dayID & data$periodID == periodID, ]
    
    # Iterate over the rows of the subset data to impute missing rows
    for (j in 2:nrow(subset_data)) {
      current_row <- subset_data[j, ]
      previous_row <- subset_data[j - 1, ]
      
      # Calculate the time difference between the current and previous rows
      time_diff <- current_row$time - previous_row$time
      
      # If there is a gap in timestamps, impute the missing rows
      if (time_diff > 1) {
        # Generate a sequence of timestamps for the missing rows
        missing_times <- seq(previous_row$time + 1, current_row$time - 1)
        
        # Create a data frame with the missing rows and NA values
        missing_rows <- data.frame(matrix(ncol = ncol(data), nrow = length(missing_times)))
        colnames(missing_rows) <- colnames(data)
        
        missing_rows$dayID <- dayID
        missing_rows$periodID <- periodID
        missing_rows$time <- missing_times
        
        # Impute the seat distance columns separately
        for (seat_col in grep("^seat", colnames(data))) {
          if (seat_col %in% c("seat24_dist", "seat25_dist", "seat29_dist")) {
            distance_diff <- mean(c(previous_row[[seat_col]], current_row[[seat_col]]), na.rm = TRUE)
            missing_rows[[seat_col]] <- distance_diff
          } else {
            missing_rows[[seat_col]] <- mean(c(previous_row[[seat_col]], current_row[[seat_col]]), na.rm = TRUE)
          }
        }
        
        # Append the missing rows to the imputed data frame
        imputed_data <- rbind(imputed_data, missing_rows)
      }
    }
  }
  
  # Combine the original data and the imputed data
  imputed_data <- rbind(data, imputed_data)
  
  # Sort the combined data frame by dayID, periodID, and time
  imputed_data <- imputed_data[order(imputed_data$dayID, imputed_data$periodID, imputed_data$time), ]
  
  # Reset the row names
  rownames(imputed_data) <- NULL
  
  return(imputed_data)
}


d_teacher_pos <- d_teacher_pos %>% 
  select(-c(x,y)) %>%
  mutate(dayID = day_id, periodID = period_id, time = time_unix) %>%
  select(-c(period_id, day_id, time_unix)) %>%
  relocate(dayID, periodID, time) %>%
  distinct(dayID, periodID, time, .keep_all = TRUE)

d_teacher_pos <- impute_missing_rows(d_teacher_pos) 

# Nice! This looks more continuous timestamp wise. Now, we have to figure out which students are involved in which period/day
d_teacher_pos_long <- d_teacher_pos %>%
  pivot_longer(!matches('periodID|dayID|time')) %>%
  separate(name, sep = '_', into = c('num', 'type')) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(seat_num = str_remove(num, 'seat') %>% as.numeric()) %>%
  select(-num)

d_student_pos <- d_student_pos %>%
  select(-c(x_obj, y_obj)) %>%
  rename(dayID = day_id, periodID = period_id)

to_join <- left_join(d_teacher_pos_long, d_student_pos, by = c("dayID", "periodID", "seat_num")) %>%
  select(-(seat_num)) 

duplicates <- to_join %>%
  dplyr::group_by(periodID, dayID, time, anon_student_id) %>%
  dplyr::summarize(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

to_join <- to_join %>%
  anti_join(duplicates, by = c("periodID", "dayID", "time", "anon_student_id"))

to_join <- to_join %>%
  pivot_wider(names_from = anon_student_id, values_from = dist) 

# This dataset should be the teacher distances from every single student at every single timestamp in the dataset
write.csv(to_join, "./datasets/distance_matrix_complete.csv", row.names = FALSE)

# Before proceeding, a) separate rows that have a "; " dividing multiple subjects into two b) extract the time intervals in which the 
# the teacher is the actor into a dataframe
df <- df %>%
  mutate(subject = strsplit(subject, "; ")) %>%
  unnest(subject)

intervals <- df %>%
  filter(actor == "teacher") %>%
  select(dayID, periodID, start, end) %>%
  distinct(dayID, periodID, start, end)

# Join the students for these intervals 

intervals <- intervals %>%
  left_join(to_join, by = c("dayID", "periodID", "start" = "time")) %>%
  left_join(to_join, by = c("dayID", "periodID", "end" = "time")) 

students_list <- colnames(to_join)[4:length(colnames(to_join))]


for (student in students_list) {
  intervals <- intervals %>%
    mutate(!!student := ifelse(!is.na(!!sym(paste0(student, ".y"))), !!sym(paste0(student, ".x")),
                               mean(c(!!sym(paste0(student, ".x")), !!sym(paste0(student, ".y"))))))
}

intervals <- intervals %>%
  select(-matches("\\.x$|\\.y$")) 


# Join the interval dataset to the original df 
df <- df %>%
  left_join(intervals, by = c("dayID", "periodID", "start","end"))

# Verify that there are no rows in which the teacher acts and no student locations are recorded
students_list <- colnames(df)[25:length(colnames(df))]  # Assuming the column indices for students start from 25

missing_indices <- which(df$actor == "teacher" & rowSums(is.na(df[, students_list])) == length(students_list))

if (length(missing_indices) > 0) {
  print("There are missing indices where all student columns are NA and the actor is a teacher.")
} else {
  print("No missing indices found where all student columns are NA and the actor is a teacher.")
}

# Woohoo! Note: Don't need to worry about the missing indices
  
# Write the dataframe as a CSV 

write.csv(df, "./datasets/collapsed_AI_classroom_data.csv", row.names = FALSE)

