library(tidyverse)
library(lme4)
library(lubridate)

d <- read_csv("./datasets/collapsed_AI_classroom_data.csv")

# Learning gain
d_learn <- read_csv('./datasets/meta-data-aied.csv') %>% 
  janitor::clean_names() %>% 
  mutate(
    ck_gain = (ck - ck_pre) / (16 - ck_pre),
    pk_gain = (pk - pk_pre) / (5 - pk_pre),
    ck_100 = ck/16, ck_pre_100 = ck_pre/16,
    pk_100 = pk/5, pk_pre_100 = pk_pre/5,
    ck_gain_c = ifelse(ck>ck_pre, (ck-ck_pre)/(16-ck_pre), ifelse(ck<ck_pre, (ck-ck_pre)/ck_pre, 0)),
    pk_gain_c = ifelse(pk>pk_pre, (pk-pk_pre)/(5-pk_pre), ifelse(pk<pk_pre, (pk-pk_pre)/pk_pre, 0))
  ) %>% 
  mutate(
    pk_gain = ifelse(is.infinite(pk_gain), 0, pk_gain),
    pk_gain_c = ifelse(is.infinite(pk_gain_c), 0, pk_gain_c),
    ck_gain = ifelse(is.infinite(ck_gain), 0, ck_gain),
    ck_gain_c = ifelse(is.infinite(ck_gain_c), 0, ck_gain_c)
  ) %>% 
  mutate(
    high_ck_pre = ifelse(ck_pre > median(ck_pre, na.rm=TRUE), 1, 0),
    high_pk_pre = ifelse(pk_pre > median(pk_pre, na.rm=TRUE), 1, 0),
    high_pk_gain = ifelse(pk_gain > median(pk_gain, na.rm=TRUE), 1, 0),
    high_ck_gain = ifelse(ck_gain > median(ck_gain, na.rm=TRUE), 1, 0),
    high_pk_gain_c = ifelse(pk_gain_c  > median(pk_gain_c, na.rm=TRUE), 1, 0),
    high_ck_gain_c = ifelse(ck_gain_c > median(ck_gain_c, na.rm=TRUE), 1, 0)
  )

ref <- read_csv('./datasets/students.csv') %>% 
  select(students, animal_id)

join_this <- d_learn %>% 
  select(anon_student_id, pk_100, ck_100, pk_gain, ck_gain, pk_gain_c, ck_gain_c, matches('high_')) %>% 
  left_join(ref, by = c('anon_student_id'='animal_id')) %>% 
  filter(!is.na(students)) %>% 
  select(-anon_student_id) %>% 
  select(subject=students, everything())

# Learning rates

d <- read_delim('./datasets/tutor_log_anonymized.tsv', delim ='\t') %>% 
  janitor::clean_names() %>% 
  arrange(anon_student_id, problem_name, step_name, desc(time)) %>% 
  distinct(anon_student_id, problem_name, step_name, .keep_all = TRUE) %>% 
  mutate(outcome_bin = case_when(
    outcome == "CORRECT" ~ 1,
    outcome == "INCORRECT" ~ 0,
    outcome == "HINT" ~ 0
  )) %>% 
  filter(!is.na(outcome_bin)) %>% 
  arrange(anon_student_id, time)

combine_kc_default <- function(row) {
  kc_columns <- grep("^kc_default", names(row), value = TRUE)
  kc_values <- row[kc_columns]
  kc_values <- kc_values[!is.na(kc_values)] %>% as.character()
  return(kc_values)
}

d_afm <- d %>% 
  filter(attempt_at_step == 1) %>% 
  arrange(anon_student_id, problem_name, step_name, desc(time)) %>% 
  distinct(anon_student_id, problem_name, step_name, .keep_all = TRUE) %>% 
  mutate(outcome_bin = case_when(
    outcome == "CORRECT" ~ 1,
    outcome == "INCORRECT" ~ 0,
    outcome == "HINT" ~ 0
  )) %>% 
  filter(!is.na(outcome_bin)) %>% 
  mutate(kcs = apply(., 1, combine_kc_default)) %>% 
  mutate(kc_length = kcs %>% map_int(length)) %>% 
  filter(kc_length > 0) %>% 
  select(anon_student_id, kcs, time, outcome_bin) %>% 
  unchop(kcs) %>% 
  group_by(anon_student_id, kcs) %>% 
  arrange(time) %>% 
  reframe(time = time, n_opportunity = 1:n(), outcome_bin = outcome_bin) %>% 
  select(-time)

m_tutor <- glmer(outcome_bin ~ kcs*n_opportunity + (1 + n_opportunity | anon_student_id), 
                 d_afm, family = 'binomial', nAGQ=0, verbose=2)

#saveRDS(m_tutor, 'm_tutor.rds')

join_this2 <- ranef(m_tutor)$anon_student_id %>% 
  rownames_to_column('anon_student_id') %>% 
  janitor::clean_names() %>% 
  rename(learning_rate = n_opportunity) %>% 
  tibble() %>% 
  mutate(
    high_learning_rate = ifelse(learning_rate > median(learning_rate, na.rm=TRUE), 1, 0),
    high_intercept = ifelse(intercept > median(intercept, na.rm=TRUE), 1, 0)
  )

# df_out_final <- df_out %>% 
#   left_join(join_this2, by = c('subject' = 'anon_student_id'))

# write_csv(df_out_final, './datasets/final-sample-cb-lak24.csv')

#  Add separate sample for joining

out2 <- join_this %>% 
  left_join(join_this2, by=c('subject'='anon_student_id')) %>%
  rename(anon_student_id = subject)

# write_csv(out2, './datasets/join-sample-cb-lak24.csv')

df <- read_csv("./datasets/collapsed_AI_classroom_data.csv")

df <- df %>%
  mutate(anon_student_id = case_when(str_detect(actor, "Stu") ~ actor)) %>%
  left_join(out2, by = 'anon_student_id') %>%
  select(-anon_student_id) %>% 
  mutate(
    screenalignment_binary = ifelse(teacher_screenalignment >= 0.5, 1, 0)
  ) 

# Verify that no teachers are included 
teachers <- which(df$actor == "teacher" & !is.na(df$intercept))
# Looks empty, good 

# Before writing, add two new columns: hint_request and first_correct_attempt
# hint_request: 1 if hint request occurs, 0 otherwise
# first_correct_attempt: 1 if student solves problem on their first try, 0 otherwise 
df <- df %>%
  mutate(hint_request = ifelse(event == "Hint request", 1, 0)) 

# Distinct combinations of actor and periodID
actor_period_combs <- df %>%
  select(start, actor, periodID, dayID) %>%
  filter(actor != "teacher") %>%
  mutate(day = as.POSIXct(start, origin = "1970-01-01")) %>%
  mutate(day = as.numeric(format(day, "%d")))
  
# Note: dayID 1 is 23rd, dayID 2 is 24th, dayID 3 is 25th, as discovered above
# Note: class 1 maps to periodID 1. class 3 maps to periodID 2. class 4 maps to periodID 3. class 5 maps to periodID 4. class 7 maps to
# periodID 5, as discovered above

# Read in tutor log dataset
tutor_log <- read_delim('./datasets/tutor_log_anonymized.tsv', delim ='\t') %>%
  janitor:::clean_names() %>%
  filter(outcome %in% c('CORRECT', 'INCORRECT', 'HINT')) %>%
  mutate(opportunity_count = ifelse(student_response_type == "ATTEMPT" & attempt_at_step == 1 & is_last_attempt == 1 & outcome == "CORRECT", 1, 0)) %>%
  mutate(first_correct_attempt = ifelse(attempt_at_step == 1 & outcome == "CORRECT", 1, 0)) %>%
  filter(first_correct_attempt == 1) %>%
  rename(start = time) %>%
  mutate(start = round(as.numeric(ymd_hms(start)))) %>%
  mutate(end = start) %>% # only correct attempts
  mutate(class = as.numeric(gsub("Period", "", class))) %>%
  rename(periodID = class) %>%
  select(anon_student_id, session_id, periodID, start, end, first_correct_attempt) %>%
  mutate(day = as.POSIXct(start, origin = "1970-01-01")) %>%
  mutate(day = as.numeric(format(day, "%d"))) %>%
  mutate(dayID = case_when(day == 23 ~ 1, day == 24 ~ 2, day == 25 ~ 3)) %>%
  filter(!is.na(dayID)) %>%
  select(-c(day)) %>%
  mutate(periodID = case_when(periodID == 1 ~ 1, periodID == 3 ~ 2, periodID == 4 ~ 3, periodID == 5 ~ 4, periodID == 7 ~ 5)) %>%
  arrange(dayID, periodID, start, end) %>%
  select(-session_id) %>%
  distinct()

# number of correct attempts in tutor_log.tsv for days 23rd, 24th, 25th
correct_attempts_tutor <- read_delim('./datasets/tutor_log_anonymized.tsv', delim ='\t') %>%
  janitor:::clean_names() %>%
  filter(outcome %in% c('CORRECT') & student_response_type == "ATTEMPT") %>%
  rename(start = time) %>%
  mutate(start = round(as.numeric(ymd_hms(start)))) %>%
  mutate(day = as.POSIXct(start, origin = "1970-01-01")) %>%
  mutate(day = as.numeric(format(day, "%d"))) %>%
  mutate(dayID = case_when(day == 23 ~ 1, day == 24 ~ 2, day == 25 ~ 3)) %>%
  filter(!is.na(dayID))

# number of correct attempts in df
correct_attempts_df <- df %>%
  filter(event == "Correct attempt")

  
df <- df %>%
  left_join(tutor_log, by = c("dayID", "periodID", "start", "end", "actor" = "anon_student_id")) %>%
  mutate(first_correct_attempt = ifelse(is.na(first_correct_attempt), 0, 1))
  
# woohoo! 

write_csv(df, './datasets/final-sample-cb-lak24.csv')





