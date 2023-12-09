library(tidyverse)

diff_last_visit <- function(a, b) {
  ##
  # We want to return the difference to the time when the teacher last visited
  # Reference should be end of timestamp of visit timeframe
  ##
  pos_b <- b[(a-b)>=0]
  if (length(pos_b)==0)
    return(NA)
  closest_b <- pos_b[which.min(a-pos_b)]
  return(a-closest_b)
}

diff_next_visit <- function(a, b) {
  ##
  # We want to return the difference to the time when the teacher will next visit
  # Reference should be start of timestamp of visit timeframe
  ##
  neg_b <- b[(a-b)<=0]
  if (length(neg_b)==0)
    return(NA)
  next_b <- neg_b[which.max(a-neg_b)]
  return(a-next_b)
}

diff_clostest_visit <- function(a, b) {
  ##
  # We want to return the difference to the next closest visit, independent of whether it 
  # is in the future or was in the past
  # Reference should be time frame center of timestamp of visit timeframe
  ##
  if (length(b)==0)
    return(NA)
  best_b <- b[which.min(abs(a-b))]
  return(a-best_b)
}

d <- read_csv('./datasets/final-sample-cb-lak24.csv')
d['index']<-1:nrow(d)
d['center']<-d$start+((d$end-d$start)/2)

d2 <- d %>% 
  arrange(subject, center)

join_this <- d2 %>% 
  filter(str_detect(subject, 'Stu_')) %>% 
  filter(str_detect(content, '^Stopping')) %>% 
  group_by(subject) %>% 
  summarize(centers=list(center), starts=list(start), ends=list(end))

d3 <- d2 %>% 
  left_join(join_this, by=c('actor'='subject'))

d4 <- d3 %>% 
  mutate(
    time_diff_to_last_visit = map2_dbl(center, ends, diff_last_visit),
    time_diff_to_next_visit = map2_dbl(center, starts, diff_next_visit),
    time_diff_to_closest_visit = map2_dbl(center, centers, diff_clostest_visit)
  ) %>% 
  mutate( # Flag visit differences stretching across single periods
    time_diff_to_last_visit = ifelse(abs(time_diff_to_last_visit)>30*60, NA, time_diff_to_last_visit),
    time_diff_to_next_visit = ifelse(abs(time_diff_to_next_visit)>30*60, NA, time_diff_to_next_visit),
    time_diff_to_closest_visit = ifelse(abs(time_diff_to_closest_visit)>30*60, NA, time_diff_to_closest_visit)
  ) %>% 
  mutate(visit_class = case_when(
    abs(time_diff_to_next_visit) > abs(time_diff_to_last_visit) ~ 1, # after
    abs(time_diff_to_next_visit) <= abs(time_diff_to_last_visit) ~  -1, # before
    str_detect(actor, 'Stu_') ~ 0, # never visited
    TRUE ~ NA
  )) %>% 
  select(-time_diff_to_last_visit, time_diff_to_next_visit, time_diff_to_closest_visit)

d5 <- d4 %>% 
  group_by(actor) %>%
  mutate(had_visit = ifelse(cumsum(visit_class == 1)>=1, 1, 0)) %>% 
  mutate(visit_or_not = had_visit) %>%
  mutate(before_or_after = case_when(visit_class == 1 ~ 1, visit_class == -1 ~ 0, visit_class == 0 ~ NA)) %>%
  arrange(index)   

write_csv(d5, './datasets/final-sample-cb-lak24-extended.csv')
