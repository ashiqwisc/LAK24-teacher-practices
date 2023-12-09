library(tidyverse)

d <- read_csv('./datasets/final-sample-cb-lak24-extended.csv')

d_qual <- read_delim('./datasets/observation_events_anonymized.tsv', delim='\t')

d <- d %>% 
  left_join(d_qual %>% select(timestamp, note) %>% mutate(timestamp = round(timestamp)), by = c('start'='timestamp'))

hlr_sids <- d %>% 
  filter(high_learning_rate == 1) %>% 
  pull(actor) %>% 
  unique()

llr_sids <- d %>% 
  filter(high_learning_rate == 0) %>% 
  pull(actor) %>% 
  unique()

# Screen alignment replay, high LR
tmp <- d %>% 
  filter(!(actor %in% llr_sids)) %>% 
  select(actor, start, event, content, note, Talking, screenalignment_binary, Correct_attempt, first_correct_attempt, hint_request) %>% 
  mutate(idx = row_number())

idx <- tmp$idx[(tmp$screenalignment_binary==1) | (tmp$Talking==1)]
idx <- idx[!is.na(idx)]
set.seed(421)
sampled_idx <- sample(idx, 25, replace=FALSE) %>% sort()

# Replay, teacher context only
alls <- data.frame()
i<-1
for (idxx in sampled_idx) {
  tmp2 <- tmp %>%  filter(actor=='teacher' | idx==idxx) %>% mutate(idx_teacher=row_number())
  new_idx <- tmp2$idx_teacher[tmp2$idx==idxx]
  start_idx <- max(1, new_idx - 3)
  end_idx <- min(nrow(new_idx), new_idx + 3)
  ref <- data.frame(tmp2[start_idx:end_idx, ]) %>% select(event, note)
  print(ref)
  ref['example'] <- i
  i <- i+1
  alls <- rbind(alls, ref) # OK because small data
}

write_csv(alls, './datasets/high-learning-rate-replay.csv')

# Screen alignment replay, low LR
tmp <- d %>% 
  filter(!(actor %in% hlr_sids)) %>% 
  select(actor, start, event, content, note, Talking, screenalignment_binary, Correct_attempt, first_correct_attempt, hint_request) %>% 
  mutate(idx = row_number())

idx <- tmp$idx[(tmp$screenalignment_binary==1) | (tmp$Talking==1)]
idx <- idx[!is.na(idx)]
set.seed(421)
sampled_idx <- sample(idx, 25, replace=FALSE) %>% sort()

# Replay, teacher context only
alls <- data.frame()
i<-1
for (idxx in sampled_idx) {
  tmp2 <- tmp %>%  filter(actor=='teacher' | idx==idxx) %>% mutate(idx_teacher=row_number())
  new_idx <- tmp2$idx_teacher[tmp2$idx==idxx]
  start_idx <- max(1, new_idx - 3)
  end_idx <- min(nrow(new_idx), new_idx + 3)
  ref <- data.frame(tmp2[start_idx:end_idx, ]) %>% select(event, note)
  print(ref)
  ref['example'] <- i
  i <- i+1
  alls <- rbind(alls, ref) # OK because small data
}

write_csv(alls, './datasets/low-learning-rate-replay.csv')