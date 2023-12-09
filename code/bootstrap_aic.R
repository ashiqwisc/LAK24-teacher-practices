bootstrap_aic <- function(set_1, set_2, sample.size){
  set_1_aic_list <- c()
  set_2_aic_list <- c()
  for(i in c(1:sample.size)){
    # browser()
    sampled_index <- sample(c(1:nrow(set_1$points)),nrow(set_1$points), replace = TRUE)

    set_1_spat_feat <- as.data.frame(set_1$points) %>%
      filter(high_learning_rate != -1) 
    set_1.resampled.points <- set_1_spat_feat[sampled_index, ] %>% as.data.frame()
    set_1_logit_spat_feat <- glm(set_1.resampled.points$high_learning_rate ~ set_1.resampled.points$MR1, family = "binomial")
    
    
    set_2_spat_feat <- as.data.frame(set_2$points) %>%
      filter(high_learning_rate != -1) 
    set_2.resampled.points <- set_2_spat_feat[sampled_index, ] %>% as.data.frame()
    set_2_logit_spat_feat <- glm(set_2.resampled.points$high_learning_rate ~ set_2.resampled.points$MR1, family = "binomial")
    
    
    set_1_aic_list <- c(set_1_aic_list, set_1_logit_spat_feat$aic)
    set_2_aic_list <- c(set_2_aic_list, set_2_logit_spat_feat$aic)
  }
  
  # wilcox_test <- wilcox.test(set_1_aic_list, set_2_aic_list,
  #                                paired = TRUE,
  #                                alternative = "two.sided")
  # wilcox_test.results <- paste0("t(",wilcox_test$parameter,") = ",round(wilcox_test$statistic,3),", p = ",round(wilcox_test$p.value, 3))
  return(t.test(set_1_aic_list %>% unlist(), set_2_aic_list %>% unlist()))
}