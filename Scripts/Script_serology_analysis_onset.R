##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Analysis data Serology - Onset
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################
# Niet mogelijk denk te weinig binnen/tussen groepen en te kleine sample size. 

# set.seed(12345)
# 
# data_onset <- data_serology %>%
#   filter(!is.na(result_num)) %>%
#   group_by(group_code, lamb_id) %>%
#   filter(day == min(day)) %>%
#   ungroup %>%
#   select(group_code, lamb_id, time = day) %>%
#   bind_rows(data_serology %>% filter(group_code == "C") %>% distinct(group_code, lamb_id)) %>%
#   mutate(status = if_else(group_code == "C", 0, 1),
#          # Replace Inf with the maximum day if no response was observed
#          time = if_else(is.na(time), 28, time), 
#          group_code = factor(group_code)) %>%
#   rename(group = group_code) %>%
#   # Filter data to include only events (status == 1)
#   filter(status == 1)
# 
# # Perform Kruskal-Wallis test
# kruskal_test <- kruskal_test(time ~ group, data = data_onset, distribution = approximate(nresample = 10000))
# 
# # Print Kruskal-Wallis test results
# print(kruskal_test)
# 
# # Perform post-hoc pairwise comparisons using Wilcoxon rank-sum tests with the coin package
# pairwise_comparisons <- function(data, group1, group2) {
#   subset_data <- data %>% filter(group %in% c(group1, group2))
#   test_result <- wilcox_test(time ~ group, data = subset_data, distribution = approximate(nresample = 10000))
#   p_value <- pvalue(test_result)
#   return(p_value)
# }
# 
# # Get unique groups
# groups <- unique(data_onset$group)
# 
# # Initialize matrix to store pairwise p-values
# pairwise_results <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
# 
# # Perform pairwise comparisons
# for (i in 1:(length(groups) - 1)) {
#   for (j in (i + 1):length(groups)) {
#     p_value <- pairwise_comparisons(data_onset, groups[i], groups[j])
#     pairwise_results[i, j] <- p_value
#     pairwise_results[j, i] <- p_value
#   }
# }
# 
# # Adjust p-values for multiple testing using Bonferroni correction
# adjusted_p_values <- p.adjust(pairwise_results[lower.tri(pairwise_results)], method = "bonferroni")
# pairwise_results[lower.tri(pairwise_results)] <- adjusted_p_values
# pairwise_results[upper.tri(pairwise_results)] <- adjusted_p_values
# 
# # Print pairwise comparison results
# print(pairwise_results)

#------------------ Create a dataset with the time to onset and event indicator
# data_onset <- data_serology %>%
#   filter(!is.na(result_num)) %>% 
#   group_by(group_code, lamb_id) %>%
#   filter(day == min(day)) %>% 
#   ungroup %>% 
#   select(group_code, lamb_id, time = day) %>% 
#   bind_rows(data_serology %>% filter(group_code == "C") %>% distinct(group_code, lamb_id)) %>% 
#   mutate(status = if_else(group_code == "C", 0, 1),
#          # Replace Inf with the maximum day if no response was observed
#          time = if_else(is.na(time), 28, time)) %>% 
#   rename(group = group_code)
# 
# #------------------ Create a survival object
# surv_obj <- Surv(data_onset$time, data_onset$status)
# 
# #------------------ Fit a survival curve
# fit <- survfit(surv_obj ~ data_onset$group)
# 
# #------------------ Calculate the log-rank test statistic
# log_rank_test <- survdiff(surv_obj ~ data_onset$group)
# observed_stat <- log_rank_test$chisq
# 
# #------------------ Permutation test
# n_permutations <- 1000
# permuted_stats <- numeric(n_permutations)
# 
# for (i in 1:n_permutations) {
#   permuted_group <- sample(data_onset$group)
#   permuted_test <- survdiff(surv_obj ~ permuted_group)
#   permuted_stats[i] <- permuted_test$chisq
# }
# 
# #------------------ Calculate p-value
# p_value <- mean(permuted_stats >= observed_stat)
# 
# #------------------ Print overall test results
# cat("Observed Statistic:", observed_stat, "\n")
# cat("P-value:", p_value, "\n")
# 
# # Post-hoc pairwise comparisons using permutation tests
# pairwise_comparisons <- function(group1, group2) {
#   subset_indices <- which(data_onset$group %in% c(group1, group2))
#   subset_surv_obj <- surv_obj[subset_indices]
#   subset_group <- data_onset$group[subset_indices]
#   
#   # Check if there are enough data points for both groups
#   if (length(unique(subset_group)) < 2) {
#     return(NA)
#   }
#   
#   observed_stat <- survdiff(subset_surv_obj ~ subset_group)$chisq
#   permuted_stats <- numeric(n_permutations)
#   
#   for (i in 1:n_permutations) {
#     permuted_group <- sample(subset_group)
#     permuted_test <- survdiff(subset_surv_obj ~ permuted_group)
#     permuted_stats[i] <- permuted_test$chisq
#   }
#   
#   p_value <- mean(permuted_stats >= observed_stat)
#   return(p_value)
# }
# 
# # Perform pairwise comparisons
# groups <- unique(data_onset$group)
# pairwise_results <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
# 
# for (i in 1:(length(groups)-1)) {
#   for (j in (i+1):length(groups)) {
#     p_value <- pairwise_comparisons(groups[i], groups[j])
#     pairwise_results[i, j] <- p_value
#     pairwise_results[j, i] <- p_value
#   }
# }
# 
# # Adjust p-values for multiple testing using Bonferroni correction
# adjusted_p_values <- p.adjust(pairwise_results[lower.tri(pairwise_results)], method="bonferroni")
# pairwise_results[lower.tri(pairwise_results)] <- adjusted_p_values
# pairwise_results[upper.tri(pairwise_results)] <- adjusted_p_values
# 
# # Print pairwise comparison results
# print(pairwise_results)



