##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Analysis data Serology
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

#------------------ Permutation 
# In a permutation test, you shuffle the labels of your groups (or time points) many times and calculate 
# the test statistic (e.g., difference in means between groups) for each permutation. This creates a null 
# distribution, and the position of the observed test statistic in this distribution allows you to compute a p-value.

# Exact Test: Permutation tests do not rely on assumptions about the distribution of the data, making them exact tests.
# Small Sample Sizes: They are particularly useful for small sample sizes where traditional parametric tests may not be appropriate.
# Flexibility: They can be used to test a wide range of hypotheses.

# calculate the difference in the immune response between two time points (T2 and T1) for each group and 
# then compares these differences between groups.

#------------------------------------------------------------------------------------------------------------# 
# Dag 21 
#------------------------------------------------------------------------------------------------------------# 
#------------------ Data prep
data_serology_analysis_t21 <- data_serology %>%
  filter(!is.na(time_point)) %>% 
  mutate(score = factor(score)) %>% 
  filter(day == 21)
# data_serology_analysis$score
# [1] 0 0 3 1 3 4 4 4 0 0 5 5 3 5 3 3 0 0 5 5 4 4 2 4 4 5 5 4 5 5 0 0
# Levels: 0 1 2 3 4 5

#------------------ Function to perform permutation test between two groups using Fisher's Exact Test
permutation_test <- function(data, group1, group2, n_permutations = 1000) {
  # Subset data for the two groups
  subset_data <- data %>% filter(group_code %in% c(group1, group2))
 
  # Observed test statistic: Fisher's Exact Test
  observed_stat <- fisher.test(table(subset_data$score, subset_data$group_code))$p.value
  
  # Permutation test
  perm_stats <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    permuted_outcome <- sample(subset_data$score)
    perm_stats[i] <- fisher.test(table(permuted_outcome, subset_data$group_code))$p.value
  }
  
  # Calculate p-value
  p_value <- mean(perm_stats <= observed_stat)
  
  return(p_value)
}

#------------------ Get all pairwise combinations of groups
group_combinations <- combn(unique(data_serology_analysis_t21$group_code), 2, simplify = FALSE)

#------------------ Perform permutation tests for all pairwise combinations
results_21 <- map_dfr(group_combinations, ~{
  group1 <- .x[1]
  group2 <- .x[2]
  p_value <- permutation_test(data=data_serology_analysis_t21, group1, group2)
  tibble(group1 = group1, group2 = group2, p_value = p_value)
})

#------------------ Print results
print(results_21)

#------------------------------------------------------------------------------------------------------------# 
# Dag 28 
#------------------------------------------------------------------------------------------------------------# 

#------------------ Data prep
data_serology_analysis_t28 <- data_serology %>%
  filter(!is.na(time_point)) %>% 
  mutate(score = factor(score)) %>% 
  filter(day == 28)

#------------------ Get all pairwise combinations of groups
group_combinations <- combn(unique(data_serology_analysis_t28$group_code), 2, simplify = FALSE)

#------------------ Perform permutation tests for all pairwise combinations
results_28 <- map_dfr(group_combinations, ~{
  group1 <- .x[1]
  group2 <- .x[2]
  p_value <- permutation_test(data=data_serology_analysis_t28, group1, group2)
  tibble(group1 = group1, group2 = group2, p_value = p_value)
})

#------------------ Print results
print(results_28)
























# data_serology_analysis <- data_serology %>%
#   filter(!is.na(time_point)) #%>% 
#   #filter(group_code != "C")
# 
# #------------------ Function to calculate the difference in immune response between groups
# calc_group_diff <- function(data) {
#   data %>%
#     group_by(group_code, time_point) %>%
#     reframe(mean_score = mean(score), .groups = 'drop') %>%
#     spread(time_point, mean_score) %>%
#     mutate(diff = T2 - T1)
# }
# 
# #------------------ Observed differences
# observed_diffs <- calc_group_diff(data_serology_analysis)
# 
# #------------------ Function to calculate pairwise differences
# pairwise_diff <- function(diffs) {
#   combn(unique(diffs$group_code), 2, function(groups) {
#     group1 <- diffs %>% filter(group_code == groups[1]) %>% pull(diff)
#     group2 <- diffs %>% filter(group_code == groups[2]) %>% pull(diff)
#     diff <- group1 - group2
#     tibble(group1 = groups[1], group2 = groups[2], diff = diff)
#   }, simplify = FALSE) %>%
#     bind_rows()
# }
# 
# #------------------ Observed pairwise differences
# observed_pairwise_diffs <- pairwise_diff(observed_diffs)
# 
# #------------------ Permutation test
# set.seed(123) # for reproducibility 
# 
# n_permutations <- 1000
# 
# perm_pairwise_diffs <- replicate(n_permutations, {
#   permuted_data <- data_serology_analysis %>%
#     mutate(score = sample(score, replace = FALSE)) 
#   perm_diffs <- calc_group_diff(permuted_data)
#   pairwise_diff(perm_diffs)
# }, simplify = FALSE) %>%
#   bind_rows(.id = "permutation")
# 
# #------------------ Calculate p-values for each pairwise comparison
# p_values <- observed_pairwise_diffs %>%
#   rowwise() %>%
#   mutate(p_value = mean(abs(perm_pairwise_diffs %>%
#                               filter(group1 == group1 & group2 == group2) %>%
#                               pull(diff)) >= abs(diff)))
# 
# #------------------ Print results
# print(observed_pairwise_diffs)
# print(p_values)
# 
# #------------------ Visualize the permutation distribution for each pairwise comparison
# perm_pairwise_diffs %>%
#   ggplot(aes(x = diff)) +
#   geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
#   facet_wrap(~ interaction(group1, group2), scales = "free") +
#   geom_vline(data = observed_pairwise_diffs, aes(xintercept = diff), color = "red", linetype = "dashed") +
#   labs(title = "Permutation Test for Pairwise Differences in Immune Response",
#        x = "Difference in Immune Response",
#        y = "Frequency")


###################################################################





