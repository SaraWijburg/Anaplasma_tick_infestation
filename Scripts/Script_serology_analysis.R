##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Analysis data Serology - Severity
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################
set.seed(12345)
#------------------ Permutation 
# In a permutation test, you shuffle the labels of your groups (or time points) many times and calculate 
# the test statistic (e.g., difference in means between groups) for each permutation. This creates a null 
# distribution, and the position of the observed test statistic in this distribution allows you to compute a p-value.

# Exact Test: Permutation tests do not rely on assumptions about the distribution of the data, making them exact tests.
# Small Sample Sizes: They are particularly useful for small sample sizes where traditional parametric tests may not be appropriate.
# Flexibility: They can be used to test a wide range of hypotheses.

# calculate the difference in the immune response between two time points (T2 and T1) for each group and 
# then compares these differences between groups.

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


#------------------------------------------------------------------------------------------------------------# 
# Dag 14 
#------------------------------------------------------------------------------------------------------------# 
#------------------ Data prep
data_serology_analysis_t14 <- data_serology %>%
  mutate(score = factor(score)) %>% 
  filter(day == 14)

#------------------ Get all pairwise combinations of groups
group_combinations <- combn(unique(data_serology_analysis_t14$group_code), 2, simplify = FALSE)

#------------------ Perform permutation tests for all pairwise combinations
results_14 <- map_dfr(group_combinations, ~{
  group1 <- .x[1]
  group2 <- .x[2]
  p_value <- permutation_test(data=data_serology_analysis_t14, group1, group2)
  tibble(group1 = group1, group2 = group2, p_value = p_value)
})

results_14 <- results_14 %>%
  mutate(p_adjusted = p.adjust(p_value, method = "bonferroni"))

#------------------ Print results
print(results_14)

#------------------------------------------------------------------------------------------------------------# 
# Dag 21 
#------------------------------------------------------------------------------------------------------------# 
#------------------ Data prep
data_serology_analysis_t21 <- data_serology %>%
  mutate(score = factor(score)) %>% 
  filter(day == 21)

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
results_21 <- results_21 %>%
  mutate(p_adjusted = p.adjust(p_value, method = "bonferroni"))

print(results_21)

#------------------------------------------------------------------------------------------------------------# 
# Dag 28 
#------------------------------------------------------------------------------------------------------------# 

#------------------ Data prep
data_serology_analysis_t28 <- data_serology %>%
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
results_28 <- results_28 %>%
  mutate(p_adjusted = p.adjust(p_value, method = "bonferroni"))
print(results_28)


#------------------ Visualize
ggarrange(
  
  data_serology_analysis_t21 %>% 
  group_by(group_code, score) %>% 
  count %>% ungroup %>% 
  ggplot(mapping = aes(x = group_code, y = n, fill = score)) +
  geom_bar(stat = "identity", position = "fill", colour = "grey20", size = 0.5, width = 1.0) +
  labs(title = "Distribution of Scores by Group on Day 21",
       x = "",
       y = "Proportion",
       fill = "Score") +
  theme_minimal() + scale_fill_brewer(palette="Set1"),
  
  data_serology_analysis_t28 %>% 
    group_by(group_code, score) %>% 
    count %>% ungroup %>% 
    ggplot(mapping = aes(x = group_code, y = n, fill = score)) +
    geom_bar(stat = "identity", position = "fill", colour = "grey20", size = 0.5, width = 1.0) +
    labs(title = "Distribution of Scores by Group on Day 28",
         x = "",
         y = "Proportion",
         fill = "Score") +
    theme_minimal() + scale_fill_brewer(palette="Set1"), 
  
  common.legend = T
  
)

#------------------------------------------------------------------------------------------------------------# 
# Clean 
#------------------------------------------------------------------------------------------------------------# 
rm(group_combinations, observed_diffs, observed_pairwise_diffs, results_28, results_21, results_14,
   data_serology_analysis_t14, data_serology_analysis_t21, data_serology_analysis_t28); gc()


























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





