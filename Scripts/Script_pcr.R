##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: PCR
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

#------------------ Import
data_pcr_org <- read_excel("Data/PCR results Sara.xlsx", col_types = c("text", "text", "numeric", "numeric")) %>% 
  filter(!is.na(Lamb)) %>% suppressWarnings()

#------------------ Adjust/clean
data_pcr <- data_pcr_org %>% 
  clean_names() %>% 
  mutate(pos_neg = if_else(is.na(anaplasma), 0, 1))

data_pcr$lamb %>% unique %>% length
data_pcr %>% select(lamb, group) %>% table

#------------------ Analyses
# Find the last time point each animal was positive
last_positive <- data_pcr %>%
  filter(pos_neg == 1) %>%
  group_by(group, lamb) %>%
  reframe(last_positive_time = max(date_nr, na.rm = TRUE)) %>% 
  add_row(group = "C", lamb = "A", last_positive_time = 0) %>% 
  arrange(group, lamb) %>% 
  mutate(last_positive_time = if_else(group == "C", 0, last_positive_time), 
         last_positive_time = if_else(group == "T", last_positive_time-4, last_positive_time),
         group = factor(group, levels = c("A", "AT", "T", "C")))

# Compare values between groups 
last_positive %>% 
  ggplot(mapping = aes(x = group, y = last_positive_time)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme_classic() + 
  labs(x = "", y = "Last day positive")

# Plot for pos-neg per day 
library(ggtext)

plot_pcr <- data_pcr %>% 
  select(group, lamb, date_nr, pos_neg) %>% 
  mutate(pos_neg = if_else(group == "C", 0, pos_neg), 
         pos_neg = if_else(pos_neg == 0, "PCR neg.", "PCR pos."),
         group = factor(group, levels = c("A", "AT", "T", "C")),
         lamb_group = str_c("**", group, "** - ", lamb),
         lamb_group = factor(lamb_group, levels = c(
           "**C** - A", "**C** - E", "**C** - I", "**C** - P",
           "**T** - B", "**T** - C", "**T** - K", "**T** - O",
          "**AT** - F", "**AT** - G", "**AT** - J", "**AT** - N",
          "**A** - D", "**A** - H", "**A** - L", "**A** - M")),
         date_nr = date_nr %>% as.character(), 
         date_nr = factor(date_nr, levels = c("0", "3", "4", "6", "8", "10", "12", "14", "17", "21", "24", "28"))) %>% 
  ggplot(mapping = aes(x = date_nr, y = lamb_group, fill = pos_neg)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  scale_fill_manual(values = c("red", "green")) +
  labs(y = "", x = "", fill = "") +
  theme_minimal() + 
  theme( axis.text.y = element_markdown(hjust = 0), 
         text = element_text(size = 16), 
         legend.position= "bottom")

# Export 
tiff(filename = str_c("Output/plot_pcr_pos_neg_", format(now(), format = "%Y%m%d_%H%M"), ".tiff"), width =659, height = 526)
plot_pcr %>% print
dev.off()



# Perform the Kruskal-Wallis test
#------------------ Function to perform permutation test between two groups using Kruskal-Wallis test
permutation_test_krus <- function(data, group1, group2, n_permutations = 1000) {
  # Subset data for the two groups
  subset_data <- data %>% filter(group %in% c(group1, group2))
  
  # Observed test statistic: Kruskal-Wallis Test
  observed_stat <- kruskal_test(last_positive_time ~ group, data = subset_data)$p

    # Permutation test
  perm_stats <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    permuted_outcome <- sample(subset_data$last_positive_time)
    permuted_data    <- subset_data %>% mutate(last_positive_time = permuted_outcome)
    perm_stats[i]    <- kruskal_test(last_positive_time ~ group, data = permuted_data)$p
  }
  
  # Calculate p-value
  p_value <- mean(perm_stats <= observed_stat)
  return(p_value)
}

#------------------ Get all pairwise combinations of groups
group_combinations <- combn(unique(last_positive$group), 2, simplify = FALSE)

#------------------ Perform permutation tests for all pairwise combinations
results_pcr <- map_dfr(group_combinations, ~{
  group1 <- .x[1]
  group2 <- .x[2]
  p_value <- permutation_test_krus(data = last_positive, group1 = group1, group2 = group2)
  tibble(group1 = group1, group2 = group2, p_value = p_value)
})


# Correct for multiple testing
results_pcr <- results_pcr %>%
  mutate(p_adjusted = p.adjust(p_value, method = "bonferroni"))

# kruskal_test1 <-  kruskal_test(last_positive_time ~ group, data = last_positive)
# 
# # Print the result of the Kruskal-Wallis test
# print(kruskal_test1)
# 
# # If the Kruskal-Wallis test is significant, perform post-hoc pairwise comparisons using the Dunn test
# post_hoc <- last_positive %>%
#   dunn_test(last_positive_time ~ group, p.adjust.method = "bonferroni")
# By adjusting p-values, you reduce the risk of false positives and make your statistical conclusions more reliable.
# Print the result of the post-hoc Dunn test

post_hoc %>% write_xlsx("Output/comparison_pcr.xlsx")




# permutation_test_post_hoc <- function(data, group1, group2, n_permutations = 1000) {
#   # Subset data for the two groups
#   subset_data <- data %>% filter(group %in% c(group1, group2))
#   
#   # Observed test statistic: Kruskal-Wallis Test (posthoc)
#   # observed_stat <- kruskal_test(last_positive_time ~ group, data = subset_data)$p
#   observed_stat <- dunn_test(last_positive_time ~ group, p.adjust.method = "bonferroni", data = subset_data)$p
#   # Permutation test
#   perm_stats <- numeric(n_permutations)
#   
#   for (i in 1:n_permutations) {
#     permuted_outcome <- sample(subset_data$last_positive_time)
#     permuted_data    <- subset_data %>% mutate(last_positive_time = permuted_outcome)
#     perm_stats[i]    <- dunn_test(last_positive_time ~ group, p.adjust.method = "bonferroni", data = permuted_data)$p
#   }
#   
#   # Calculate p-value
#   p_value <- mean(perm_stats <= observed_stat)
#   return(p_value)
# }
# 
# results_pcr <- map_dfr(group_combinations, ~{
#   group1 <- .x[1]
#   group2 <- .x[2]
#   p_value <- permutation_test_post_hoc(data = last_positive, group1 = group1, group2 = group2)
#   tibble(group1 = group1, group2 = group2, p_value = p_value)
# })