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
         group = factor(group, levels = c("A", "AT", "T", "C")))

# Compare values between groups 
last_positive %>% 
  ggplot(mapping = aes(x = group, y = last_positive_time)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme_classic() + 
  labs(x = "", y = "Last day positive")

# Perform the Kruskal-Wallis test
kruskal_test <- kruskal_test(last_positive_time ~ group, data = last_positive)

# Print the result of the Kruskal-Wallis test
print(kruskal_test)

# If the Kruskal-Wallis test is significant, perform post-hoc pairwise comparisons using the Dunn test
post_hoc <- last_positive %>%
  dunn_test(last_positive_time ~ group, p.adjust.method = "bonferroni")
# By adjusting p-values, you reduce the risk of false positives and make your statistical conclusions more reliable.
# Print the result of the post-hoc Dunn test

post_hoc %>% write_xlsx("Output/comparison_pcr.xlsx")
