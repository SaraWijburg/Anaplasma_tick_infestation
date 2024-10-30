##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Teken
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

#------------------ Import
data_ticks_org <- read_excel("Data/teken data Sara.xlsx")

#------------------ Clean 
data_ticks <- data_ticks_org %>% 
  clean_names() %>% 
  mutate(across(c(anaplasma, neoehrlichia, babesia18s, borrelia_flab, borrelia_ospa, 
                  miyamotoi, spiroplasma, babesia_microti, rickettsia_helvetica), as.numeric)) %>%
  mutate(across(c(anaplasma, neoehrlichia, babesia18s, borrelia_flab, borrelia_ospa, 
                  miyamotoi, spiroplasma, babesia_microti, rickettsia_helvetica), ~ if_else(is.na(.), 0, 1), .names = "{.col}_yn")) %>% 
  suppressWarnings()

data_ticks$group %>% table

#------------------ Visualise
# Reshape the data for plot
data_long <- data_ticks %>%
  select(group, ticknr_rivm, contains("yn")) %>% 
  mutate(borrelia_yn = borrelia_flab_yn + borrelia_ospa_yn,
         borrelia_yn = if_else(borrelia_yn == 0, 0, 1)) %>% 
  select(-c(borrelia_flab_yn, borrelia_ospa_yn)) %>% 
  pivot_longer(cols = starts_with("anaplasma_yn"):starts_with("borrelia_yn"), 
               names_to = "pathogen", 
               values_to = "presence") %>%
  mutate(pathogen = gsub("_yn", "", pathogen)) %>% 
  group_by(group, pathogen, presence) %>% 
  count() %>% ungroup %>% 
  mutate(presence = factor(presence))

# Create the plot
ggplot(data_long, aes(x = group, y = n, fill = factor(presence))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("0" = "#af8dc3", "1" = "#7fbf7b"), name = "Presence") +
  theme_minimal() +
  labs(x = "", y = "Proportion", title = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_grid(.~ pathogen)

#------------------------------------------------------------------------------------------------------------# 
# Analysis 
#------------------------------------------------------------------------------------------------------------# 
#------------------ anaplasma
data_ticks %>% select(group, anaplasma_yn) %>% table

# Data prep
data_anaplasma <- data_long %>%
  filter(pathogen == "anaplasma")
# Create a contingency table
contingency_table <- xtabs(n ~ group + presence, data = data_anaplasma)
# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)
# Print the results
print(chi_squared_test)
# Posthoc 
fifer::chisq.post.hoc(contingency_table, test='chisq.test', digits = 20, control = c("bonferroni"))

#------------------ neoehrlichia
data_ticks %>% select(group, neoehrlichia_yn) %>% table
# Data prep
data_neoehrlichia <- data_long %>%
  filter(pathogen == "neoehrlichia")
# Create a contingency table
contingency_table <- xtabs(n ~ group + presence, data = data_neoehrlichia)
# Perform the chi-squared test
fisher_test <- fisher_test(contingency_table)
print(fisher_test)
pairwise_fisher_test(contingency_table, p.adjust.method = "bonferroni", detailed = T)

#------------------ babesia18s
data_ticks %>% select(group, babesia18s_yn) %>% table
# Data prep
data_babesia18s <- data_long %>%
  filter(pathogen == "babesia18s")
# Create a contingency table
contingency_table <- xtabs(n ~ group + presence, data = data_babesia18s)
# Perform the chi-squared test
fisher_test <- fisher_test(contingency_table)
print(fisher_test)
pairwise_fisher_test(contingency_table, p.adjust.method = "bonferroni", detailed = T)

#------------------ borrelia
# Data prep
data_borrelia <- data_long %>%
  filter(pathogen == "borrelia")
# Create a contingency table
contingency_table <- xtabs(n ~ group + presence, data = data_borrelia)
# Perform the chi-squared test
fisher_test <- fisher_test(contingency_table)
print(fisher_test)
pairwise_fisher_test(contingency_table, p.adjust.method = "bonferroni", detailed = T)

#------------------ borrelia_flab
# data_ticks %>% select(group, borrelia_flab_yn) %>% table
# # Data prep
# data_borrelia_flab <- data_long %>%
#   filter(pathogen == "borrelia_flab")
# # Create a contingency table
# contingency_table <- xtabs(n ~ group + presence, data = data_borrelia_flab)
# # Perform the chi-squared test
# fisher_test <- fisher_test(contingency_table)
# print(fisher_test)
# pairwise_fisher_test(contingency_table, p.adjust.method = "bonferroni", detailed = T)
# 
# #------------------ borrelia_ospa 
# data_ticks %>% select(group, borrelia_ospa_yn) %>% table
# data_borrelia_ospa <- data_long %>%
#   filter(pathogen == "borrelia_ospa")
# # Create a contingency table
# contingency_table <- xtabs(n ~ group + presence, data = data_borrelia_ospa)
# # Perform the chi-squared test
# fisher_test <- fisher_test(contingency_table)
# print(fisher_test)
# pairwise_fisher_test(contingency_table, p.adjust.method = "bonferroni", detailed = T)

#------------------ miyamotoi
data_ticks %>% select(group, miyamotoi_yn) %>% table # alles negatief

#------------------ spiroplasma
data_ticks %>% select(group, spiroplasma_yn) %>% table
data_spiroplasma <- data_long %>%
  filter(pathogen == "spiroplasma")
# Create a contingency table
contingency_table <- xtabs(n ~ group + presence, data = data_spiroplasma)
# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)
# Print the results
print(chi_squared_test)
fifer::chisq.post.hoc(contingency_table, test='chisq.test', digits = 20, control = c("bonferroni"))

#------------------ babesia_microti
data_ticks %>% select(group, babesia_microti_yn) %>% table # alles negatief

#------------------ rickettsia_helvetica
data_ticks %>% select(group, rickettsia_helvetica_yn) %>% table
data_rickettsia_helvetica <- data_long %>%
  filter(pathogen == "rickettsia_helvetica")
# Create a contingency table
contingency_table <- xtabs(n ~ group + presence, data = data_rickettsia_helvetica)
# Perform the chi-squared test
fisher_test <- fisher_test(contingency_table)
print(fisher_test)
pairwise_fisher_test(contingency_table, p.adjust.method = "bonferroni", detailed = T)

#------------------------------------------------------------------------------------------------------------# 
# Clean 
#------------------------------------------------------------------------------------------------------------# 
rm(data_anaplasma, data_babesia18s, data_borrelia_flab,data_borrelia_ospa, data_long, data_neoehrlichia, 
   data_rickettsia_helvetica, data_spiroplasma, data_ticks_org, contingency_table, fisher_test, chi_squared_test); gc()
