##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Hematologie
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################
# Ik heb maandag met de andere auteur naar de resultaten gekeken en hij vroeg zich af of we toch niet een 
# manier kunnen verzinnen om de hematologische parameters per groep per monstername dag met elkaar kunnen 
# vergelijken. Zoals we het volgens mij nu gedaan hebben is met de Tukey post hoc test is wel een verschil 
# per groep maar niet per dag. Zou jij eens kunnen kijken of dat wel te doen is of dat de sample size per
# dag te klein is? 

#------------------ Import
hematologie_df_org <- read_excel("Data/hematologie Sara.xlsx") %>% 
  clean_names()

#------------------ Clean
hematologie_df <- hematologie_df_org %>% 
  mutate(lamb  = factor(lamb), 
         group = factor(group, levels = c("A", "AT", "T", "C")))
unique(hematologie_df$day) %>% length # 12 time point 

#------------------ Visualisatie 
# Bereken de gemiddelde waarde en standaardfout per tijdsmoment per groep
function_plot_hematologie <- function(data, column_name, x_label, title){
  
  column <- column_name
  
  summary_hematologie_df <- data %>%
    select(group, day, column = all_of(column)) %>% 
    group_by(group, day) %>%
    reframe(mean_value = mean(column, na.rm = T),
            se         = sd(column, na.rm = T) / sqrt(n()))
  

  if(x_label){
    
    ggplot(summary_hematologie_df, aes(x = day, y = mean_value, fill = group, color = group)) +
    geom_line() +  
    geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se), alpha = 0.2, color = NA) +  
    labs(title = "", x = "Time", y = title, fill = "", color = "") +
    # scale_color_manual(values = c("A" = "blue", "AT" = "red", "C" = "green", "T" = "purple")) + 
    # scale_fill_manual(values = c("A" = "blue", "AT" = "red", "C" = "green", "T" = "purple")) + 
    theme_classic() + 
      theme(text = element_text(size = 16))} else{
      
      ggplot(summary_hematologie_df, aes(x = day, y = mean_value, fill = group, color = group)) +
        geom_line() +  
        geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se), alpha = 0.2, color = NA) +  
        labs(title = "", x = "", y = title, fill = "", color = "") +
        # scale_color_manual(values = c("A" = "blue", "AT" = "red", "C" = "green", "T" = "purple")) + 
        # scale_fill_manual(values = c("A" = "blue", "AT" = "red", "C" = "green", "T" = "purple")) + 
        theme_classic() + 
        theme(text = element_text(size = 16))
      }
}


  
# Inlezen uitkomsten KW
outcome_KW_hematology <- read_excel("Data/outcome_KW_hematology.xlsx") %>% 
  mutate(y_value = case_when(
    outcome == "wbs" ~ 12, 
    outcome == "plt" ~ 700, 
    outcome == "neut" ~ 3.5, 
    outcome == "lymf" & group2 != "C" & group1 == "A" & day == 8 ~ 9.5, 
    outcome == "lymf" ~ 10),  
    group = group1, 
    shape_id = factor(if_else(group2 == "C", "1", "0" ), levels = c("1", "0")))

plot_wbc  <- function_plot_hematologie(data = hematologie_df, column_name = "wbc", x_label = FALSE, title = "Mean white blood cell count (g/l)") +
  geom_point(outcome_KW_hematology %>% filter(outcome == "wbs"), mapping = aes(x = day, y = y_value, shape=shape_id), show.legend = FALSE, color = "black") + 
  scale_shape_manual(values = c(8, 3))

plot_plt  <- function_plot_hematologie(data = hematologie_df, column_name = "plt", x_label = FALSE, title = "Mean platelet count (g/l)") +
  geom_point(outcome_KW_hematology %>% filter(outcome == "plt"), mapping = aes(x = day, y = y_value, shape=shape_id), show.legend = FALSE, color = "black") + 
  scale_shape_manual(values = c(8, 3))

plot_neut <- function_plot_hematologie(data = hematologie_df, column_name = "neut", x_label = TRUE, title = "Mean neutrophil count (g/l)") +
  geom_point(outcome_KW_hematology %>% filter(outcome == "neut"), mapping = aes(x = day, y = y_value, shape=shape_id), show.legend = FALSE, color = "black") + 
  scale_shape_manual(values = c(8, 3))

plot_lymf <- function_plot_hematologie(data = hematologie_df, column_name = "lymf", x_label = TRUE, title = "Mean lymphocyte count (g/l)") +
  geom_point(outcome_KW_hematology %>% filter(outcome == "lymf"), mapping = aes(x = day, y = y_value, shape=shape_id), show.legend = FALSE, color = "black") + 
  scale_shape_manual(values = c(8, 3))

# Export 
tiff(filename = str_c("Output/plot_hematologie_", format(now(), format = "%Y%m%d_%H%M"), ".tiff"), width =900, height = 700)
ggarrange(plot_wbc, plot_plt, plot_neut, plot_lymf, common.legend = T, legend = "bottom", align = "hv", 
          labels = c("A", "B", "C", "D")) %>% print
dev.off()

#------------------ Analyses
simple_KW_function <- function(data, day, parameter){
  
  day_filter = day
  kruskal_test1 <-  kruskal_test(outcome ~ group, data = data %>% filter(day == day_filter))
  # Perform post-hoc pairwise comparisons using the Dunn test
  post_hoc <- data %>% filter(day == day_filter) %>% dunn_test(outcome ~ group, p.adjust.method = "bonferroni") %>% 
    mutate(day       = day,
           parameter = parameter)
  return(post_hoc)
}

day_0_wbc  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 0, parameter = "wbc")
day_3_wbc  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 3, parameter = "wbc")
day_4_wbc  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 4, parameter = "wbc")
day_6_wbc  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 6, parameter = "wbc")
day_8_wbc  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 8, parameter = "wbc")
day_10_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 10, parameter = "wbc")
day_12_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 12, parameter = "wbc")
day_14_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 14, parameter = "wbc")
day_17_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 17, parameter = "wbc")
day_21_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 21, parameter = "wbc")
day_24_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 24, parameter = "wbc")
day_28_wbc <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = wbc), day = 28, parameter = "wbc")

day_0_plt  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 0, parameter = "plt")
day_3_plt  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 3, parameter = "plt")
day_4_plt  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 4, parameter = "plt")
day_6_plt  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 6, parameter = "plt")
day_8_plt  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 8, parameter = "plt")
day_10_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 10, parameter = "plt")
day_12_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 12, parameter = "plt")
day_14_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 14, parameter = "plt")
day_17_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 17, parameter = "plt")
day_21_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 21, parameter = "plt")
day_24_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 24, parameter = "plt")
day_28_plt <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = plt), day = 28, parameter = "plt")

day_0_lymf  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 0, parameter = "lymf")
day_3_lymf  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 3, parameter = "lymf")
day_4_lymf  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 4, parameter = "lymf")
day_6_lymf  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 6, parameter = "lymf")
day_8_lymf  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 8, parameter = "lymf")
day_10_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 10, parameter = "lymf")
day_12_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 12, parameter = "lymf")
day_14_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 14, parameter = "lymf")
day_17_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 17, parameter = "lymf")
day_21_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 21, parameter = "lymf")
day_24_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 24, parameter = "lymf")
day_28_lymf <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = lymf), day = 28, parameter = "lymf")

day_0_neut  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 0, parameter = "neut")
day_3_neut  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 3, parameter = "neut")
day_4_neut  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 4, parameter = "neut")
day_6_neut  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 6, parameter = "neut")
day_8_neut  <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 8, parameter = "neut")
day_10_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 10, parameter = "neut")
day_12_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 12, parameter = "neut")
day_14_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 14, parameter = "neut")
day_17_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 17, parameter = "neut")
day_21_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 21, parameter = "neut")
day_24_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 24, parameter = "neut")
day_28_neut <- simple_KW_function(data = hematologie_df %>% select(group, day, outcome = neut), day = 28, parameter = "neut")

tabel_overzicht <- day_0_wbc %>% 
  bind_rows(day_3_wbc, day_4_wbc, day_8_wbc, day_10_wbc, day_12_wbc, day_14_wbc, day_17_wbc, day_21_wbc, day_24_wbc, day_28_wbc,
            day_0_plt, day_3_plt, day_4_plt, day_8_plt, day_10_plt, day_12_plt, day_14_plt, day_17_plt, day_21_plt, day_24_plt, day_28_plt,
            day_0_lymf, day_3_lymf, day_4_lymf, day_8_lymf, day_10_lymf, day_12_lymf, day_14_lymf, day_17_lymf, day_21_lymf, day_24_lymf, day_28_lymf,
            day_0_neut, day_3_neut, day_4_neut, day_8_neut, day_10_neut, day_12_neut, day_14_neut, day_17_neut, day_21_neut, day_24_neut, day_28_neut)
tabel_overzicht %>% write_xlsx(path = str_c("Output/overzicht_hematologie_", format(now(), format = "%Y%m%d_%H%M"), ".xlsx"))
# Verwijder objecten die beginnen met "day_"
rm(list = ls(pattern = "^day_"))
rm(list = ls(pattern = "^plot_"))
rm(tabel_overzicht);gc()
# Perform the Kruskal-Wallis test
# function_test <- function(data, day){
#   
#   day_filter = day
#   
#   #------------------ Function to perform permutation test between two groups using Kruskal-Wallis test
#   permutation_test_krus <- function(data, group1, group2, n_permutations = 1000) {
#     # Subset data for the two groups
#     subset_data <- data %>% filter(group %in% c(group1, group2))
#     
#     # Observed test statistic: Kruskal-Wallis Test
#     observed_stat <- kruskal_test(outcome ~ group, data = subset_data)$p
#     
#     # Permutation test
#     perm_stats <- numeric(n_permutations)
#     
#     for (i in 1:n_permutations) {
#       permuted_outcome <- sample(subset_data$outcome)
#       permuted_data    <- subset_data %>% mutate(outcome = permuted_outcome)
#       perm_stats[i]    <- kruskal_test(outcome ~ group, data = permuted_data)$p
#     }
#     
#     # Calculate p-value
#     p_value <- mean(perm_stats <= observed_stat)
#     return(p_value)
#   }
#   
#   #------------------ Get all pairwise combinations of groups
#   group_combinations <- combn(unique(hematologie_df$group), 2, simplify = FALSE)
#   
#   #------------------ Perform permutation tests for all pairwise combinations
#   results_test <- map_dfr(group_combinations, ~{
#     group1 <- .x[1]
#     group2 <- .x[2]
#     p_value <- permutation_test_krus(data = data %>% filter(day == day_filter), group1 = group1, group2 = group2)
#     tibble(group1 = group1, group2 = group2, p_value = p_value)
#   })
#   
#   # Correct for multiple testing
#   results <- results_test %>%
#     mutate(p_adjusted = p.adjust(p_value, method = "bonferroni"))
#   
#   return(results)
# }
# 
# #------------------ Analyses
# day_0_wbc  <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 0)
# day_3_wbc  <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 3)
# day_4_wbc  <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 4)
# day_6_wbc  <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 6)
# day_8_wbc  <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 8)
# day_10_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 10)
# day_12_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 12)
# day_14_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 14)
# day_17_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 17)
# day_21_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 21)
# day_24_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 24)
# day_28_wbc <- function_test(data = hematologie_df %>% select(group, day, outcome = wbc), day = 28)
# 
# day_0_plt  <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 0)
# day_3_plt  <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 3)
# day_4_plt  <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 4)
# day_6_plt  <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 6)
# day_8_plt  <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 8)
# day_10_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 10)
# day_12_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 12)
# day_14_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 14)
# day_17_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 17)
# day_21_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 21)
# day_24_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 24)
# day_28_plt <- function_test(data = hematologie_df %>% select(group, day, outcome = plt), day = 28)
# 
# day_0_lymf  <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 0)
# day_3_lymf  <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 3)
# day_4_lymf  <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 4)
# day_6_lymf  <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 6)
# day_8_lymf  <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 8)
# day_10_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 10)
# day_12_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 12)
# day_14_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 14)
# day_17_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 17)
# day_21_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 21)
# day_24_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 24)
# day_28_lymf <- function_test(data = hematologie_df %>% select(group, day, outcome = lymf), day = 28)
# 
# day_0_neut  <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 0)
# day_3_neut  <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 3)
# day_4_neut  <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 4)
# day_6_neut  <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 6)
# day_8_neut  <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 8)
# day_10_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 10)
# day_12_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 12)
# day_14_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 14)
# day_17_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 17)
# day_21_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 21)
# day_24_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 24)
# day_28_neut <- function_test(data = hematologie_df %>% select(group, day, outcome = neut), day = 28)


