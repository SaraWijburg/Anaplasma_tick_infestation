##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Temperature 
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

TEMP_Sara <- read_excel("Data/TEMP Sara.xlsx")

data_temperature <- TEMP_Sara %>% 
  mutate(Group = factor(case_when(
    Group == "A+T" ~ "AT", 
    TRUE ~ Group), levels = c("A", "AT", "T", "C")),
    fever_y_n = if_else(Temperature > 40, 1, 0))

# Visualize
plot_data <- data_temperature %>%
  group_by(Group, Day) %>%
  reframe(mean_value = mean(Temperature, na.rm = T),
          se         = sd(Temperature, na.rm = T) / sqrt(n()))

plot_temp <- plot_data %>% 
  ggplot(mapping = aes(x = Day, y = mean_value, fill = Group, color = Group)) +
  geom_line() +  
  geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se), alpha = 0.2, color = NA) +  
  labs(y = "Temperature (°C)", x = "Day", fill = "", color = "") +
  theme_classic() + 
  xlim(0, 28) + 
  geom_hline(yintercept=40, linetype="dashed", color = "red") + 
  annotate("text", x = 28, y = 40, label = "Fever", vjust = -0.5, hjust = 1, color = "red") + 
  theme(text = element_text(size = 16))

# Export 
tiff(filename = str_c("Output/plot_temperatuur_", format(now(), format = "%Y%m%d_%H%M"), ".tiff"), width =750, height = 350)
plot_temp %>% print
dev.off()

# Analyse
data_analyses <- data_temperature %>% 
  group_by(Group, Lamb) %>% 
  reframe(days_fever = sum(fever_y_n))

# Kruskall-Wallis
kruskal_test1 <- kruskal_test(days_fever ~ Group, data = data_analyses)
post_hoc      <- data_analyses %>% dunn_test(days_fever ~ Group, p.adjust.method = "bonferroni")

# ANOVA
anova_result <- aov(days_fever ~ Group, data = data_analyses)
summary(anova_result)
posthoc_result <- pairwise.t.test(data_analyses$days_fever, data_analyses$Group, p.adjust.method = "bonferroni")
posthoc_result

