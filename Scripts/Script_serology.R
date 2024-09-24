##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Serology
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

#------------------ Import 
serology_data_org1 <- read_excel("Data/serology_data.xlsx", sheet = "sampling_schedule")
serology_data_org2 <- read_excel("Data/serology_data.xlsx", sheet = "lamb_info")

#------------------ Clean
source(file = "Scripts/Script_serology_clean.R")

#------------------ Analyses
# Onset of the immune response: Whether there's a difference in the time at which the immune response becomes noticeable between the groups.
# Severity of the immune response: Whether there is a difference in the intensity or severity of the immune response between groups over time.
source(file = "Scripts/Script_serology_analysis.R")



