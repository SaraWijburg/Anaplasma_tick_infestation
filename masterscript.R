##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Script Anaplasma - Ticks - Lambs  
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################
# Clean environment 
rm(list=ls())

##############################################################################################################
# Inladen packages
##############################################################################################################

# Welke packages worden gebruikt in dit script?
# install.packages("devtools")
# devtools::install_github("dustinfife/flexplot")
# devtools::install_github("dustinfife/fifer"); 
gebruikte_packages <- c("tidyverse", "readxl", "writexl","lubridate", "ggpubr", "janitor", "survival", "coin", "multcomp",
                        "rstatix", "RVAideMemoire", "plotrix", "fifer", "flexplot")

# Welke packages zijn al geinstallerd?
geinstalleerde_packages <- rownames(installed.packages())

# Te installeren packages
installeren_packages <- gebruikte_packages[!gebruikte_packages %in% geinstalleerde_packages]
if (length(installeren_packages) > 0) install.packages(pkgs = installeren_packages)

# Laad packages als ze nog niet attached zijn
if (!all(gebruikte_packages %in% .packages())) {
  suppressMessages(invisible(lapply(
    X              = gebruikte_packages,
    FUN            = library,
    character.only = TRUE)))
}

options(scipen=999) # Remove scientific notation 

select <- dplyr::select

##############################################################################################################
# Analyses 
##############################################################################################################
source(file = "Scripts/Script_serology.R")
source(file = "Scripts/Script_pcr.R")
source(file = "Scripts/Script_teken.R")
source(file = "Scripts/Script_hematologie.R")
