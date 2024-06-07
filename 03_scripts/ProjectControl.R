
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, readr, openxlsx, readxl, 
               DataCombine, countrycode, ggplot2, patchwork, bannerCommenter)

devtools::install_github("david-hammond/tidyindexR")

options (scipen = 999)


source("03_scripts/ProjectFunction.R")
source("03_scripts/ProjectVariables.R")
source("03_scripts/01_cleaning/01_GTI.R")
source("03_scripts/01_cleaning/02_GPI.R")
source("03_scripts/01_cleaning/03_PPI.R")
source("03_scripts/01_cleaning/04_Combined_data_frame.R")
source("03_scripts/02_standardOutputs/ChartsandTables.R")
source("03_scripts/ProjectExport.R")
