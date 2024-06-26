
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, readr, openxlsx, readxl, stringr, 
               DataCombine, countrycode, ggplot2, patchwork, bannerCommenter)

devtools::install_github("david-hammond/tidyindexR")

options (scipen = 999)


source("03_scripts/ProjectFunction.R")
source("03_scripts/ProjectVariables.R")
source("03_scripts/01_cleaning/01_GTI.R")
source("03_scripts/01_cleaning/02_GPI.R")
source("03_scripts/01_cleaning/03_PPI.R")
source("03_scripts/01_cleaning/04_ETR.R")
source("03_scripts/01_cleaning/05_Combined_data_frame.R")
source("03_scripts/01_cleaning/06_GPI_Sentence.R")
source("03_scripts/01_cleaning/07_PPI_Sentence.R")
source("03_scripts/01_cleaning/08_ETR_Sentence.R")
source("03_scripts/01_cleaning/09_Calculating_Risk_Profile.R")
source("03_scripts/02_standardOutputs/ChartsandTables.R")
source("03_scripts/ProjectExport.R")
