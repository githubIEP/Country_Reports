

ETR <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()


# etr <- ETR %>%
#   group_by(geocode) %>%
#   summarise(value = max(value)) %>%
#   dplyr::filter(str_starts(geocode, GEOCODE)) %>%
#   dplyr::filter(value == max(value)) %>%
#   rename(`ETR` = value) %>%
#   mutate(geocode = GEOCODE) %>%
#   distinct()


# etr_food <- ETR %>%
#   dplyr::filter(variablename == "Food Security (BANDED)") %>%
#   dplyr::filter(str_starts(geocode, GEOCODE)) %>%
#   dplyr::filter(year == max(year))
# 
# 
#   

etr_food <- ETR %>%
  dplyr::filter(variablename == "Food Security (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Food Security` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()

etr_water <- ETR %>%
  dplyr::filter(variablename == "Water Risk (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Water Risk` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()


etr_natr <- ETR %>%
  dplyr::filter(variablename == "Natural Hazard Exposure (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Natural Hazard Exposure` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()



etr_dem <- ETR %>%
  dplyr::filter(variablename == "Population Increase to 2050 (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Demographic Pressure` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()


# 
# data_frames <- list(
#   etr_food, etr_water, etr_natr
# )
# 
# etr_all <- etr_dem
# 
# for(df in data_frames) {
#   etr_all <- etr_all %>% left_join(df, by = "geocode")
# }
# 
# 
# etr_all <- etr_all %>%
#   dplyr::select(-c(geocode))
# 
# 
# 
# 
# 
# etr1 <- etr_food %>%
#   left_join(etr_water)
# 
# 
# 
# 
# etr2 <- etr_dem %>%
#   left_join(etr_natr)
# 
# 
# 
# etr1 <- etr1 %>%
#   dplyr::select(-c(geocode))
# 
# etr2 <- etr2 %>%
#   dplyr::select(-c(geocode))
# 
# 
# 
# matrix1 <- as.matrix(etr1)
# matrix2 <- as.matrix(etr2)
# 
# # Combine the matrices
# combined_matrix <- rbind(matrix1, matrix2)
# 
# # Print the result
# print(combined_matrix)
# 
# dfs <- list(etr1, etr2)
# 
# # Standardize column names
# dfs <- lapply(dfs, function(x) { names(x) <- c("V1", "V2"); return(x) })
# 
# # Combine the data frames
# etr_all <- do.call(rbind, dfs)
# 
# # Print the result
# print(etr_all)
# 
# 
# 
# 
# 
# 
# 
# 
etr_food <- etr_food %>%
  dplyr::select(-c(`geocode`))

etr_water <- etr_water %>%
   dplyr::select(-c(`geocode`))

etr_dem <- etr_dem %>%
  dplyr::select(-c(`geocode`))

etr_natr <- etr_natr %>%
  dplyr::select(-c(`geocode`))


### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                sf,
                iepg,
                scales,
                patchwork,
                extrafont)



# Export Data
plot_list <- c("pCHART_PPI", "pCHART_ACLED", "pMAP")

selected_columns <- c("geocode", "GPI Rank", "GTI Rank", "PPI Rank", "ETR")
# selected_columns1 <- c("Food Security")
# Write selected columns to an Excel file
write_selected_columns_to_excel(Indicators.df, selected_columns, "04_outputs/country_report.xlsx")
#Save Plots
save_plots_as_png(plot_list)

# Load this to existing workbook
existing_wb <- loadWorkbook("04_outputs/country_report.xlsx")

# Write the generated text for GPI into a single cell for example, starcol is 1 and start row is 4 so its Column A and row 4
writeData(existing_wb, sheet = "Sheet1", GPI_Sentence.df$text, startCol = 1, startRow = 4, colNames = FALSE)
writeData(existing_wb, sheet = "Sheet1", PPI_Sentence.df$text, startCol = 1, startRow = 6, colNames = FALSE)
writeData(existing_wb, sheet = "Sheet1", ETR_Sentence.df$text, startCol = 1, startRow = 8, colNames = FALSE)
writeData(existing_wb, sheet = "Sheet1", etr_food, startCol = 1, startRow = 10, colNames = TRUE)
writeData(existing_wb, sheet = "Sheet1", etr_water, startCol = 2, startRow = 10, colNames = TRUE)
writeData(existing_wb, sheet = "Sheet1", etr_natr, startCol = 1, startRow = 12, colNames = TRUE)
writeData(existing_wb, sheet = "Sheet1", etr_dem, startCol = 2, startRow = 12, colNames = TRUE)

# Save the changes
saveWorkbook(existing_wb, file = "04_outputs/country_report.xlsx", overwrite = TRUE)


