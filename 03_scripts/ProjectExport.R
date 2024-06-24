##### ----- EXPORT CHARTS, MAPS AND TABLES


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
writeData(existing_wb, sheet = "Sheet1", ETR_Food.df, startCol = 1, startRow = 10, colNames = TRUE)
writeData(existing_wb, sheet = "Sheet1", ETR_Water.df, startCol = 2, startRow = 10, colNames = TRUE)
writeData(existing_wb, sheet = "Sheet1", ETR_Natural.df, startCol = 1, startRow = 12, colNames = TRUE)
writeData(existing_wb, sheet = "Sheet1", ETR_Demographic.df, startCol = 2, startRow = 12, colNames = TRUE)
# Save the changes
saveWorkbook(existing_wb, file = "04_outputs/country_report.xlsx", overwrite = TRUE)

