##### ----- EXPORT CHARTS, MAPS AND TABLES


### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                sf,
                iepg,
                scales,
                patchwork,
                extrafont)



# Export Data
plot_list <- c("pCHART_PPI", "pCHART_ACLED")

selected_columns <- c("geocode", "GPI", "GTI", "PPI", "ETR")

# Write selected columns to an Excel file
write_selected_columns_to_excel(Indicators.df, selected_columns, "04_outputs/country_report.xlsx")
#Save Plots
save_plots_as_png(plot_list)

