##### ----- EXPORT CHARTS, MAPS AND TABLES
#' Note that for the export to work, the standardoutputs and analysis scripts have to 
#' be run first, in order to create the chart information, plot, and df objects

### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                sf,
                iepg,
                scales,
                patchwork,
                extrafont)



### --- Section 3 --------------------------------------------------------------
#' Section 3 Econ Costing Charts

# Export Data
plot_list <- c("pCHART_PPI")

selected_columns <- c("geoname", "GPI", "GTI", "PPI")

# Write selected columns to an Excel file
write_selected_columns_to_excel(Indicators.df, selected_columns, "04_outputs/country_report.xlsx")
save_plots_as_png(plot_list)

