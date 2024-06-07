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

# Create Workbook
wbCHARTS_SECTION3 <- createWorkbook()

# List
SECTION3_EXPORT <- c(
  "CHART_PPI")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("3", wbCHARTS_SECTION3, CHARTBOOK_3, SECTION3_EXPORT)


selected_columns <- c("geoname", "GPI", "GTI", "PPI")

# Write selected columns to an Excel file
write_selected_columns_to_excel(Indicators.df, selected_columns, "04_outputs/country_report.xlsx")

