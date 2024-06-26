
library(zoo)


df <- iepg_search()


df1 <- df %>%
  dplyr::filter(variablename == "GDP per capita (constant 2010 US$)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `value`)) %>%
  rename(`GDP Per Capita Constant 2010 US$` = value)



df2 <- df %>%
  dplyr::filter(variablename == "Urban population (% of total population)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `value`)) %>%
  rename(`Urbanization Rate` = value)



df3 <- df %>%
  dplyr::filter(source == "GPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `population`)) %>%
  rename(`Population` = population) %>%
  distinct() 



data_frames <- list(
  df2, df3
)

df_all <- df1

for(df in data_frames) {
  df_all <- df_all %>% left_join(df, by = "geocode")
}


df_all <- df_all %>%
  dplyr::select(-c(`geocode`))

df_long <- df_all %>%
  pivot_longer(cols = c(`GDP Per Capita Constant 2010 US$`, `Urbanization Rate`, `Population`), 
               names_to = "Country Stats",
               values_to = "Value")
  

# wb <- loadWorkbook("04_outputs/country_report.xlsx")
# 
#  start_col <- 10
#  start_row <- 10
# #
# 
#  writeData(wb, sheet = "Sheet1", df_long, startCol = 1, startRow = 18, borders = "columns")
# #
# #
# # # Create a style with borders
#  borderStyle <- createStyle(border = "TopBottomLeftRight", borderColour = "black")
# #
# # # Get the dimensions of the data frame
#  num_rows <- nrow(df_long)
#  num_cols <- ncol(df_long)
# #
# # # Apply the style to the data range
#  addStyle(wb, sheet = "Sheet1", style = borderStyle, rows = 18:(num_rows + 1), cols = 1:num_cols, gridExpand = TRUE)
# #
#  # addStyle(wb, sheet = "Sheet1", style = borderStyle, rows = 18:4, cols = 1:2, gridExpand = TRUE)
# #
# #
#  saveWorkbook(wb, "04_outputs/country_report.xlsx", overwrite = TRUE)
# #
#

wb <- loadWorkbook("04_outputs/country_report.xlsx")
# addWorksheet(wb, "Sheet1")  # Add a new worksheet if necessary
writeDataTable(wb, "Sheet1", x = df_long, tableStyle = "TableStyleMedium1", withFilter = FALSE, startRow = 18, startCol = 1)
saveWorkbook(wb, "04_outputs/country_report.xlsx", overwrite = TRUE)


