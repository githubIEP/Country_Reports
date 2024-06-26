
library(zoo)


df <- iepg_search()
  
  

# df1 <- df %>%
#   dplyr::filter(variablename == "GDP per capita (constant 2015 US$)") %>%
#   pull(muid) %>%
#   iepg_get() %>%
#   ungroup() %>%
#   dplyr::filter(geocode == GEOCODE)
# 

df1 <- df %>%
  dplyr::filter(variablename == "GDP per capita (constant 2010 US$)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `value`)) %>%
  rename(`GDP Per Capita Constant 2010 US$` = value) %>%
  dplyr::select(-c(geocode))



df2 <- df %>%
  dplyr::filter(variablename == "Urban population (% of total population)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `value`)) %>%
  rename(`Urbanization Rate` = value) %>%
  dplyr::select(-c(geocode))



df3 <- df %>%
  dplyr::filter(source == "GPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `population`)) %>%
  rename(`Population` = population) %>%
  distinct() %>%
  dplyr::select(-c(geocode))




dfs <- list(df1, df2, df3)
# 
# 
# df_to_list <- function(df) {
#   lapply(seq_len(nrow(df)), function(i) {
#     setNames(as.list(df[i, ]), names(df))
#   })
# }
# 
# 
# dfs_list <- lapply(dfs, df_to_list)
# 
# 
# start_row <- 14
# start_col <- 1
# 
# wb <- loadWorkbook("04_outputs/country_report.xlsx")
# 
#   
# 
# for (i in seq_along(dfs_list)) {
#   current_df_list <- dfs_list[[i]]
#   for (j in seq_along(current_df_list)) {
#     current_row <- start_row + (j - 1)
#     current_col <- start_col + (i - 1) * (ncol(dfs[[i]]) + 1)  # Leave space between data frames
#     writeData(wb, sheet = 1, x = current_df_list[[j]], startCol = current_col, startRow = current_row, colNames = TRUE)
#   }
# }
# 
# 
# saveWorkbook(wb, "04_outputs/country_report.xlsx", overwrite = TRUE)
# 
# 
# 


# 
# dfs_list <- lapply(dfs, df_to_list)
# 
# 
# 
# write_df_to_excel <- function(wb, sheet, df, start_row, start_col) {
#   for (col in 1:ncol(df)) {
#     writeData(wb, sheet = sheet, x = names(df)[col], startCol = start_col, startRow = start_row, colNames = FALSE)
#     writeData(wb, sheet = sheet, x = df[[col]], startCol = start_col, startRow = start_row + 1, colNames = FALSE)
#     start_col <- start_col + 2  # Move to the next column for the next data frame
#   }
# }
# 
# # Write each data frame vertically
# sheet <- 1
# start_row <- 14
# start_col <- 1
# 
# for (df in dfs) {
#   write_df_to_excel(wb, sheet, df, start_row, start_col)
#   start_row <- start_row + nrow(df) + 2  # Move to the next row for the next data frame
# }
# 
# # Save the workbook
# saveWorkbook(wb, "04_outputs/country_report.xlsx", overwrite = TRUE)
# 








df_list <- list(df1, df2, df3)


# Function to write a data frame with column names and values side by side
write_df_with_format <- function(wb, sheet, df, start_row, start_col) {
  # Get the names and values
  names <- colnames(df)
  values <- as.matrix(df)
  
  # Write the names
  writeData(wb, sheet, names, startRow = start_row, startCol = start_col, colNames = FALSE, rowNames = FALSE)
  
  # Write the values
  for (i in 1:ncol(df)) {
    writeData(wb, sheet, values[, i], startRow = start_row + 1, startCol = start_col + (i - 1) * 2, colNames = FALSE, rowNames = FALSE)
  }
  
  # Apply borders
  addStyle(wb, sheet, style = createStyle(border = "bottom", borderStyle = "thin"), 
           rows = start_row, cols = start_col:(start_col + ncol(df) * 2 - 1), gridExpand = TRUE)
  addStyle(wb, sheet, style = createStyle(border = "right", borderStyle = "thin"), 
           rows = start_row:(start_row + nrow(df)), cols = start_col:(start_col + ncol(df) * 2 - 1), gridExpand = TRUE)
}

# Write each data frame to the sheet with proper formatting
start_row <- 20
start_col <- 1
for (df in df_list) {
  write_df_with_format(wb, sheet, df, start_row, start_col)
  start_row <- start_row + nrow(df) + 2  # Move to the next row for the next data frame
}

# Save the workbook
saveWorkbook(wb, "04_outputs/country_report.xlsx", overwrite = TRUE)







