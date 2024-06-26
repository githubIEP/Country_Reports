
library(zoo)


df <- iepg_search()
  
  

df1 <- df %>%
  dplyr::filter(variablename == "GDP per capita (constant 2015 US$)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE)


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



dfs <- list(df1, df2, df3)


df_to_list <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    setNames(as.list(df[i, ]), names(df))
  })
}


dfs_list <- lapply(dfs, df_to_list)


start_row <- 14
start_col <- 1

wb <- loadWorkbook("04_outputs/country_report.xlsx")

  

for (i in seq_along(dfs_list)) {
  current_df_list <- dfs_list[[i]]
  for (j in seq_along(current_df_list)) {
    current_row <- start_row + (j - 1)
    current_col <- start_col + (i - 1) * (ncol(dfs[[i]]) + 1)  # Leave space between data frames
    writeData(wb, sheet = 1, x = current_df_list[[j]], startCol = current_col, startRow = current_row, colNames = TRUE)
  }
}


saveWorkbook(wb, "04_outputs/country_report.xlsx", overwrite = TRUE)

