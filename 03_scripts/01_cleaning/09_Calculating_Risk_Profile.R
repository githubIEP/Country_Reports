Risk_GPI_df <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`GPI Score` = value)



Risk_PPI_df <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`PPI Score` = value)



Risk_ETR_df <- iepg_search("ETR 2023") %>%
  dplyr::filter(variablename == "Water Risk (BANDED)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`ETR` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()




data_frames <- list(
  Risk_ETR_df, Risk_PPI_df
)

Risk_df <- Risk_GPI_df

for(df in data_frames) {
  Risk_df <- Risk_df %>% 
    left_join(df, by = "geocode")
}



Risk_df <- Risk_df %>%
  dplyr::select(-c(`year.x`, `year.y`))



Risk_df <- calculate_risk_levels(Risk_df, "GPI Score", "ETR", "PPI Score") %>%
  dplyr::select(-c(`geocode`, `GPI Score`, `ETR`, `PPI Score`, `composite_index`)) %>%
  rename(`Risk Level` = risk_level)

