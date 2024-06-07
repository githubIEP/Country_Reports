


GPI_REGION <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(gpi_region == REGION) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c('geocode', 'value')) %>%
  arrange(value = value) %>%
  rename(`overall score` = value)


GPI_score <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`overall score` = value)



GPI_domain1 <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "ongoing conflict") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`ongoing conflict` = value)


GPI_domain2 <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "militarisation") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`militarisation` = value)


GPI_domain3 <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "safety and security") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`safety and security` = value)






data_frames <- list(
  GPI_domain1, GPI_domain2, GPI_domain3
)

GPI_Sentence.df <- GPI_score

for(df in data_frames) {
  GPI_Sentence.df <- GPI_Sentence.df %>% left_join(df)
}







