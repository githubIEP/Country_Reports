
#### GPI Rank ================================================


GPI <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  dplyr::rename(GPI = `value`)





