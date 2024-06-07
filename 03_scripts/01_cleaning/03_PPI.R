
# PPI Overall Score ===========================================================================================

PPI <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  dplyr::rename(PPI = `value`)
