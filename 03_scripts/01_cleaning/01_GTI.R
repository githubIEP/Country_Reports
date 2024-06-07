library(iepg)
library(dplyr)
library(openxlsx)



GTI <- iepg_search("GTI rank") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  dplyr::rename(GTI = `value`)



