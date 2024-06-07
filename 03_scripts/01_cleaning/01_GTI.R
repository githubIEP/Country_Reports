library(iepg)
library(dplyr)
library(openxlsx)



GTI <- iepg_search("GTI rank") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  dplyr::rename(GTI = `value`)



