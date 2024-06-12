

ETR <- iepg_search("ETR 2023") %>%
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
