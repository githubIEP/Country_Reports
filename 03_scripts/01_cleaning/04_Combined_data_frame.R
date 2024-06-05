
library(iepg)


PPI1 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Acceptance of the Rights of Others") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Acceptance of the Rights of Others` = `value`)



PPI2 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Equitable Distribution of Resources") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Equitable Distribution of Resources` = `value`)



PPI3 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Free Flow of Information") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Free Flow of Information` = `value`)


PPI4 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`PPI Overall Score` = `value`)


PPI5 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Sound Business Environment") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Sound Business Environment` = `value`)


PPI6 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Well-Functioning Government") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Well-Functioning Government` = `value`)


PPI7 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Low Levels of Corruption") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Low Levels of Corruption` = `value`)


PPI8 <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "Good Relations with Neighbours") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c(`geoname`, `year`, `value`)) %>%
  rename(`Good Relations with Neighbours` = `value`)



# List of data frames to join
data_frames <- list(
  PPI2, PPI3, PPI4, PPI5, PPI6, PPI7, PPI8
)

# Perform left joins in a loop
PPI_pillars.df <- PPI1

for(df in data_frames) {
  PPI_pillars.df <- PPI_pillars.df %>% left_join(df)
}

rm(PPI1,PPI2, PPI3, PPI4, PPI5, PPI6, PPI7, PPI8)

rio::export(PPI_pillars.df, "04_outputs/PPI_Pillars.xlsx")
