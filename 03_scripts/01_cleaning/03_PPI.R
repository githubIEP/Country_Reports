
#################################################################
##       Pulling the overall PPI score from the database       ##
#################################################################

# The purpose of this is to print the country's PPI rank in the final excel sheet
# We do this by pulling the overall PPI score from the database
# The code then filters for the last available year
# The code then creates a new column which ranks the scores from lowest to highest. This is the rank column
# It is the filtered by the country's geocode and selects three columns (geocode, year, rank)



PPI <- iepg_search("PPI 2023 Report") %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(year == max(year)) %>%
  mutate(`PPI Rank` = rank(value)) %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `PPI Rank`))
