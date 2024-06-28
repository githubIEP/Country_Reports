#################################################################
##       Pulling the overall GPI score from the database       ##
#################################################################

# The purpose of this is to print the country's GPI rank on the excel sheet.
# In order to do this we need to create a ranking column based on the country's GPI score

# Similar to the previous script, this pulls the overall GPI score
# It is then filtered by the latest available year
# The code then creates a new ranking column by ranking all the scores from lowest to highest to get the GPI rank
# We then filter it for the country we are looking for by its geocode.
# We then have a data frame that selects the geocode, year and Rank column


GPI <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(year == max(year)) %>%
  mutate(`GPI Rank` = rank(value)) %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `GPI Rank`))


