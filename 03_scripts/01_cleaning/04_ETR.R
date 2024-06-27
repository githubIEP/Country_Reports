##################################################################
##          Pulling ETR Water Risk Score from Database          ##
##################################################################

# The purpose of this is to print an ETR score in the final country report excel sheet.
# Since ETR does not have an overall score, we need to select a domain from the four ETR domains
# In this case Water Risk was chosen, but this is subject to change
# Since all ETR data is sub-national, the max score for the selected domain is chosen, again this can change in the future
# The code then uses the stringr function to identify all the admin 1 level geocodes that start with the country's geocode
# For Example since the country's geooode is BFA, any geocode that starts with BFA will be selected.
# The code then selects the maximum value from all the regions.
# This will be the score that will be printed in the country report excel sheet. 



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
