#################################################################
##    This script is pulling the GTI rank from the database    ##
#################################################################

# The Purpose of this is to print the GTI rank that would eventually appear on the excel file
# First we pull it from the database then filter it by the country's geocode and the last available year
# And what we are left with is data frame with 3 columns and one row, the geocode, year and rank


GTI <- iepg_search("GTI rank") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  dplyr::rename(`GTI Rank` = `value`)