
#####################################################################################################################
##  This script takes in a processed file with ranks etc calculated and then creates a generic statement about it  ##
#####################################################################################################################

### It purpose is to construct a subnational statement like this:

### "Ghor ranks equal 18 out of 34 administrative units in Afghanistan.
## Globally, it is in the 88th percentile placing it as having Very High vulnerability.
### This compares to South Asia's average being in the 81st percentile for vulnerability."

###  Read in data

scores = rio::import("02_data/processed/dummy-data.xlsx")

###  Calculate Regional Averages
scores = scores %>% group_by(region, domain, variablename) %>%
  mutate(reg_avg = median(banded)) %>% ungroup() %>%
  group_by(domain, variablename) %>%
  mutate(reg_avg_pc = 100*round(scales::rescale(reg_avg),2)) %>% ungroup()

###  Define a percentage cutoff for statement
cutoff = 10
scores$regional_statement = ifelse(scores$reg_avg_pc < cutoff,
                                   paste("in the", cutoff, "per cent least vulnerable in the world"),
                                   paste(scales::ordinal(scores$reg_avg_pc), "percentile for vulnerability."))
###  Construct the sentence
scores$global_statement = 100*round(scales::rescale(scores$banded), 2)
scores$global_statement = ifelse(scores$global_statement < cutoff,
                                 paste("in the", cutoff, "per cent least vulnerable in the world"),
                                 paste(scales::ordinal(100*round(scales::rescale(scores$global_statement), 2)),
                                       " percentile placing it as having ",
                                       scores$country_cat,
                                       " vulnerabilty in this indicator."))

scores$statements = paste("The", scores$variablename, "subindicator from",
                          scores$source, "in the", scores$domain,
                          "domain is only available at the national level.")

scores$rank_statement = ifelse(scores$equal_ranks == 1, " ranks equal ", ' ranks ')

scores$statements[1] = ifelse(scores$data_level == 1,
                           paste0(scores$geoname,
                                  scores$rank_statement,
                                  scores$in_country_rank,
                                  ' out of ',
                                  scores$num_adm,
                                  " administrative units in ",
                                  scores$country,
                                  ". Globally, it is in the ",
                                  scales::ordinal(100*round(scales::rescale(scores$banded), 2)),
                                  " percentile placing it as having ",
                                  scores$country_cat,
                                  " vulnerability. This compares to ",
                                  scores$region,
                                  "'s average being in the ",
                                  scores$regional_statement),
                           scores$statements)
