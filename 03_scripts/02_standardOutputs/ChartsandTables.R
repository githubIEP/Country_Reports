#################################################################
##                       Charts Section:                       ##
##               Positive Peace, maps, incidents               ##
#################################################################

### --- Libraries, Variables, Functions
library(ggrepel)

### --- List of Standard Charts and Tables

# Chart Pie for Burkina Faso's Positive Peace Pillars ====================================================
CHART_PPI = c(title = "Change in Pillars of Peace 2013 - 2022",
                  sheet = "Since 2013, four out of the eight pillars deteriorated more than the other four improved. 
              The overall PPI deteriorated by 2% since 2013.", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")


CHART_ACLED = c(title = "5,224 deaths from terrorism",
              sheet = "", source = "IEP Calculations", xtext = "", ytext = "",
              type = "Chart", position = "Normal")


### --- Loading Data

PPI_df <- rio::import("04_outputs/PPI_pillars.xlsx")

## -- CHART_BAR_CHART_PPI -----------------------------------------------------------------

CHART_PPI.df <- PPI_df %>%
  pivot_longer(cols = -c(geoname, year),
               names_to = "variablename",
               values_to = "PPI") %>%
  dplyr::filter(year > 2012)

CHART_PPI.df <- CHART_PPI.df %>%
  group_by(geoname, variablename) %>%
  mutate(
    min_year = min(year),
    max_year = max(year),
    min_score = PPI[year == min_year],
    max_score = PPI[year == max_year],
    pct_diff = (max_score - min_score) / min_score 
  ) %>%
  ungroup() %>%
  select(-min_year, -max_year, -min_score, -max_score, -year, -geoname, -PPI) %>%
  distinct() %>%
  mutate(color = ifelse(pct_diff > 0, "deterioration", "improvement")) 



CHART_PPI.df <- CHART_PPI.df[order(CHART_PPI.df$pct_diff), ]


CHART_PPI.df <- CHART_PPI.df %>%
  mutate(variablename = factor(variablename, levels = sort(unique(variablename)))) %>%
  mutate(pct = abs(pct_diff))


CHART_PPI.df <- CHART_PPI.df %>%
  mutate(variablename = factor(variablename, levels = variablename[order(pct_diff)]))

pCHART_PPI = ggplot(data=CHART_PPI.df, aes(x=variablename, y= pct, fill=color)) + 
  geom_bar(position=position_dodge(width=0.9), stat='identity') +
  scale_fill_manual(values=c("deterioration"="red", "improvement"="lightblue")) +
  scale_y_continuous(labels=scales::percent) +
  coord_flip()



pCHART_PPI <- f_ThemeTraining(
  plot = pCHART_PPI, 
  chart_info = CHART_PPI, 
  plottitle = "include", 
  xaxis = "Include", 
  yaxis = "Include", 
  xgridline = "Include", 
  ygridline = "Include"
)


pCHART_PPI

# 2. ACLED CHART ==============================================================

# Pulling fatalities data from data base


Acled.df <- iepg_acled() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  group_by(year) %>%
  summarise(fatalities = sum(fatalities, na.rm = TRUE))

CHART_ACLED.df <- Acled.df %>%
  dplyr::filter(year > 2006)
 

pCHART_ACLED <- ggplot(data = CHART_ACLED.df, aes(x = year, y = fatalities)) +
  geom_line (size = 0.75, color = 'red') +
  scale_x_continuous(breaks = c(min(CHART_ACLED.df$year), max(CHART_ACLED.df$year))) 
  
  

pCHART_ACLED <- f_ThemeTraining(
  plot = pCHART_ACLED, 
  chart_info = CHART_ACLED, 
  plottitle = "include", 
  xaxis = "Include", 
  yaxis = "Include", 
  xgridline = "Include", 
  ygridline = "Include"
)


pCHART_ACLED
