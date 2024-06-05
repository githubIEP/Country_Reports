#################################################################
##                       Charts Section:                       ##
##               Positive Peace, maps, incidents               ##
#################################################################

### --- Libraries, Variables, Functions
library(knitr)
library(openxlsx)
library(reshape2)
library(iepg)
library(dplyr)
library(scales)
library(cowplot)
library(grid)
library(iepsqlite)
library(patchwork)
library(kableExtra)
library(rvest)
library(ggrepel)

### --- List of Standard Charts and Tables

# Chart Pie for Burkina Faso's Positive Peace Pillars ====================================================
CHART_PPI = c(title = "Change in Pillars of Peace 2009 - 2022",
                  sheet = "Pillars", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")



### --- Loading Data

PPI_df <- rio::import("04_outputs/PPI_pillars.xlsx")



## -- CHART_BAR_CHART_PPI -----------------------------------------------------------------

CHART_PPI.df <- PPI_df %>%
  pivot_longer(cols = -c(geoname, year),
               names_to = "variablename",
               values_to = "PPI")


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
  mutate(color = ifelse(pct_diff > 0, "positive", "negative"))





pCHART_PPI <- ggplot(CHART_PPI.df, aes(x = pct_diff, y = reorder(variablename, pct_diff), fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(pct_diff)), hjust = -0.2, size = 3) +
  scale_fill_manual(values = c("positive" = "red", "negative" = "lightblue"), guide = FALSE) + 
  scale_x_continuous(labels = scales::percent_format(prefix = "", big.mark = ",")) 




pCHART_PPI <- f_ThemeTraining(plot = pCHART_PPI, 
                                     chart_info = CHART_PPI, 
                                     plottitle = "", 
                                     xaxis = "", 
                                     yaxis = "Include", 
                                     xgridline = "Include", 
                                     ygridline = "") +
  theme(legend.position = "none")



pCHART_PPI


