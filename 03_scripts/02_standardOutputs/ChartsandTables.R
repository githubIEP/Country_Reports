#################################################################
##                       Charts Section:                       ##
##               Positive Peace, maps, incidents               ##
#################################################################

### --- Libraries, Variables, Functions
library(ggrepel)

### --- List of Standard Charts and Tables

# Chart Pie for Burkina Faso's Positive Peace Pillars ====================================================

CHART_PPI <- list( title = "Change in Pillars of Peace 2013 - 2022",
  sheet = "",  
  source = "IEP Calculations",
  xtext = "",
  ytext = "",
  type = "Chart",
  position = "Normal"
)


CHART_ACLED <- list(
  title = "",
  sheet = "", 
  source = "IEP Calculations", 
  xtext = "", 
  ytext = "",
  type = "Chart", 
  position = "Normal"
)


MAP_BATTLE = c(title = "",
               sheet = "",
               source = "IEP Calculations",
               xtext = "", 
               ytext = "",
               type = "Map", 
               position = "Normal")



## -- CHART_BAR_CHART_PPI -----------------------------------------------------------------

CHART_PPI.df <- PPI_pillars.df %>%
  pivot_longer(cols = -c(geocode, year),
               names_to = "variablename",
               values_to = "PPI") %>%
  dplyr::filter(year > 2012)

CHART_PPI.df <- CHART_PPI.df %>%
  group_by(geocode, variablename) %>%
  mutate(
    min_year = min(year),
    max_year = max(year),
    min_score = PPI[year == min_year],
    max_score = PPI[year == max_year],
    pct_diff = (max_score - min_score) / min_score 
  ) %>%
  ungroup() %>%
  select(-min_year, -max_year, -min_score, -max_score, -year, -geocode, -PPI) %>%
  distinct() %>%
  mutate(color = ifelse(pct_diff > 0, "deterioration", "improvement")) 

CHART_PPI.df <- CHART_PPI.df[order(CHART_PPI.df$pct_diff), ]


CHART_PPI.df <- CHART_PPI.df %>%
  mutate(variablename = factor(variablename, levels = sort(unique(variablename)))) %>%
  mutate(pct = abs(pct_diff))

CHART_PPI.df <- CHART_PPI.df %>%
  mutate(variablename = factor(variablename, levels = variablename[order(pct_diff)]))

summary <- count_negative_values(CHART_PPI.df, "pct_diff", "PPI Overall Score")
overall_score <- get_overall_score(CHART_PPI.df, "PPI Overall Score", "pct_diff")
CHART_PPI$sheet <- paste(summary, overall_score)


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
  
  
ACLED_Title <- generate_title(CHART_ACLED.df, "fatalities")

# Assign the generated title string to CHART_ACLED

CHART_ACLED$title <- ACLED_Title


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


# 3. Battle Deaths Map of Country =====================================================

Battle_deaths_df <- iepg_acled() %>%
  dplyr::filter(event_type == "Battles") %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `fatalities`, `longitude`, `latitude`)) %>%
  iepsqlite::latlong2shp("level1") %>%
  dplyr::select(c(`ID`, `year`, `fatalities`)) %>%
  group_by(ID, year) %>%
  summarise(fatalities = sum(fatalities)) %>%
  arrange(ID) %>%
  dplyr::filter(stringr::str_starts(ID, GEOCODE)) 


year_data <- Battle_deaths_df %>%
  group_by(ID) %>%
  mutate(
    min_year = min(year)
  )

min_year <- as.numeric(max(year_data$min_year))


Battle_deaths_df <- Battle_deaths_df %>%
  dplyr::filter(year > (min_year-1))


Battle_deaths_df <- Battle_deaths_df %>%
  group_by(ID) %>%
  mutate(
    min_year = min(year),
    max_year = max(year),
    min_score = fatalities[year == min_year],
    max_score = fatalities[year == max_year],
    diff = (max_score - min_score)
  ) %>%
  ungroup() %>%
  dplyr::select(c(`ID`, `diff`)) %>%
  rename(geocode = `ID`) %>%
  distinct() 


shp.df <- iepg_get_gadm("level1") %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::select(-c(`pop_year`))


shp.df <- shp.df %>%
  left_join(Battle_deaths_df) %>%
  mutate(diff = coalesce(diff, 0))
  

shp.df$band <- ifelse(shp.df$diff < 0, "Decrease in fatalities",
                    ifelse(shp.df$diff == 0, "No Increase in fatalities",
                           ifelse(shp.df$diff > 0 & shp.df$diff <= 50, "Increase in fatalities of less than 50",
                                  ifelse(shp.df$diff > 50 & shp.df$diff <= 100, "Increase in fatalities between 50 & 100",
                                         ifelse(shp.df$diff > 100 & shp.df$diff <= 200, "Increase in fatalities between 100 & 200",
                                                ifelse(shp.df$diff > 200, "Increase in fatalities over 200", NA))))))



  

shp.df$color <- category_to_color(shp.df$band)


pMAP <- ggplot(data = shp.df) +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_fill_manual(values = shp.df$color, 
                    breaks = shp.df$color, 
                    labels = shp.df$band) +
  labs(fill = "Deaths")

MAP_title <- generate_title_map(Battle_deaths_df, min_year)
MAP_BATTLE$title <- MAP_title

pMAP<- f_ThemeTraining(
  plot = pMAP, 
  chart_info = MAP_BATTLE, 
  plottitle = "include", 
  xaxis = "Include", 
  yaxis = "Include", 
  xgridline = "", 
  ygridline = ""
) +
  theme(panel.grid = element_blank())

pMAP


# 4. Creating ETR Table =======================================================


ETR_Food.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Food Security (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Food Security` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct() 


ETR_Water.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Water Risk (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Water Risk` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct() 


ETR_Natural.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Food Security (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Natural Hazard Exposure` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct() 


ETR_Demographic.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Population Increase to 2050 (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Demographic Pressure` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct() 


data_frames <- list(
  ETR_Water.df, ETR_Natural.df, ETR_Demographic.df
)

ETR_TABLE_df <- ETR_Food.df

for(df in data_frames) {
  ETR_TABLE_df <- ETR_TABLE_df %>% 
    left_join(df, by = "geocode")
}


ETR_TABLE_df <- ETR_TABLE_df %>%
  dplyr::select(-c(`geocode`))

ETR_TABLE_df <- ETR_TABLE_df %>%
  pivot_longer(cols = c(`Food Security`, `Water Risk`, `Natural Hazard Exposure`, `Demographic Pressure`), 
               names_to = "ETR Domains",
               values_to = "ETR Score out of 5")



# 5. Creating the Country Stats Table =====================================================

GDP_df <- iepg_search() %>%
  dplyr::filter(variablename == "GDP per capita (constant 2010 US$)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `value`)) %>%
  rename(`GDP Per Capita Constant 2010 US$` = value)



Urban_df <- iepg_search() %>%
  dplyr::filter(variablename == "Urban population (% of total population)") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `value`)) %>%
  rename(`Urbanization Rate` = value)



Population_df <- iepg_search() %>%
  dplyr::filter(source == "GPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c(`geocode`, `population`)) %>%
  rename(`Population` = population) %>%
  distinct() 



data_frames <- list(
  Urban_df, Population_df
)

TABLE_df <- GDP_df

for(df in data_frames) {
  TABLE_df <- TABLE_df %>% 
    left_join(df, by = "geocode")
}


TABLE_df <- TABLE_df %>%
  dplyr::select(-c(`geocode`))

TABLE_df <- TABLE_df %>%
  pivot_longer(cols = c(`GDP Per Capita Constant 2010 US$`, `Urbanization Rate`, `Population`), 
               names_to = "Country Stats",
               values_to = "Value")



