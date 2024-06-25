library(sf)

# 
# battle <- Acled.df %>%
#   dplyr::filter(event_type == "Battles") %>%
#   dplyr::filter(geocode == GEOCODE) %>%
#   dplyr::select(c(`geocode`, `year`, `fatalities`, `longitude`, `latitude`)) %>%
#   iepsqlite::latlong2shp("level1")


df <- battle %>%
  dplyr::select(c(`ID`, `year`, `fatalities`)) %>%
  ungroup()




tmp <- tmp %>%
  group_by(year, ID) %>%
  summarise(fatalities = sum(fatalities))



tmp1 <- df %>%
  group_by(ID, year) %>%
  summarise(fatalities = sum(fatalities))


tmp1 <- tmp1 %>%
  arrange(ID)


tmp1 <- tmp1 %>%
  dplyr::filter(stringr::str_starts(ID, GEOCODE))


tmp1 <- tmp1 %>%
  dplyr::filter(year > 2017)

tmp2 <- tmp1 %>%
  group_by(ID) %>%
  mutate(
    min_year = min(year),
    max_year = max(year),
    min_score = fatalities[year == min_year],
    max_score = fatalities[year == max_year],
    pct_diff = (max_score - min_score) / min_score 
  ) %>%
  ungroup()


tmp2 <- tmp1 %>%
  group_by(ID) %>%
  mutate(
    min_year = min(year),
    max_year = max(year),
    min_score = fatalities[year == min_year],
    max_score = fatalities[year == max_year],
    pct_diff = (max_score - min_score)
  ) %>%
  ungroup()



tmp2 <- tmp2 %>%
  dplyr::select(c(`ID`, `pct_diff`)) %>%
  rename(geocode = `ID`) %>%
  distinct() 
  


s <- shp.df %>%
  dplyr::select(-c(`band`, `color`, `fatalities`))


tmp3 <- s %>%
  left_join(tmp2)

tmp3 <- tmp3 %>%
  mutate(pct_diff = coalesce(pct_diff, 0))


tmp3$band <- ifelse(tmp3$pct_diff < 0, "Decrease in fatalities",
                    ifelse(tmp3$pct_diff == 0, "No Increase in fatalities",
                      ifelse(tmp3$pct_diff > 0 & tmp3$pct_diff <= 50, "Increase of fatalities less than 50",
                             ifelse(tmp3$pct_diff > 50 & tmp3$pct_diff <= 100, "Increase of fatalities between 50 & 100",
                                    ifelse(tmp3$pct_diff > 100 & tmp3$pct_diff <= 200, "Increase of fatalities between 100 & 200",
                                           ifelse(tmp3$pct_diff > 200, "Increase of fatalities over 200", NA))))))



# 
# 
# tmp3$band <- ifelse(tmp3$pct_diff <= 0, "Decrease in fatalities",
#                            ifelse(tmp3$pct_diff > 0 & tmp3$pct_diff <= 50, "Increase of fatalities less than 50",
#                                   ifelse(tmp3$pct_diff > 50 & tmp3$pct_diff <= 100, "Increase of fatalities between 50 & 100",
#                                          ifelse(tmp3$pct_diff > 100 & tmp3$pct_diff <= 200, "Increase of fatalities between 100 & 200",
#                                                 ifelse(tmp3$pct_diff > 200, "Increase of fatalities over 200", NA)))))
# 
# 
# 



category_to_color <- function(category) {
  # Create a named vector for mapping categories to colors
  color_map <- c(
    "Decrease in fatalities" = "green",
    "No Increase in fatalities" = "lightgreen",
    "Increase of fatalities less than 50" = "pink",
    "Increase of fatalities between 50 & 100" = "maroon",
    "Increase of fatalities between 100 & 200" = "red",
    "Increase of fatalities over 200" = "darkred"
  )
  
  # Match the category with the color map
  colors <- color_map[category]
  
  return(colors) 
}



tmp3$color <- category_to_color(tmp3$band)
# tmp3 <- st_as_sf(tmp3)


MAP_BATTLE = c(title = "Increase in Battle Fatalities since 2018",
               sheet = "", source = "IEP Calculations", xtext = "", ytext = "",
               type = "Map", position = "Normal")





pMAP <- ggplot(data = tmp3) +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_fill_manual(values = tmp3$color, 
                    breaks = tmp3$color, 
                    labels = tmp3$band) +
  labs(fill = "Deaths")


pMAP<- f_ThemeTraining(
  plot = pMAP, 
  chart_info = MAP_BATTLE, 
  plottitle = "include", 
  xaxis = "Include", 
  yaxis = "Include", 
  xgridline = "", 
  ygridline = ""
)

pMAP













  








# 
# 
# 
# # Assuming tmp3 has a column 'band' that you want to use for the legend
# 
# colors <- unique(tmp3$color)
# bands <- unique(tmp3$band)
# color_map <- setNames(colors, bands)  # Map colors to corresponding band names
# 
# pMAP <- ggplot(data = tmp3) +
#   geom_sf(aes(fill = band)) +  # Use 'band' for fill aesthetic
#   theme_minimal() +
#   scale_fill_manual(values = color_map, name = "Band") +
#   guides(fill = guide_legend(override.aes = list(color = colors)))
# 
# pMAP <- f_ThemeTraining(
#   plot = pMAP, 
#   chart_info = MAP_BATTLE, 
#   plottitle = "include", 
#   xaxis = "Include", 
#   yaxis = "Include", 
#   xgridline = "", 
#   ygridline = ""
# )
# 
# pMAP
