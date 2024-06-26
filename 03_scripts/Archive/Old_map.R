

# 3. Battle Deaths Map of Country =====================================================

Battle_deaths_df <- iepg_acled() %>%
  dplyr::filter(event_type == "Battles") %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::filter(year == LATEST_YEAR) %>%
  dplyr::select(c(`geocode`, `fatalities`, `longitude`, `latitude`)) %>%
  iepsqlite::latlong2shp("level1") %>%
  group_by(ID) %>%
  summarise(fatalities = sum(fatalities)) %>% 
  dplyr::filter(stringr::str_starts(ID, GEOCODE)) %>%
  rename(geooriginal = ID)


shp.df <- iepg_get_gadm("level1") %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::select(-c(`pop_year`))

shp.df <- shp.df %>%
  left_join(Battle_deaths_df) %>%
  mutate(fatalities = coalesce(fatalities, 0))


shp.df$band <- ifelse(shp.df$fatalities >= 0 & shp.df$fatalities < 25, "0 - 25",
                      ifelse(shp.df$fatalities >= 25 & shp.df$fatalities < 50, "25 - 50",
                             ifelse(shp.df$fatalities >= 50 & shp.df$fatalities < 75, "50 - 75",
                                    ifelse(shp.df$fatalities >= 75 & shp.df$fatalities < 100, "75 - 100",
                                           ifelse(shp.df$fatalities > 100, "Over 100", NA)))))


shp.df$band = factor(shp.df$band, c("0 - 25", "25 - 50", "50 - 75", "75 - 100", "Over 100"), ordered=T)


shp.df$color <- category_to_color(shp.df$band)


pMAP <- ggplot(data = shp.df) +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_fill_manual(values = shp.df$color, 
                    breaks = shp.df$color, 
                    labels = shp.df$band) +
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
