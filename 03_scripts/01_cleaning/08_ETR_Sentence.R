

ETR_domain1.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Water Risk (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  rename(`Water Risk` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()

ETR_domain2.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Food Security (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  rename(`Food Security` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()


ETR_domain3.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Natural Hazard Exposure (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  dplyr::filter(value == max(value)) %>%
  rename(`Natural Hazard Exposure` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()


ETR_domain4.df <- iepg_search("ETR 2023") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(variablename == "Population Increase to 2050 (BANDED)") %>%
  group_by(geocode) %>%
  summarise(value = max(value)) %>%
  dplyr::filter(str_starts(geocode, GEOCODE)) %>%
  rename(`Population Increase to 2050` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()




data_frames <- list(
  ETR_domain1.df, ETR_domain2.df, ETR_domain3.df
)

ETR_Sentence.df <- ETR_domain4.df

for(df in data_frames) {
  ETR_Sentence.df <- ETR_Sentence.df %>% left_join(df)
}


ETR_Sentence.df <- ETR_Sentence.df %>%
  rename(`Food Insecurity` = `Food Security`) %>%
  rename(`Demographic Pressure` = `Population Increase to 2050`)

ETR_Sentence.df <- add_food_insecurity_band(ETR_Sentence.df)
ETR_Sentence.df <- add_water_risk_band(ETR_Sentence.df)
ETR_Sentence.df <- add_natural_hazard_exposure_band(ETR_Sentence.df)
ETR_Sentence.df <- add_demographic_pressure_band(ETR_Sentence.df)



# Function to generate the text
generate_text <- function(row) {
  
  # Construct the text
  text <- paste("According to the latest edition of the Ecological Threat Report,", COUNTRY_NAME, "had a demographic pressure score of", row["Demographic Pressure"], 
                ".", "This means that", COUNTRY_NAME, "is at", row["Demographic_pressure"], "risk of population increases by 2050.",
                COUNTRY_NAME, "is also at", row["Water_Risk"], "of water insecurity, having a maximum score of", row["Water Risk"], ".",
                COUNTRY_NAME, "has a maximum food insecurity score of", row["Food Insecurity"], ".", "This means the country is in",
                row["Food_Insecurity"], "risk of food insecurity. Lastly,", COUNTRY_NAME, "has a maximum natural hazard exposure score of",
                row["Natural Hazard Exposure"], ".", "This means the country is at", row["Natural_Hazard_Exposure"], "risk of natural hazard exposure."
  )
  
  return(text)
}




ETR_Sentence.df$text <- apply(ETR_Sentence.df, 1, generate_text)



