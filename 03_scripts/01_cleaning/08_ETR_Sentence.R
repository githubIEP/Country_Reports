##################################################################
##                    ETR Sentence Structure                    ##
##################################################################

# Just like the GPI and PPI statements, we create a final statement about the country's ETR position.

# The statement will read like this:

# "According to the latest edition of the Ecological Threat Report, Burkina Faso had a demographic pressure score of 5. 
#  This means that Burkina Faso is at severe risk of population increases by 2050. 
#  Burkina Faso is also at severe risk of water insecurity, having a maximum score of 5. 
#  Burkina Faso has a maximum food insecurity score of 5. 
#  This means the country is in severe risk of food insecurity. 
#  Lastly, Burkina Faso has a maximum natural hazard exposure score of 4. 
#  This means the country is at high risk of natural hazard exposure."


# In order to construct this statement we need the max value of the ETR domains for the country.
# Since all data is sub-national, we use the stringr function to filter all the data where the geocode starts with BFA.
# Once we get all the regions in Burkina Faso, we select the max ETR value and we do this for all the ETR domains.



ETR_domain1.df <- iepg_search("ETR 2023") %>%
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

ETR_domain2.df <- iepg_search("ETR 2023") %>%
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
  dplyr::filter(value == max(value)) %>%
  rename(`Population Increase to 2050` = value) %>%
  mutate(geocode = GEOCODE) %>%
  distinct()


# We perform a loop to combine all the ETR domains into one data frame.

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


# The next block of code uses a pre defined function to assign a risk level based on the ETR domain score.
# Since the max value of Burkina Faso's Demographic Pressure is 5, the function assigns the risk level as "sever risk" to that value.
# It does this for all ETR domain scores. 
# We would need this due to the structure of the sentence. 

ETR_Sentence.df <- add_food_insecurity_band(ETR_Sentence.df)
ETR_Sentence.df <- add_water_risk_band(ETR_Sentence.df)
ETR_Sentence.df <- add_natural_hazard_exposure_band(ETR_Sentence.df)
ETR_Sentence.df <- add_demographic_pressure_band(ETR_Sentence.df)



# Function to generate the text
# We now create this function to contruct the statement that would appear in the country report excel sheet. 


generate_text <- function(row) {
  
  # Construct the text
  text <- paste("According to the latest edition of the Ecological Threat Report,", COUNTRY_NAME, "had a demographic pressure score of", row["Demographic Pressure"], 
                ".", "This means that", COUNTRY_NAME, "is at", row["Demographic_pressure"], "of population increases by 2050.",
                COUNTRY_NAME, "is also at", row["Water_Risk"], "of water insecurity, having a maximum score of", row["Water Risk"], ".",
                COUNTRY_NAME, "has a maximum food insecurity score of", row["Food Insecurity"], ".", "This means the country is in",
                row["Food_Insecurity"], "of food insecurity. Lastly,", COUNTRY_NAME, "has a maximum natural hazard exposure score of",
                row["Natural Hazard Exposure"], ".", "This means the country is at", row["Natural_Hazard_Exposure"], "of natural hazard exposure."
  )
  
  return(text)
}



# We apply the text to the dataframe.

ETR_Sentence.df$text <- apply(ETR_Sentence.df, 1, generate_text)



