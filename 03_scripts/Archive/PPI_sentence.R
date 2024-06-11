
PPI <- iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() 


PPI1 <- rio::import("02_data/processed/ppi.xlsx")



PPI_REGION <- PPI1 %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  dplyr::filter(gpi_region == REGION) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c('geocode', 'value')) %>%
  arrange(value = value) %>%
  rename(`overall score` = value) %>%
  mutate(`Regional Rank` = rank(`overall score`)) %>%
  dplyr::select(`geocode`, `Regional Rank`) %>%
  subset(geocode == GEOCODE)


PPI_PILLAR1.df <- PPI1 %>%
  dplyr::filter(variablename == "Equitable Distribution of Resources") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Equitable Distribution of Resources` = `value`)



PPI_PILLAR2.df <- PPI1 %>%
  dplyr::filter(variablename == "Acceptance of the Rights of Others") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Acceptance of the Rights of Others` = `value`)


PPI_PILLAR3.df <- PPI1 %>%
  dplyr::filter(variablename == "Free Flow of Information") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Free Flow of Information` = `value`)



PPI_PILLAR4.df <- PPI1 %>%
  dplyr::filter(variablename == "Sound Business Environment") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Sound Business Environment` = `value`)



PPI_PILLAR5.df <- PPI1 %>%
  dplyr::filter(variablename == "Well-Functioning Government") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Well-Functioning Government` = `value`)



PPI_PILLAR6.df <- PPI1 %>%
  dplyr::filter(variablename == "Low Levels of Corruption") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Low Levels of Corruption` = `value`)



PPI_PILLAR7.df <- PPI1 %>%
  dplyr::filter(variablename == "Good Relations with Neighbours") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Good Relations with Neighbours` = `value`)



PPI_PILLAR8.df <- PPI1 %>%
  dplyr::filter(variablename == "High Levels of Human Capital") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`High Levels of Human Capital` = `value`)



PPI_OVERALL.df <- PPI1 %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`PPI Overall Score` = `value`)






data_frames <- list(
  PPI_REGION, PPI_PILLAR1.df, PPI_PILLAR2.df, PPI_PILLAR3.df, PPI_PILLAR4.df,
  PPI_PILLAR5.df,PPI_PILLAR6.df, PPI_PILLAR7.df, PPI_PILLAR8.df
)

PPI_Sentence.df <- PPI_OVERALL.df

for(df in data_frames) {
  PPI_Sentence.df <- PPI_Sentence.df %>% left_join(df)
}



PPI_Sentence.df <- PPI_Sentence.df %>%
  mutate(`overall change` = `PPI Overall Score` - lag(`PPI Overall Score`)) %>%
  mutate(`Acceptance of the Rights of Others change` = `Acceptance of the Rights of Others` - lag(`Acceptance of the Rights of Others`)) %>%
  mutate(`Equitable Distribution of Resources change` = `Equitable Distribution of Resources` - lag(`Equitable Distribution of Resources`)) %>%
  mutate(`Free Flow of Information change` = `Free Flow of Information` - lag(`Free Flow of Information`)) %>%
  mutate(`Sound Business Environment change` = `Sound Business Environment` - lag(`Sound Business Environment`)) %>%
  mutate(`Well-Functioning Government change` = `Well-Functioning Government` - lag(`Well-Functioning Government`)) %>%
  mutate(`Low Levels of Corruption change` = `Low Levels of Corruption` - lag(`Low Levels of Corruption`)) %>%
  mutate(`Good Relations with Neighbours change` = `Good Relations with Neighbours` - lag(`Good Relations with Neighbours`)) %>%
  mutate(`High Levels of Human Capital change` = `High Levels of Human Capital` - lag(`High Levels of Human Capital`)) %>%
  mutate(`PPI Overall Score` = round(`PPI Overall Score`))


PPI_Sentence.df <- PPI_Sentence.df %>%
  dplyr::filter(year == max(year))








# Assume you have a data frame named df with columns 'year', 'country_name', 'change_in_peacefulness'

# Function to generate the text
generate_text <- function(row) {
  # Identify the column with the maximum value
  columns <- c("Acceptance of the Rights of Others change", "Equitable Distribution of Resources change", 
               "Free Flow of Information change", "Sound Business Environment change",
               "Well-Functioning Government change", "Low Levels of Corruption change",
               "Good Relations with Neighbours change", "High Levels of Human Capital change")
  max_col <- columns[which.max(row[columns])]
  
  # Special case handling
  if (max_col == "Acceptance of the Rights of Others change") {
    description <- paste("Acceptance of the Rights of Others")
  } else if (max_col == "Equitable Distribution of Resources change") {
    description <- paste("Equitable Distribution of Resources")
  } else if (max_col == "Free Flow of Information change") {
    description <- paste("Free Flow of Information")
  } else if (max_col == "Sound Business Environment change") {
    description <- paste("Sound Business Environment")
  } else if (max_col == "Well-Functioning Government change") {
    description <- paste("Well-Functioning Government")
  } else if (max_col == "Low Levels of Corruption change") {
    description <- paste("Low Levels of Corruption")
  } else if (max_col == "Good Relations with Neighbours change") {
    description <- paste("Good Relations with Neighbours")
  } else if (max_col == "High Levels of Human Capital change") {
    description <- paste("High Levels of Human Capital")
  } else {
    description <- max_col
  }
  
  # Construct the text
  text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["PPI Overall Score"], "in the POSITIVE PEACE REPORT.",
                "This represents a", ifelse(row["overall change"] > 0, "deterioration", "improvement"),
                "from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                "This was driven by a", 
                ifelse(
                  row[max_col] > 0, "deterioration in the", "improvement in the"), description, "Pillar.")
  
  return(text)
}


PPI_Sentence.df$text <- apply(PPI_Sentence.df, 1, generate_text)


