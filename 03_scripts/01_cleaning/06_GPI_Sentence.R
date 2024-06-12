
GPI_REGION <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(gpi_region == REGION) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c('geocode', 'value')) %>%
  arrange(value = value) %>%
  rename(`overall score` = value) %>%
  mutate(`Regional Rank` = rank(`overall score`)) %>%
  dplyr::select(`geocode`, `Regional Rank`) %>%
  subset(geocode == GEOCODE)



GPI_score <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`overall score` = value)



GPI_domain1 <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "ongoing conflict") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`ongoing conflict` = value)


GPI_domain2 <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "militarisation") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`militarisation` = value)


GPI_domain3 <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "safety and security") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup() %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c('geocode', 'year', 'value')) %>%
  rename(`safety and security` = value)






data_frames <- list(
  GPI_domain1, GPI_domain2, GPI_domain3, GPI_REGION
)

GPI_Sentence.df <- GPI_score

for(df in data_frames) {
  GPI_Sentence.df <- GPI_Sentence.df %>% left_join(df)
}




GPI_Sentence.df <- GPI_Sentence.df %>%
  mutate(`overall change` = `overall score` - lag(`overall score`)) %>%
  mutate(`ongoing conflict change` = `ongoing conflict` - lag(`ongoing conflict`)) %>%
  mutate(`safety and security change` = `safety and security` - lag(`safety and security`)) %>%
  mutate(`militarisation change` = `militarisation` - lag(`militarisation`)) %>%
  mutate(`overall score` = round(`overall score`))



GPI_Sentence.df <- GPI_Sentence.df %>%
  dplyr::filter(year == max(year))


# Function to generate the text
generate_text <- function(row) {
  # Check if "overall score" is not missing
  if (!is.na(row["overall change"])) {
    # Identify the column with the maximum value
    columns <- c("ongoing conflict change", "militarisation change", "safety and security change")
    max_col <- columns[which.max(row[columns])]
    min_col <- columns[which.min(row[columns])]
    
    # Special case handling
    if (max_col == "safety and security change") {
      description <- paste("safety and security change")
    } else if (max_col == "militarisation change") {
      description <- paste("militarisation")
    } else if (max_col == "ongoing conflict change") {
      description <- paste("ongoing conflict")
    } else {
      description <- max_col
    }
    
    if (min_col == "safety and security change") {
      description1 <- paste("safety and security change")
    } else if (min_col == "militarisation change") {
      description1 <- paste("militarisation")
    } else if (min_col == "ongoing conflict change") {
      description1 <- paste("ongoing conflict")
    } else {
      description1 <- min_col
    }
    
    # Construct the text
    if (row["overall change"] > 0) {
      text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["overall score"], "in the GLOBAL PEACE INDEX.",
                    "This represents a deterioration from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                    "This was driven by a deterioration in the", description, "domian")
    } else {
      text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["overall score"], "in the GLOBAL PEACE INDEX.",
                    "This represents an improvement from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                    "This was driven by an improvement in the", description1, "domain")
    }
    
    return(text)
  } else {
    return("Error: 'overall score' is missing.")
  }
}



# Apply the function to each row of the data frame
GPI_Sentence.df$text <- apply(GPI_Sentence.df, 1, generate_text)








