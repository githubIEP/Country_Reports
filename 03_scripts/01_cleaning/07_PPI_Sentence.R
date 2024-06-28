#################################################################
##                   PPI Statement Structure                   ##
#################################################################

# This is similar to the GPI statement. We create a paragraph for the country's PPI position and it reads like this:

# "In 2022, Burkina Faso had an overall score of 4 in the POSITIVE PEACE REPORT. This represents a deterioration from the previous year. 
#  It is currently ranked 17th in the region. This was driven by a deterioration in the Well Functioning Government Pillar. In the last five years
#  Burkina Faso has seen an overall deterioration in Positive Peace."

# In order to generate this statement we need the following:

# 1. The PPI rank in the country's region
# 2. The overall PPI score
# 3. All 8 Pillars of the PPI

# To start with, we pull the PPI score from the data base and filter by the country's region.
# The next step is to use the rank function and rank all the scores from lowest to highest. 
# The next block of codes would then pull the score and pillars from the database.




PPI_REGION <- iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  dplyr::filter(gpi_region == REGION) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(c('geocode', 'value')) %>%
  arrange(value = value) %>%
  rename(`overall score` = value) %>%
  mutate(`Regional Rank` = rank(`overall score`)) %>%
  dplyr::select(`geocode`, `Regional Rank`) %>%
  subset(geocode == GEOCODE)




PPI_PILLAR1.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Equitable Distribution of Resources") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Equitable Distribution of Resources` = `value`)



PPI_PILLAR2.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Acceptance of the Rights of Others") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Acceptance of the Rights of Others` = `value`)


PPI_PILLAR3.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Free Flow of Information") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Free Flow of Information` = `value`)



PPI_PILLAR4.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Sound Business Environment") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Sound Business Environment` = `value`)



PPI_PILLAR5.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Well-Functioning Government") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Well-Functioning Government` = `value`)



PPI_PILLAR6.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Low Levels of Corruption") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Low Levels of Corruption` = `value`)



PPI_PILLAR7.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "Good Relations with Neighbours") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`Good Relations with Neighbours` = `value`)



PPI_PILLAR8.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "High Levels of Human Capital") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`High Levels of Human Capital` = `value`)



PPI_OVERALL.df <-  iepg_search("PPI 2023 Report") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()  %>%
  dplyr::filter(variablename == "PPI Overall Score") %>%
  dplyr::filter(geocode == GEOCODE) %>%
  dplyr::select(c(`geocode`, `year`, `value`)) %>%
  rename(`PPI Overall Score` = `value`)




# This loop then combines all the above data frames into one single data frame 

data_frames <- list(
  PPI_REGION, PPI_PILLAR1.df, PPI_PILLAR2.df, PPI_PILLAR3.df, PPI_PILLAR4.df,
  PPI_PILLAR5.df,PPI_PILLAR6.df, PPI_PILLAR7.df, PPI_PILLAR8.df
)

PPI_Sentence.df <- PPI_OVERALL.df

for(df in data_frames) {
  PPI_Sentence.df <- PPI_Sentence.df %>% left_join(df)
}



# Just like the previous script, this chunk of code creates a change column for all the domains and overall score
# It also creates the five year change column to calculate the change in the overall score in five years.
# And finally the code rounds the overall PPI score. 


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
  mutate(`five year change` = (`PPI Overall Score` - lag(`PPI Overall Score`, 5))) %>%
  mutate(`PPI Overall Score` = round(`PPI Overall Score`))


# This code then takes the latest year. 

PPI_Sentence.df <- PPI_Sentence.df %>%
  dplyr::filter(year == max(year))


# Function to generate the text

# This next block of code, creates a function to create the PPI statement
# The way in which this function is structured such that, if the changes in the overall score is greater than zero,
# the function will select first paragraph and if the change is not greater than zero, the function will select the second paragraph.

# The reason for this is because if there is an improvement in the PPI score from the previous year, we want to select the pillar that drove its improvement.
# Likewise if there is a deterioration in the overall score from the previous year, we want to select the pillar that drove the deterioration.


# In order to create the paragraph, we start by creating a list of columns which include the change in pillar columns.
# we have two categories that look at the max value in the pillar change columns and the min value in the pillar change column. 
# We create a definition for each column whereby, the pillar change column is renamed to simply the name of the pillar.

# This is done for both the max and min values of the pillar changes.



generate_text <- function(row) {
  # Check if "overall score" is not missing
  if (!is.na(row["overall change"])) {
    # Identify the column with the maximum value
    columns <- c("Acceptance of the Rights of Others change", "Equitable Distribution of Resources change", 
                 "Free Flow of Information change", "Sound Business Environment change",
                 "Well-Functioning Government change", "Low Levels of Corruption change",
                 "Good Relations with Neighbours change", "High Levels of Human Capital change")
    max_col <- columns[which.max(row[columns])]
    min_col <- columns[which.min(row[columns])]
    
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
    
    if (min_col == "Acceptance of the Rights of Others change") {
      description1 <- paste("Acceptance of the Rights of Others")
    } else if (min_col == "Equitable Distribution of Resources change") {
      description1 <- paste("Equitable Distribution of Resources")
    } else if (min_col == "Free Flow of Information change") {
      description1 <- paste("Free Flow of Information")
    } else if (min_col == "Sound Business Environment change") {
      description1 <- paste("Sound Business Environment")
    } else if (min_col == "Well-Functioning Government change") {
      description1 <- paste("Well-Functioning Government")
    } else if (min_col == "Low Levels of Corruption change") {
      description1 <- paste("Low Levels of Corruption")
    } else if (min_col == "Good Relations with Neighbours change") {
      description1 <- paste("Good Relations with Neighbours")
    } else if (min_col == "High Levels of Human Capital change") {
      description1 <- paste("High Levels of Human Capital")
    } else {
      description1 <- min_col
    }
    # Construct the text
    if (row["overall change"] > 0) {
      text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["PPI Overall Score"], "in the POSITIVE PEACE REPORT.",
                    "This represents a deterioration from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                    "This was driven by a deterioration in the", description, "Pillar. In the last five years", COUNTRY_NAME, "has seen an overall",
                    ifelse(row["five year change"] > 0, "deterioration", "improvement"), "in Positive Peace.")
    } else {
      text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["PPI Overall Score"], "in the POSITIVE PEACE REPORT.",
                    "This represents an improvement from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                    "This was driven by an improvement in the", description1, "Pillar. In the last five years", COUNTRY_NAME, "has seen an overall",
                    ifelse(row["five year change"] > 0, "deterioration", "improvement"), "in Positive Peace.")
    }
    
    return(text)
  } else {
    return("Error: 'overall score' is missing.")
  }
}

# Apply the function to each row of the data frame

PPI_Sentence.df$text <- apply(PPI_Sentence.df, 1, generate_text)


