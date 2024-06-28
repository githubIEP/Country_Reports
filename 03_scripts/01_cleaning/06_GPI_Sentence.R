#################################################################
##                   GPI Statement Structure                   ##
#################################################################

# The main purpose of this script is to create a paragraph about the country's GPI position that would be printed on the country report excel sheet.
# In this instance the sentence would read like this:

# "In 2023, Burkina Faso had an overall score of 3 in the GLOBAL PEACE INDEX. This represents a deterioration from the previous year.
# "It is currently ranked 38th in the region. This was driven by a deterioration in the safety and security domain. In the last five years,
# "Burkina Faso, has seen a deterioration in Global Peace."


# In order to construct this sentence we would need the following
# 1. The overall GPI score
# 2. The regional GPI rank
# 3. The 3 GPI domains

# To start with we pull the overall GPI score and filter by region and use the rank function to rank the score from lowest to highest
# We then pull the GPI score and domains from the database and filter by the country's geocode.



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



# we perform a loop that will combine the all the above data frames into one single data frame called GPI_Sentence_df

data_frames <- list(
  GPI_domain1, GPI_domain2, GPI_domain3, GPI_REGION
)

GPI_Sentence.df <- GPI_score

for(df in data_frames) {
  GPI_Sentence.df <- GPI_Sentence.df %>% left_join(df)
}

# In order to get to determine whether GPI scores and domains have improved or deteriorated, we need to work the 
# change in domains and scores.
# This chunk of code then create new 'change' columns.
# This calculates the the raw change in scores and domains from the previous year.
# Another column that is created is the five year change column. 
# This looks at the raw change in the overall score by taking the difference from the most recent year to the value five years ago.
# Finally in order to print the raw score the code then just rounds the score.


GPI_Sentence.df <- GPI_Sentence.df %>%
  mutate(`overall change` = `overall score` - lag(`overall score`)) %>%
  mutate(`ongoing conflict change` = `ongoing conflict` - lag(`ongoing conflict`)) %>%
  mutate(`safety and security change` = `safety and security` - lag(`safety and security`)) %>%
  mutate(`militarisation change` = `militarisation` - lag(`militarisation`)) %>%
  mutate(`five year change` = `overall score` - lag(`overall score`, 5)) %>%
  mutate(`overall score` = round(`overall score`))


# We then filter this data frame to the most recent year. 
# We now have all the elements for the GPI statement. 

GPI_Sentence.df <- GPI_Sentence.df %>%
  dplyr::filter(year == max(year))



# This next block of code, creates a function to create the GPI statement
# The way in which this function is structured is such that, if the change overall score is greater than zero,
# the function will select first paragraph and if the change is not greater than zero, the function will select the second paragraph.

# The reason for this is because if there is an improvement in the GPI score from the previous year, we want to select the domain that drove its improvement.
# Likewise if there is a deterioration in the overall score from the previous year, we want to select the domain that drove the deterioration.


# In order to create the paragraph, we start by creating a list of columns which include the change in domain columns.
# we have two categories that look at the max value in the domain change columns and the min value in the domain change column. 
# We create a definition for each column whereby, if the domain change column is renamed to simply the name of the domain.

# This is done for both the max and min values of the domain changes.




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
      description <- paste("safety and security")
    } else if (max_col == "militarisation change") {
      description <- paste("militarisation")
    } else if (max_col == "ongoing conflict change") {
      description <- paste("ongoing conflict")
    } else {
      description <- max_col
    }
    
    if (min_col == "safety and security change") {
      description1 <- paste("safety and security")
    } else if (min_col == "militarisation change") {
      description1 <- paste("militarisation")
    } else if (min_col == "ongoing conflict change") {
      description1 <- paste("ongoing conflict")
    } else {
      description1 <- min_col
    }
    
    # Construct the text
    # This is the structure of the GPI statement, if the overall change is positive, the first statement is selected and if it is negative the second statment is selected.
    # If there is an issue with the code, it will print the error message. 
    
    if (row["overall change"] > 0) {
      text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["overall score"], "in the GLOBAL PEACE INDEX.",
                    "This represents a deterioration from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                    "This was driven by a deterioration in the", description, "domian. In the last five years,", COUNTRY_NAME, ", has seen an overall",
                    ifelse(row["five year change"] > 0, "deterioration", "improvement"), "in Global Peace.")
    } else {
      text <- paste("In", row["year"], ",", COUNTRY_NAME, "had an overall score of", row["overall score"], "in the GLOBAL PEACE INDEX.",
                    "This represents an improvement from the previous year. It is currently ranked", row["Regional Rank"], "th in the region.",  
                    "This was driven by an improvement in the", description1, "domain. In the last five years", COUNTRY_NAME, ", has seen an overall",
                    ifelse(row["five year change"] > 0, "deterioration", "improvement"), "in Global Peace.")
    }
    
    return(text)
  } else {
    return("Error: 'overall score' is missing.")
  }
}


# Apply the function to each row of the data frame
GPI_Sentence.df$text <- apply(GPI_Sentence.df, 1, generate_text)








