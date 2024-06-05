

GPI <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  dplyr::filter(geoname == COUNTRY_NAME) %>%
  dplyr::select(c('geoname', 'year', 'value')) %>%
  mutate(change = value - lag(value))

  




# Load required libraries
library(openxlsx)  # For writing to Excel

# Assume you have a data frame named df with columns 'year', 'country_name', 'change_in_peacefulness'

# Function to generate the text
generate_text <- function(row) {
  text <- paste("In", row["year"], ",", row["geoname"], "experienced a", 
                ifelse(row["change"] > 0, "increase", "decrease"), 
                "in peacefulness")
  return(text)
}

# Apply the function to each row of the data frame
GPI$text <- apply(GPI, 1, generate_text)

existing_wb <- loadWorkbook("04_outputs/country_report.xlsx")

# Write the generated text into a single cell
writeData(existing_wb, sheet = "Sheet1", GPI$text, startCol = 1, startRow = 4, colNames = FALSE)

# Save the changes
saveWorkbook(existing_wb, file = "04_outputs/country_report.xlsx", overwrite = TRUE)
