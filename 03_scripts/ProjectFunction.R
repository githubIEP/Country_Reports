# -------------------------------------------------------------------------------------------------------------------------------

#################################################################
##             Function to load necessary packages             ##
#################################################################

# This function is defined to load the packages necessary for this project


f_LibraryLoader <- function(...) {
  args <- substitute(list(...))[-1] # Capture the unquoted arguments
  package_names <- sapply(args, function(arg) {
    if (is.character(arg)) {
      return(arg) # Return the argument if it's a string
    } else {
      return(as.character(arg)) # Convert to string if it's a symbol
    }
  })
  
  for (package in package_names) {
    if (!package %in% rownames(installed.packages())) {
      stop(paste("Package not installed:", package))
    }
    
    if (!package %in% .packages()) {
      library(package, character.only = TRUE)
    }
  }
}


# This function loads the packages

f_LibraryLoader(tidyverse, 
                rlang,
                extrafont,
                dplyr,
                openxlsx,
                iepg,
                stringr,
                iepsqlite)

# -------------------------------------------------------------------------------------------------------------------------------

##################################################################
##                Function to Create Excel Sheet                ##
##################################################################

# This function creates the excel file where all the rankins, statements and tables would be printed.

write_selected_columns_to_excel <- function(data, columns, file_name) {
  wb <- createWorkbook()
  
  addWorksheet(wb, "Sheet1")
  
  if (!all(columns %in% colnames(data))) {
    stop("Some columns are not in the data frame")
  }
  
  writeData(wb, "Sheet1", data[, columns, drop = FALSE])
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
}


# -------------------------------------------------------------------------------------------------------------------------------

#################################################################
##                  Theme for Charts and Maps                  ##
#################################################################

# This theme function would apply a theme based on the plot description.
# This theme specifies the axis, titles, grid lines, labels and legends based on the type of plot, wheter it is a 'chart' or 'map'


f_ThemeTraining <- function(plot, chart_info, plottitle = "include", xaxis = "Include", yaxis = "Include", xgridline = "Include", ygridline = "Include") {
  plot <- plot + theme_minimal() +
    labs(
      title = ifelse(plottitle == "include", chart_info['title'], ""),
      subtitle = chart_info['sheet'],
      caption = chart_info['source'],
      x = chart_info['xtext'],
      y = chart_info['ytext']
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0)
    )
  
  # x-axis title
  if (xaxis == "Include") {
    plot <- plot + theme(axis.title.x = element_text())
  } else {
    plot <- plot + theme(axis.title.x = element_blank())
  }
  
  # y-axis title
  if (yaxis == "Include") {
    plot <- plot + theme(axis.title.y = element_text())
  } else {
    plot <- plot + theme(axis.title.y = element_blank())
  }
  
  # x-axis gridline
  if (xgridline == "Include") {
    plot <- plot + theme(panel.grid.major.x = element_line())
  } else {
    plot <- plot + theme(panel.grid.major.x = element_blank())
  }
  
  # y-axis gridline
  if (ygridline == "Include") {
    plot <- plot + theme(panel.grid.major.y = element_line())
  } else {
    plot <- plot + theme(panel.grid.major.y = element_blank())
  }
  
  # Always remove axis text and ticks if the type is "Map"
  if (chart_info['type'] == "Map") {
    plot <- plot + theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  # Set legend position
  if(chart_info['position'] == "Normal") {
    plot <- plot + theme(legend.position = "right")
  } else {
    plot <- plot + theme(legend.position = "bottom")
  }
  
  return(plot)
}


# -------------------------------------------------------------------------------------------------------------------------------

####################################################################
##  Function to save plots to a location and save them as a png   ##
####################################################################


save_plots_as_png <- function(plot_names, dest_path = "04_outputs/plots") {
  if (!dir.exists(dest_path)) {
    dir.create(dest_path)
  }
  
  for (i in seq_along(plot_names)) {
    plot_object <- get(plot_names[i])
    
    file_name <- file.path(dest_path, paste0(plot_names[i], ".png"))
    
    ggsave(file_name, plot_object, width = 8, height = 6, dpi = 300)
    
    cat("Plot", plot_names[i], "saved as", file_name, "\n")
  }
}

# -------------------------------------------------------------------------------------------------------------------------------

#################################################################
##                   Function to pull Region                   ##
#################################################################

# Creating function to pull region from the geocode.
# We first pull the geocode from the database. 
# The code defines a function where a new column called gpi_region is created to pull the correspomding region based on the defined geocode. 

df <- iepg_search("GPI 2023 Report") %>%
  dplyr::filter(variablename == "overall score") %>%
  pull(muid) %>%
  iepg_get() %>%
  ungroup()


get_region <- function(geocode) {
  region <- df$gpi_region[df$geocode == GEOCODE]
  if (length(region) == 0) {
    return("Unknown")
  } else {
    return(region)
  }
}


# -------------------------------------------------------------------------------------------------------------------------------

##################################################################
##        Function that creates a text for the PPI chart        ##
##################################################################

# There are two functions created to generate a statement in the PPI chart
# The Statement will read like this:

# "Since 2013, 4 out of the 8 Pillars improved and the other 4 deteriorated. The Overall PPI deteriorated by 2% since 2013."

# To generate the first half of this sentence, we need to run a function that would scan the PPI data frame.
# The code then categorizes the percentage change in PPI data frame 
# The changes are then counted for both positive and negative changes.
# The positive changes are called positive_count and negative changes are called negative_count.
# The reason for this is so that the statement can be auto generated for any country and can paste the pillars that have deteriorated and improved.



count_negative_values <- function(df, column_name, exclude_row) {

    if (!is.character(column_name)) {
    stop("column_name must be a string.")
  }
  
  
  if (!column_name %in% names(df)) {
    stop("The specified column does not exist in the data frame.")
  }
  
  filtered_df <- df[df$variablename != exclude_row, ]

  column_values <- filtered_df[[column_name]]
  
  negative_count <- sum(column_values < 0, na.rm = TRUE)
  
  total_count <- length(column_values)
  
  positive_count <- total_count - negative_count
  
  summary_statement <- paste(
    "Since 2013,",
    negative_count,
    "out of the",
    total_count,
    "pillars improved and the other",
    positive_count,
    "deteriorated."
  )
  
  return(summary_statement)
}


# This is the second function which completes the second half of the above statement. 
# We want to know by how much the overall PPI improved or deteriorated.
# To do this, the code selects the change in overall PPI from the PPI data frame multiplies it with 100 to get the percentage value
# Then the code pastes the second half of the sentence
# The code checks the percentage value and if the percentage value is positive, it would paste "deteriorated' and "improved" if the percentage value is negative.
# It would the print the absolute value of the percentage so that the printed value would be strictly positive.

get_overall_score <- function(df, variablename, column_name) {
  if (!is.character(column_name)) {
    stop("column_name must be a string.")
  }
  
  overall_row <- df[df$variablename == variablename, ]
  
  if (nrow(overall_row) == 0) {
    stop("The specified row does not exist in the data frame.")
  }
  if (!column_name %in% names(overall_row)) {
    stop("The specified column does not exist in the data frame.")
  }
  
  percentage_value <- round(overall_row[[column_name]] * 100)
  
  overall_score_statement <- paste("The overall PPI", ifelse(percentage_value > 0, "deteriorated", "improved"), "by", abs(percentage_value), "% since 2013.")
  
  return(overall_score_statement)
}


# -------------------------------------------------------------------------------------------------------------------------------

#################################################################
##                         ACLED Title                         ##
#################################################################

# Function to generate the title for the ACLED chart.
# This function would check the Acled data frame and select the last value in the data frame and paste it in the title



generate_title <- function(df, value_column) {
  latest_value <- tail(df[[value_column]], n = 1)
  
  formatted_value <- format(latest_value, big.mark = ",")
  
  title_string <- paste(formatted_value, "deaths from terrorism")
  
  return(title_string)
}


# -------------------------------------------------------------------------------------------------------------------------------

#################################################################
##                        Title for Map                        ##
#################################################################

# This function checks the minimum date in the data frame and print the earliest date in the statement.

generate_title_map <- function(df, min_year) {
  
  formatted_value <- format(min_year)
  
  title_string <- paste("Increase in Battle Fatalities since", formatted_value)
  
  return(title_string)
}


# -------------------------------------------------------------------------------------------------------------------------------


##################################################################
##                    Colour Palette for Map                    ##
##################################################################

# This function is there to create a colour palette for the map 
# This will be applied to the map in the Charts and Tables script

category_to_color <- function(category) {
  # Create a named vector for mapping categories to colors
  color_map <- c(
    "Decrease in fatalities" = "green",
    "No Increase in fatalities" = "lightgreen",
    "Increase in fatalities of less than 50" = "pink",
    "Increase in fatalities between 50 & 100" = "maroon",
    "Increase in fatalities between 100 & 200" = "red",
    "Increase in fatalities over 200" = "darkred"
  )
  
  # Match the category with the color map
  colors <- color_map[category]
  
  return(colors) 
}


# -------------------------------------------------------------------------------------------------------------------------------


#################################################################
##                          ETR Bands                          ##
#################################################################

# For each of the ETR domains, we want to assign a risk level, since this will be used to construct the ETR Statement that would appear in the final excel sheet. 
# For each score it would assign either "Very Low risk", "low risk", "medium risk", "high risk" and "severe risk". 

add_food_insecurity_band <- function(df) {
  df <- df %>%
    mutate(`Food_Insecurity` = case_when(
      `Food Insecurity` == 1 ~ "very low risk",
      `Food Insecurity` == 2 ~ "low risk",
      `Food Insecurity` == 3 ~ "medium risk",
      `Food Insecurity` == 4 ~ "high risk",
      `Food Insecurity` == 5 ~ "severe risk",
      TRUE ~ NA_character_  # Handle any unexpected values
    ))
  return(df)
}


add_water_risk_band <- function(df) {
  df <- df %>%
    mutate(`Water_Risk` = case_when(
      `Water Risk` == 1 ~ "very low risk",
      `Water Risk` == 2 ~ "low risk",
      `Water Risk` == 3 ~ "medium risk",
      `Water Risk` == 4 ~ "high risk",
      `Water Risk` == 5 ~ "severe risk",
      TRUE ~ NA_character_  # Handle any unexpected values
    ))
  return(df)
}

add_natural_hazard_exposure_band <- function(df) {
  df <- df %>%
    mutate(`Natural_Hazard_Exposure` = case_when(
      `Natural Hazard Exposure` == 1 ~ "very low risk",
      `Natural Hazard Exposure` == 2 ~ "low risk",
      `Natural Hazard Exposure` == 3 ~ "medium risk",
      `Natural Hazard Exposure` == 4 ~ "high risk",
      `Natural Hazard Exposure` == 5 ~ "severe risk",
      TRUE ~ NA_character_  # Handle any unexpected values
    ))
  return(df)
}


add_demographic_pressure_band <- function(df) {
  df <- df %>%
    mutate(`Demographic_pressure` = case_when(
      `Demographic Pressure` == 1 ~ "very low risk",
      `Demographic Pressure` == 2 ~ "low risk",
      `Demographic Pressure` == 3 ~ "medium risk",
      `Demographic Pressure` == 4 ~ "high risk",
      `Demographic Pressure` == 5 ~ "severe risk",
      TRUE ~ NA_character_  # Handle any unexpected values
    ))
  return(df)
}


# -------------------------------------------------------------------------------------------------------------------------------

##################################################################
##                    Calculating Risk Level                    ##
##################################################################

# This function is checks the data frame for the defined column and calculates the risk level by taking the average of the column values
# After this it creates a band, where values 1 to 5 are assigned "Low Risk" to "Very High Risk".

calculate_risk_levels <- function(df, col1, col2, col3) {
  df$composite_index <- rowMeans(df[, c(col1, col2, col3)], na.rm = TRUE)
  
  # Categorize the composite index into risk levels
  df$risk_level <- cut(
    df$composite_index,
    breaks = c(-Inf, 2, 3, 4, 5),
    labels = c("Low Risk", "Medium Risk", "High Risk", "Very High Risk"),
    right = FALSE
  )
  
  return(df)
}
