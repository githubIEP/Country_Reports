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




f_LibraryLoader(tidyverse, 
                rlang,
                extrafont,
                dplyr,
                openxlsx,
                iepg,
                stringr)




write_selected_columns_to_excel <- function(data, columns, file_name) {
  wb <- createWorkbook()
  
  addWorksheet(wb, "Sheet1")
  
  if (!all(columns %in% colnames(data))) {
    stop("Some columns are not in the data frame")
  }
  
  writeData(wb, "Sheet1", data[, columns, drop = FALSE])
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
}



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

# Define the save_plots_as_png function
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



### Creating function to pull region from the geocode ============================================

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


# Creating ETR bands ================================================================================


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



# ETR Colour function ===============================================================================

category_to_color <- function(category) {
  # Create a named vector for mapping categories to colors
  color_map <- c(
    "very low risk" = "darkgreen",
    "low risk" = "green",
    "medium" = "yellow",
    "high risk" = "orange",
    "severe risk" = "red"
  )
  
  # Match the category with the color map
  colors <- color_map[category]
  
  return(colors)
}

