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
  
  if (xaxis == "Include") {
    plot <- plot + theme(axis.title.x = element_text())
  } else {
    plot <- plot + theme(axis.title.x = element_blank())
  }
  
  if (yaxis == "Include") {
    plot <- plot + theme(axis.title.y = element_text())
  } else {
    plot <- plot + theme(axis.title.y = element_blank())
  }
  
  if (xgridline == "Include") {
    plot <- plot + theme(panel.grid.major.x = element_line())
  } else {
    plot <- plot + theme(panel.grid.major.x = element_blank())
  }
  
  if (ygridline == "Include") {
    plot <- plot + theme(panel.grid.major.y = element_line())
  } else {
    plot <- plot + theme(panel.grid.major.y = element_blank())
  }
  
  if(chart_info['position'] == "Normal") {
    plot <- plot + theme(legend.position = "right")
  } else {
    plot <- plot + theme(legend.position = "bottom")
  }
  
  return(plot)
}



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
