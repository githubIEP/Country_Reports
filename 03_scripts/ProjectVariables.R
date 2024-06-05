#### Project Variables #####
install.packages("bannerCommenter")
library(bannerCommenter)



###################################################################
##  Creating general Variables: COUNTRY NAME & most recent year  ##
###################################################################

COUNTRY_NAME = "Burkina Faso"
LATEST_YEAR = 2023


### Read Files and data ###

#### Charts ######
CHART_UNIT = "cm"

# Chart Sizes
CHARTS <- list(
  small = c(width = 8.45, height = 10),
  medium = c(width = 12, height = 10),
  large = c(width = 17.6, height = 10)
)

# Map Sizes
MAPS <- list(
  small = c(width = 12, height = 8),
  medium = c(width = 14, height = 10),
  large = c(width = 28, height = 14)
)

# Chart Fonts
HEAVY_FONT = "Helvetica LT Pro" 
LIGHT_FONT = "Helvetica LT Pro Light" 

