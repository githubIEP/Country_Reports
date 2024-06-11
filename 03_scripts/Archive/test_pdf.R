# Load required libraries
library(grid)
library(gridExtra)
library(ggplot2)
library(png)
# # Dummy data frames
# data_frame1 <- data.frame(A = 1:3, B = 4:6)
# data_frame2 <- data.frame(A = 7:9, B = 10:12)

# Read PNG files
img1 <- readPNG("04_outputs/plots/pCHART_ACLED.png")
img2 <- readPNG("04_outputs/plots/pCHART_PPI.png")

# Convert images to grobs
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)


text_df <- GPI_Sentence.df %>%
  dplyr::select(c(`text`))

# Create table grob
table_grob1 <- tableGrob(Indicators.df)
table_grob2 <- tableGrob(text_df)

layout <- rbind(
  c(1,3),
  c(2,4)
)

# Create the PDF
pdf("04_outputs/Layout/output.pdf", width = 8.5, height = 11)

# Arrange the images and table in a grid
grid.arrange(table_grob1, table_grob2, g1, g2, layout_matrix = layout)

# Close the PDF device
dev.off()
















