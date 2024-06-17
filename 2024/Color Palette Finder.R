# Load necessary libraries
library(ggstream)
library(ggplot2)
library(paletteer)

# Load the data
df <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/stream.csv")

# Create the streamgraph
ggplot(df, aes(x = year, y = value, fill = name)) +
  geom_stream(color = "black") +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Streamgraph of the datastream dataset", x = "Year", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5))