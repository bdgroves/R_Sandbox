# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(readr)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Snake Species Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:",
                  choices = NULL, # Will be populated in server
                  selected = NULL)
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load and process the data
  snake_data <- read_csv("C:/data/R_Projects/R_Sandbox/rattlesnakes_data_clean.csv")
  
  # Convert to simple features (sf) object
  snake_data_sf <- st_as_sf(snake_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Update the species selection input
  updateSelectInput(session, "species", 
                    choices = unique(snake_data_sf$name),
                    selected = unique(snake_data_sf$name)[1])
  
  # Render the map
  output$map <- renderLeaflet({
    selected_species <- input$species
    filtered_data <- snake_data_sf %>% filter(name == selected_species)
    
    # Check if filtered_data has any points
    if (nrow(filtered_data) > 0) {
      leaflet(data = filtered_data) %>%
        addTiles() %>%
        addCircleMarkers(
          ~longitude, ~latitude,
          radius = 3,           # Adjust the size of the dots
          color = "blue",       # Color of the dots
          fillOpacity = 0.7,    # Transparency of the fill color
          popup = ~name
        ) %>%
        setView(lng = mean(filtered_data$longitude, na.rm = TRUE), 
                lat = mean(filtered_data$latitude, na.rm = TRUE), 
                zoom = 4)
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -98.583333, lat = 39.833333, zoom = 4) %>%  # Default view if no data
        addPopup("No data available for the selected species")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
