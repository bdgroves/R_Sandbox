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
                  choices = NULL)  # To be populated in server
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
  snake_data_sf <- st_as_sf(snake_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Update the species selection input
  updateSelectInput(session, "species", 
                    choices = unique(snake_data_sf$name),
                    selected = unique(snake_data_sf$name)[1])
  
  # Render the map
  output$map <- renderLeaflet({
    selected_species <- input$species
    filtered_data <- snake_data_sf %>% filter(name == selected_species)
    
    # Create a convex hull polygon
    convex_hull <- st_convex_hull(st_union(filtered_data))
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = convex_hull, color = "blue", fillOpacity = 0.3, weight = 2) %>%
      addCircleMarkers(data = filtered_data, ~longitude, ~latitude, color = "blue", radius = 4, popup = ~name) %>%
      setView(lng = mean(filtered_data$longitude, na.rm = TRUE), 
              lat = mean(filtered_data$latitude, na.rm = TRUE), 
              zoom = 4)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
