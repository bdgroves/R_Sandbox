# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)

rattlesnakes_data_clean <- read_csv("rattlesnakes_data_clean.csv")


# Rename 'name' column to 'scientific_name' for clarity
rattlesnakes_data_clean <- rattlesnakes_data_clean %>%
  rename(scientific_name = name)

# Define the User Interface
ui <- fluidPage(
  titlePanel("Rattlesnake Species Map"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select species
      selectInput("species", "Select Species:",
                  choices = unique(rattlesnakes_data_clean$scientific_name),
                  selected = unique(rattlesnakes_data_clean$scientific_name)[1])
    ),
    
    mainPanel(
      # Output map
      leafletOutput("map")
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data by selected species
  filtered_data <- reactive({
    rattlesnakes_data_clean %>%
      filter(scientific_name == input$species)
  })
  
  # Initialize the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(
        lng = mean(rattlesnakes_data_clean$longitude, na.rm = TRUE), 
        lat = mean(rattlesnakes_data_clean$latitude, na.rm = TRUE), 
        zoom = 5
      )
  })
  
  # Update map markers when a different species is selected
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        popup = ~scientific_name,
        radius = 3,
        color = "red",
        fillOpacity = 0.5
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
