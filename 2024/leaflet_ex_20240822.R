library(shiny)
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("US County Population Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", 
                  choices = state.name, 
                  selected = "California"),
      helpText("Select a state to view county populations.")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive expression to fetch population data based on selected state
  state_data <- reactive({
    get_acs(
      geography = "county",
      variables = "B01003_001",  # Total population variable
      state = input$state,
      year = 2020,
      geometry = TRUE
    ) %>%
      rename(population = estimate, county_name = NAME) %>%
      st_transform(crs = 4326)
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(state_data()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "blue",
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(county_name, "Population:", format(population, big.mark = ",")),
        labelOptions = labelOptions(
          textsize = "15px",
          direction = "auto"
        )
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
