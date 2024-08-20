# Load required libraries
library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(colourpicker)
library(tidyverse)
library(tidycensus)

ca_counties <- get_acs(geography = "county", 
                       variables = "B01003_001", # Total population
                       state = "CA", 
                       geometry = TRUE) %>% 
  st_transform(4326) # Ensure the CRS is compatible with mapgl

# Define the UI
ui <- page_sidebar(
  title = "mapgl with Shiny",
  sidebar = sidebar(
    colourInput("color", "Select a color", value = "blue"),
    sliderInput("slider", "Show population values above:",
                value = 50000, min = 50000, max = max(ca_counties$estimate, na.rm = TRUE))
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

# Define the server logic
server <- function(input, output, session) {
  output$map <- renderMaplibre({
    maplibre(style = carto_style("positron")) |> 
      fit_bounds(ca_counties, animate = FALSE) |> 
      add_fill_layer(id = "ca_data",
                     source = ca_counties,
                     fill_color = "blue",
                     fill_opacity = 0.5)
  })
  
  observeEvent(input$color, {
    maplibre_proxy("map") |>
      set_paint_property("ca_data", "fill-color", input$color)
  })
  
  observeEvent(input$slider, {
    maplibre_proxy("map") |> 
      set_filter("ca_data", 
                 list(">=", get_column("estimate"), input$slider))
  })
}

# Run the Shiny app
shinyApp(ui, server)
                     
                     