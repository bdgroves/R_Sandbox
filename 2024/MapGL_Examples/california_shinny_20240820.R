# Load required libraries
library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(colourpicker)
library(tidyverse)
library(tidycensus)

# Load California census tracts data for median household income
ca_tracts <- get_acs(geography = "tract", 
                     variables = "B19013_001", # Median household income
                     state = "CA", 
                     geometry = TRUE) %>% 
  st_transform(4326) # Ensure the CRS is compatible with mapgl

# Define the UI
ui <- page_sidebar(
  title = "mapgl with Shiny",
  sidebar = sidebar(
    colourInput("color", "Select a color", value = "blue"),
    sliderInput("slider", "Show median income values above:",
                value = 50000, min = min(ca_tracts$estimate, na.rm = TRUE), 
                max = max(ca_tracts$estimate, na.rm = TRUE))
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
      fit_bounds(ca_tracts, animate = FALSE) |> 
      add_fill_layer(id = "ca_data",
                     source = ca_tracts,
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
