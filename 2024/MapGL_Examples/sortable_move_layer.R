# You'll need the dev version of mapgl
# remotes::install_github("walkerke/mapgl")
library(shiny)
library(bslib)
library(mapgl)
library(tigris)
library(sf)
library(dplyr)
library(sortable)

options(tigris_use_cache = TRUE)

rds <- roads("WA", "Pierce")
tr <- tracts("WA", "Pierce", cb = TRUE)

set.seed(123)
pois <- st_sample(st_as_sfc(st_bbox(tr)), 100) %>%
  st_sf() %>%
  mutate(id = row_number())

layer_ids <- c("Points of Interest", "Local roads", "Census tracts")

ui <- page_sidebar(
  title = "Tarrant County Map Layers",
  sidebar = sidebar(
    width = 350,
    tags$p("Drag to reorder map layers:"),
    rank_list(
      text = NULL,
      labels = layer_ids,
      input_id = "layer_order"
    )
  ),
  maplibreOutput("map", height = "calc(100vh - 56px)")
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    maplibre() %>%
      fit_bounds(rds) %>%
      add_fill_layer(
        id = "Census tracts",
        source = tr,
        fill_color = "white",
        fill_outline_color = "black"
      ) %>%
      add_line_layer(
        id = "Local roads",
        source = rds,
        line_color = "#495057",
        line_width = 1
      ) %>%
      add_circle_layer(
        id = "Points of Interest",
        source = pois,
        circle_color = "#007bff",
        circle_radius = 5,
        circle_stroke_width = 1,
        circle_stroke_color = "#ffffff"
      ) 
  })
  
  observeEvent(input$layer_order, {
    req(input$layer_order)
    new_order <- input$layer_order
    
    maplibre_proxy("map") %>%
      move_layer(layer_id = new_order[1], before_id = NULL) %>%
      move_layer(layer_id = new_order[2], before_id = new_order[1]) %>%
      move_layer(layer_id = new_order[3], before_id = new_order[2])
  })
}

shinyApp(ui, server)