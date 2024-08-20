library(tidycensus)
library(mapgl)

ca_age <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "CA",
  year = 2022,
  geometry = TRUE
)

ca_map <- mapboxgl(mapbox_style("light"),
                   bounds = ca_age) 

ca_map |> 
  add_fill_layer(
    id = "ca_tracts",
    source = fl_age,
    fill_color = interpolate(
      column = "estimate",
      values = c(20, 80),
      stops = c("lightblue", "darkblue"),
      na_color = "lightgrey"
    ),
    fill_opacity = 0.5
  ) |> 
  add_legend(
    "Median age in California",
    values = c(20, 80),
    colors = c("lightblue", "darkblue")
  )


brewer_pal <- RColorBrewer::brewer.pal(5, "RdYlBu")

ca_map |> 
  add_fill_layer(
    id = "fl_tracts",
    source = ca_age,
    fill_color = step_expr(
      column = "estimate",
      base = brewer_pal[1],
      stops = brewer_pal[2:5],
      values = seq(25, 70, 15),
      na_color = "white"
    ),
    fill_opacity = 0.5
  ) |> 
  add_legend(
    "Median age in California",
    values = c(
      "Under 25",
      "25-40",
      "40-55",
      "55-70",
      "Above 70"
    ),
    colors = brewer_pal,
    type = "categorical"
  )


ca_age$popup <- glue::glue(
  "<strong>GEOID: </strong>{ca_age$GEOID}<br><strong>Median age: </strong>{ca_age$estimate}"
)

ca_map |> 
  add_fill_layer(
    id = "ca_tracts",
    source = fl_age,
    fill_color = interpolate(
      column = "estimate",
      values = c(20, 80),
      stops = c("lightblue", "darkblue"),
      na_color = "lightgrey"
    ),
    fill_opacity = 0.5,
    popup = "popup",
    tooltip = "estimate",
    hover_options = list(
      fill_color = "yellow",
      fill_opacity = 1
    )
  ) |> 
  add_legend(
    "Median age in California",
    values = c(20, 80),
    colors = c("lightblue", "darkblue")
  )