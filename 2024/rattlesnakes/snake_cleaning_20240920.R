# Load necessary libraries
library(sf)
library(dplyr)

# Read the shapefile
shapefile <- st_read("C:/data/R_Projects/R_Sandbox/2024/rattlesnakes/rattlesnake_ranges_reptile_db_20240920.shp")

# Adding new columns with NA as default values
new_columns <- c("common_name", "scientific_name", "population_estimate", 
                 "habitat_type", "conservation_status", "threats", 
                 "observation_date", "observation_source", 
                 "protected_status", "management_actions", 
                 "elevation_range", "climate_zone", "diet", 
                 "breeding_season", "migration_patterns", 
                 "genetic_variability", "research_references", 
                 "management_plans", "last_updated", "notes")

for (col in new_columns) {
  shapefile[[col]] <- NA
}

# Reorganize the dataframe
shapefile <- shapefile %>%
  select(
    Species,
    Subspecies,
    all_of(new_columns),  # This will select all newly added columns
    area_sq_mi,
    area_sq_km,
    geometry
  )

# Save the updated shapefile
st_write(shapefile, "C:/data/R_Projects/R_Sandbox/2024/rattlesnakes/updated_rattlesnake_ranges.shp")
