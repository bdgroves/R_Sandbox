# Load required libraries and install if necessary
if (!require(spocc)) {
  install.packages('spocc', dependencies = TRUE)
  library(spocc)
}

# Additional required libraries
library(tidyverse)  # For data wrangling
library(sf)         # For handling spatial data
library(leaflet)    # For interactive maps

# Define a list of all rattlesnake species to query
species_list <- c('Crotalus adamanteus', 'Crotalus angelensis', 'Crotalus aquilus',
                  'Crotalus armstrongi', 'Crotalus atrox', 'Crotalus basiliscus',
                  'Crotalus campbelli', 'Crotalus catalinensis', 'Crotalus cerastes',
                  'Crotalus cerastes cerastes', 'Crotalus cerastes cercobombus', 
                  'Crotalus cerastes laterorepens', 'Crotalus culminatus', 
                  'Crotalus durissus', 'Crotalus durissus cumanensis', 
                  'Crotalus durissus durissus', 'Crotalus durissus marajoensis', 
                  'Crotalus durissus maricelae', 'Crotalus durissus ruruima', 
                  'Crotalus durissus terrificus', 'Crotalus durissus trigonicus', 
                  'Crotalus durissus unicolor', 'Crotalus durissus vegrandis', 
                  'Crotalus ehecatl', 'Crotalus enyo', 'Crotalus enyo cerralvensis', 
                  'Crotalus enyo enyo', 'Crotalus enyo furvus', 'Crotalus ericsmithi', 
                  'Crotalus estebanensis', 'Crotalus horridus', 'Crotalus intermedius', 
                  'Crotalus intermedius gloydi', 'Crotalus intermedius intermedius', 
                  'Crotalus intermedius omiltemanus', 'Crotalus lannomi', 
                  'Crotalus lepidus', 'Crotalus lepidus klauberi', 
                  'Crotalus lepidus lepidus', 'Crotalus lepidus maculosus', 
                  'Crotalus mictlantecuhtli', 'Crotalus mitchellii', 
                  'Crotalus mitchellii mitchellii', 'Crotalus mitchellii muertensis', 
                  'Crotalus molossus', 'Crotalus molossus molossus', 
                  'Crotalus molossus nigrescens', 'Crotalus molossus oaxacus', 
                  'Crotalus morulus', 'Crotalus oreganus', 'Crotalus oreganus abyssus', 
                  'Crotalus oreganus caliginis', 'Crotalus oreganus cerberus', 
                  'Crotalus oreganus concolor', 'Crotalus oreganus helleri', 
                  'Crotalus oreganus lutosus', 'Crotalus oreganus oreganus', 
                  'Crotalus ornatus', 'Crotalus polisi', 'Crotalus polystictus', 
                  'Crotalus pricei', 'Crotalus pricei miquihuanus', 
                  'Crotalus pricei pricei', 'Crotalus pusillus', 'Crotalus pyrrhus', 
                  'Crotalus ravus', 'Crotalus ravus brunneus', 'Crotalus ravus exigus', 
                  'Sistrurus ravus ravus', 'Crotalus ruber', 'Crotalus ruber exsul', 
                  'Crotalus ruber lorenzoensis', 'Crotalus ruber lucasensis', 
                  'Crotalus ruber ruber', 'Crotalus scutulatus', 
                  'Crotalus scutulatus salvini', 'Crotalus scutulatus scutulatus', 
                  'Crotalus simus', 'Crotalus stejnegeri', 'Crotalus stephensi', 
                  'Crotalus tancitarensis', 'Crotalus thalassoporus', 'Crotalus tigris', 
                  'Crotalus tlaloci', 'Crotalus tortugensis', 'Crotalus totonacus', 
                  'Crotalus transversus', 'Crotalus triseriatus', 'Crotalus tzabcan', 
                  'Crotalus viridis', 'Crotalus viridis nuntius', 'Crotalus viridis viridis', 
                  'Crotalus willardi', 'Crotalus willardi amabilis', 
                  'Crotalus willardi meridionalis', 'Crotalus willardi obscurus', 
                  'Crotalus willardi silus', 'Crotalus willardi willardi')

# Initialize an empty dataframe to hold all occurrences
all_rattlers <- data.frame()

# Loop over each species in the list and query their occurrence data
for (species in species_list) {
  message("Fetching data for species: ", species)
  
  tryCatch({
    # Fetch data from GBIF and iNaturalist for each species, limited to 500 records
    species_data <- occ(query = species, from = c('gbif', 'inat'), limit = 500)
    
    # Convert to dataframe
    species_df <- occ2df(species_data)
    
    # Ensure longitude and latitude are numeric and clean NAs
    species_df <- species_df %>%
      mutate_at(c('longitude', 'latitude'), as.numeric) %>%
      filter_at(vars(longitude, latitude), all_vars(!is.na(.)))
    
    # Append species data to the main dataframe
    all_rattlers <- bind_rows(all_rattlers, species_df)
  }, error = function(e) {
    message("Error fetching data for species: ", species, " - ", e$message)
  })
}

# Display the first few rows of the combined dataset
head(all_rattlers)

# Generate a color palette for the species based on the species names
species_colors <- colorFactor(topo.colors(length(unique(all_rattlers$name))), all_rattlers$name)

# Create an interactive leaflet map to display rattlesnake occurrences
leaflet(all_rattlers) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(~longitude, ~latitude, 
                   radius = 3,  # Size of the points
                   color = ~species_colors(name),  # Use the color palette
                   fillOpacity = 0.8,  # Transparency of the points
                   popup = ~paste("Species:", name, "<br>", 
                                  "Date:", date, "<br>", 
                                  "Source:", prov))  # Popup for each point with species info

# Save the data as CSV
write.csv(all_rattlers, "rattlesnakes_data.csv", row.names = FALSE)

# Convert to an 'sf' object for GeoJSON export
# First, ensure the dataframe has proper column names and types
all_rattlers_sf <- st_as_sf(all_rattlers, coords = c("longitude", "latitude"), crs = 4326)

# Save the data as GeoJSON
st_write(all_rattlers_sf, "rattlesnakes_data.geojson", driver = "GeoJSON")

# Load required libraries
library(dplyr)

# Summarize the count of occurrences for each species
species_summary <- all_rattlers %>%
  group_by(name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Display the summary
print(species_summary)

# Optionally, save the summary to a CSV file
write.csv(species_summary, "rattlesnake_species_summary.csv", row.names = FALSE)

