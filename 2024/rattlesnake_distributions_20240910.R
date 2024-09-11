# Load required libraries and install if necessary
if (!require(spocc)) {
  install.packages('spocc', dependencies = TRUE)
  library(spocc)
}

# Additional required libraries
library(tidyverse)  # For data wrangling
library(sf)         # For handling spatial data
library('rnaturalearth')  # For getting map data
library(tigris)     # For geographical boundaries in the US
library(leaflet)    # For interactive maps

# Query for occurrences of rattlesnakes (genus Crotalus) in the USA
# Using spocc to gather data from GBIF (Global Biodiversity Information Facility)
# 'query' parameter specifies the genus 'Crotalus' (rattlesnakes)
# 'from' specifies the data sources (GBIF in this case)
data <- occ(query = 'Crotalus', from = c('gbif'))

# Display the fetched data
data

# Convert occurrence data into a dataframe for easy manipulation
rattler_df <- occ2df(data)

# Querying specifically for the Western Diamondback Rattlesnake (Crotalus atrox)
# from GBIF and iNaturalist, limiting to 500 records within 2020
rattler <- spocc::occ(query = 'Crotalus atrox', 
                      from = c('gbif', 'inat'), 
                      limit = 500,
                      date = c('2020-01-01', '2020-12-31'))

# Convert to dataframe
rattler <- occ2df(rattler)

# Convert 'longitude' and 'latitude' columns to numeric to ensure proper mapping
rattler <- rattler %>%
  mutate_at(c('longitude', 'latitude'), as.numeric)

# Remove rows with missing longitude or latitude values
rattler <- rattler %>%
  filter_at(vars(longitude, latitude), all_vars(!is.na(.)))

# Display the cleaned data
head(rattler)

# Creating a map for multiple rattlesnake species
# List of additional rattlesnake species to map
species_list <- c('Crotalus viridis',   # Prairie Rattlesnake
                  'Crotalus horridus',  # Timber Rattlesnake
                  'Crotalus adamanteus')  # Eastern Diamondback Rattlesnake

# Query occurrence data for additional species and append to the rattler dataset
for (species in species_list) {
  new_species_data <- occ(query = species, from = c('gbif', 'inat'), limit = 500)
  new_species_df <- occ2df(new_species_data)
  
  # Ensure longitude and latitude are numeric and clean NAs
  new_species_df <- new_species_df %>%
    mutate_at(c('longitude', 'latitude'), as.numeric) %>%
    filter_at(vars(longitude, latitude), all_vars(!is.na(.)))
  
  # Append new species data to the rattler dataframe
  rattler <- bind_rows(rattler, new_species_df)
}

# Create an interactive leaflet map to display rattlesnake occurrences
leaflet(rattler) %>% 
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(~longitude, ~latitude, 
                   radius = 3,  # size of the points
                   color = ~ifelse(name == 'Crotalus atrox', 'red', 
                                   ifelse(name == 'Crotalus viridis', 'green', 
                                          ifelse(name == 'Crotalus horridus', 'blue', 'purple'))),  # Color code species
                   fillOpacity = 0.8,  # Transparency of the points
                   popup = ~paste("Species:", name, "<br>", 
                                  "Date:", date, "<br>",
                                  "Source:", prov))  # Popup for each point with species info
