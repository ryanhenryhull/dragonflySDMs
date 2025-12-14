# ------------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Calculate mean and median latitude and area from range predictions
# ------------------------------------------------------------------------------



# 1. Libraries
library(sf)
library(lwgeom)


# 2. Data
rm(list=ls())
odonata_rf_results <- read.csv("data/results/odonata_rf_performance_results.csv")
odonata_obs <- read.csv("data/processed/all_odonata_obs_clean.csv")
species_list <- read.csv("data/processed/odonata_species_list_with_obs.csv")

odonata_obs$species <- gsub(" ", "_", odonata_obs$species)
odonata_obs$species <- tolower(odonata_obs$species)
species_list$species <- gsub(" ", "_", species_list$species)
species_list$species <- tolower(species_list$species)



# 3. Add in mean lat & CIs, median lat, and convex hull to every species
odonata_rf_results$mean_lat <- NA
odonata_rf_results$mean_lat_5_CI <- NA
odonata_rf_results$mean_lat_95_CI <- NA
odonata_rf_results$median_lat <- NA
odonata_rf_results$num_obs <- NA

for (species in odonata_rf_results$species){
  
  latitudes <- odonata_obs[odonata_obs$species == species, "decimalLatitude"]
  
  num_obs <- species_list[species_list$species == species, "observations"]
  
  mean_lat <- mean(latitudes)
  median_lat <- median(latitudes)
  ci_lat <- quantile(latitudes, probs = c(0.025, 0.975))
  odonata_rf_results[odonata_rf_results$species == species, "mean_lat"] <- mean_lat
  odonata_rf_results[odonata_rf_results$species == species, "mean_lat_5_CI"] <- ci_lat[1]
  odonata_rf_results[odonata_rf_results$species == species, "mean_lat_95_CI"] <- ci_lat[2]
  odonata_rf_results[odonata_rf_results$species == species, "median_lat"] <- median_lat
  odonata_rf_results[odonata_rf_results$species == species, "num_obs"] <- num_obs
}



# 4. calculate convex hull and centroid latitude/longitude based on that

odonata_rf_results$convex_hull_area_km2 <- NA
odonata_rf_results$centroid_decimalLat <- NA
odonata_rf_results$centroid_decimalLong <- NA
odonata_rf_results$observation_density_obs_per_km2 <- NA


species_obs_sf <- st_as_sf(odonata_obs,
                           coords = c("decimalLongitude", "decimalLatitude"),
                           crs=4326)
species_obs_projection <- st_transform(species_obs_sf, 5070) ## 5070 is the North America Albers Equal Area projection, recommended for calculating area here

for (species in species_list$species){
  print(species)
  this_species_obs_projection <- species_obs_projection[species_obs_projection$species == species,]
  convex_hull <- st_convex_hull(st_union(this_species_obs_projection))
  area_m2 <- st_area(convex_hull)
  area_km2 <- area_m2 * 0.000001
  centroid <- st_centroid(convex_hull)
  centroid_latlong <- st_transform(centroid, 4326) # this crs is made for interpreting lat long well
  centroid_lat <- st_coordinates(centroid_latlong)[2]
  centroid_long <- st_coordinates(centroid_latlong)[1]
  
  # assigning these values to our data
  odonata_rf_results[odonata_rf_results$species == species, "convex_hull_area_km2"] <-
    area_km2
  odonata_rf_results[odonata_rf_results$species == species, "centroid_decimalLat"] <-
    centroid_lat
  odonata_rf_results[odonata_rf_results$species == species, "centroid_decimalLong"] <-
    centroid_long
  
  # assigning density of observations to our data:
  density = odonata_rf_results[odonata_rf_results$species == species, "num_obs"] / area_km2
  odonata_rf_results[odonata_rf_results$species == species, "observation_density_obs_per_km2"] <-
    density
}



# 5. Write out beautiful data
write.csv(odonata_rf_results, "data/results/odonata_rf_performance_with_latitude_stats.csv", row.names=FALSE)
