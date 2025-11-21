# ------------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Calculate mean and median latitude and area from range predictions
# ------------------------------------------------------------------------------



# 1. Libraries



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



# 4. Write out beautiful data
write.csv(odonata_rf_results, "data/results/odonata_rf_performance_with_latitude_stats.csv", row.names=FALSE)