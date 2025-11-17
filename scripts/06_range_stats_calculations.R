# ------------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Calculate mean and median latitude and area from range predictions
# ------------------------------------------------------------------------------

# 1. Libraries



# 2. Data
odonata_rf_results <- read.csv("data/results/odonata_rf_performance_results.csv")
odonata_obs <- read.csv("data/processed/all_odonata_obs_clean.csv")



# 3. Add in mean lat & CIs, median lat, and convex hull to every species

odonata_rf_results$mean_lat <- NA
odonata_rf_results$mean_lat_2.5 <- NA
odonata_rf_results$mean_lat_97.5 <- NA
odonata_rf_results$median_lat <- NA

for (species in odonata_rf_results$species){
  
  latitudes <- odonata_obs[odonata_obs$species == species, "latitude"] # not sure if this fits the df structure
  mean_lat <- mean(latitudes)
  odonata_rf_results[odonata_rf_results$species == species, "mean_latitude"]
  
  # etc...
}
  