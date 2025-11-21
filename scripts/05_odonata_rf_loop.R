# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: Run our random forest, with middle ground hyperparameterization,
# over all qualifying USA/CAN odonates
# -----------------------------------------------------------------------------




# 1. Libraries
rm(list=ls())
library(ggplot2)
library(lattice)
library(caret)
library(sf)
library(ranger)
library(shapviz)
library(future.apply)
library(recipes)
library(viridis)
library(gridExtra)
library(dplyr)
library(lessR)
library(visreg)
library(randomForest)
library(janitor)

source("src/creating_rf_df.R")
source("src/run_rf_for_one_species.R")




# 2. Data
odonata_hydroatlas_overlay <- st_read("data/processed/odonata_hydroatlas_overlay.gpkg")

species_list <- read.csv("data/processed/odonata_species_list_with_obs.csv")
species_list$species <- gsub(" ", "_", species_list$species)
species_list$species <- tolower(species_list$species)

hyperparameterization <- read.csv("data/processed/optimal_hyperparameterization.csv")
optimal_mtry = hyperparameterization[hyperparameterization$species=="optimal", "mtry"]
optimal_splitrule = hyperparameterization[hyperparameterization$species=="optimal", "splitrule"]
optimal_min_node_size = hyperparameterization[hyperparameterization$species=="optimal", "min_node_size"]

# Set up overlay for rf: re-join watersheds w/o odonata obs 
all_basins <- st_read("data/raw/CAN_USA_atlas.gpkg") # this has the env. vars., IDs, and geom
odonata_hydroatlas_overlay = odonata_hydroatlas_overlay[, c(1, 18:ncol(odonata_hydroatlas_overlay))] #removes env.vars. to avoid duplication
odonata_hydroatlas_overlay$geom <- NULL
# merge. note, obviously the non-odonate-pfafs will not be selected for RF. We need them for our final map projection.
odonata_hydroatlas_overlay <- merge(all_basins, odonata_hydroatlas_overlay, by="PFAF_ID", all.x=TRUE)
odonata_hydroatlas_overlay[is.na(odonata_hydroatlas_overlay)] <- 0
odonata_hydroatlas_overlay$HYBAS_ID <- NULL
rm(all_basins)

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()




# 3. Run loop and gather results

for (species in species_list$species){
  
  species_name = species
  print(species_name)
  
  # Create dataframe to be used in random forest model
  species_rf_df <- create_rf_dataframe(odonata_hydroatlas_overlay, species_name)
  
  # Obtain results from running rf ten times with different training/test data
  species_results <-
    run_rf_for_one_species(species_name, species_rf_df, optimal_mtry,
                           optimal_splitrule, optimal_min_node_size)
  
  species_rf_results <- species_results$species_rf_results
  species_variable_importance <- species_results$species_variable_importance
  species_prediction_dataframe <- species_results$species_prediction_dataframe
  
  # Join to our larger dataframes:
  odonata_rf_results <- rbind(odonata_rf_results, species_rf_results)
  odonata_rf_variable_importance <- rbind(odonata_rf_variable_importance,
                                          species_variable_importance)
  odonata_rf_predictions <- rbind(odonata_rf_predictions,
                                  species_prediction_dataframe)
}



# 4. Write out our awesome results
write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results.csv")
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance.csv")
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions.csv")