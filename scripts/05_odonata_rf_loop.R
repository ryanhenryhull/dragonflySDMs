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

species_list <- read.csv("data/processed/full_odonata_species_list_with_obs.csv")
species_list$species <- gsub(" ", "_", species_list$species)
species_list$species <- tolower(species_list$species)

hyperparameterization <- read.csv("data/processed/optimal_hyperparameterization.csv")
optimal_mtry = hyperparameterization[hyperparameterization$species=="optimal", "mtry"]
optimal_splitrule = hyperparameterization[hyperparameterization$species=="optimal", "splitrule"]
optimal_min_node_size = hyperparameterization[hyperparameterization$species=="optimal", "min_node_size"]

# Set up overlay for rf: re-join watersheds w/o odonata obs 
all_basins <- st_read("data/raw/NA_CA_atlas.gpkg") # this has the env. vars., IDs, and geom
odonata_hydroatlas_overlay = odonata_hydroatlas_overlay[, c(1, 19:ncol(odonata_hydroatlas_overlay))] #removes env.vars. to avoid duplication
odonata_hydroatlas_overlay$geom <- NULL
# merge. note, obviously the non-odonate-pfafs will not be selected for RF. We need them for our final map projection.
odonata_hydroatlas_overlay <- merge(all_basins, odonata_hydroatlas_overlay, by="PFAF_ID", all.x=TRUE)
odonata_hydroatlas_overlay[is.na(odonata_hydroatlas_overlay)] <- 0
odonata_hydroatlas_overlay$HYBAS_ID <- NULL
rm(all_basins)




# 3. Run loop and gather results
# we need to break this up to save results or else too dangerous
species_list_1_50 <- species_list[1:50,]
species_list_51_100 <- species_list[51:100,]
species_list_101_150 <- species_list[101:150,]
species_list_151_200 <- species_list[151:200,]
species_list_201_250 <- species_list[201:250,]
species_list_251_300 <- species_list[251:300,]
species_list_301_350 <- species_list[301:350,]
species_list_351_385 <- species_list[351:385,]




# 1_50 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_1_50$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, "  within block 1_50"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_1_50.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_1_50.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_1_50.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 51_100 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_51_100$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 51_100"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_51_100.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_51_100.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_51_100.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 101_150 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_101_150$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 101_150"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_101_150.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_101_150.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_101_150.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 151_200 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_151_200$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 151_200"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_151_200.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_151_200.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_151_200.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 201_250 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_201_250$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 201_250"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_201_250.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_201_250.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_201_250.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 251_300 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_251_300$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 251_300"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_251_300.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_251_300.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_251_300.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 301_350 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_301_350$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 301_350"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_301_350.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_301_350.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_301_350.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)





# 351_385 block

# preparing dataframes to collect the loop's results:
odonata_rf_results <- data.frame()
odonata_rf_variable_importance <- data.frame()
odonata_rf_predictions <- data.frame()

iteration = 1
for (species in species_list_351_385$species){
  
  species_name = species
  print(species_name)
  print(paste0("Iteration: ", iteration, " within block 351_385"))
  
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
  
  iteration = iteration + 1
}

write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results_351_385.csv", row.names=FALSE)
write.csv(odonata_rf_variable_importance, "data/results/odonata_rf_variable_importance_351_385.csv", row.names=FALSE)
write.csv(odonata_rf_predictions, "data/results/odonata_rf_predictions_351_385.csv", row.names=FALSE)
rm(odonata_rf_results)
rm(odonata_rf_variable_importance)
rm(odonata_rf_predictions)




# reassemble everything
# performance results
odonata_rf_results_1_50 <- read.csv("data/results/odonata_rf_performance_results_1_50.csv")
odonata_rf_results_51_100 <- read.csv("data/results/odonata_rf_performance_results_51_100.csv") 
odonata_rf_results_101_150 <- read.csv("data/results/odonata_rf_performance_results_101_150.csv")
odonata_rf_results_151_200 <- read.csv("data/results/odonata_rf_performance_results_151_200.csv")
odonata_rf_results_201_250 <- read.csv("data/results/odonata_rf_performance_results_201_250.csv")
odonata_rf_results_251_300 <- read.csv("data/results/odonata_rf_performance_results_251_300.csv")
odonata_rf_results_301_350 <- read.csv("data/results/odonata_rf_performance_results_301_350.csv")
odonata_rf_results_351_385 <- read.csv("data/results/odonata_rf_performance_results_351_385.csv")

odonata_rf_results <- rbind(
  odonata_rf_results_1_50,
  odonata_rf_results_51_100,
  odonata_rf_results_101_150,
  odonata_rf_results_151_200,
  odonata_rf_results_201_250,
  odonata_rf_results_251_300,
  odonata_rf_results_301_350,
  odonata_rf_results_351_385)

# Variable importance
variable_importance_1_50 <- read.csv("data/results/odonata_rf_variable_importance_1_50.csv")
variable_importance_51_100 <- read.csv("data/results/odonata_rf_variable_importance_51_100.csv") 
variable_importance_101_150 <- read.csv("data/results/odonata_rf_variable_importance_101_150.csv")
variable_importance_151_200 <- read.csv("data/results/odonata_rf_variable_importance_151_200.csv")
variable_importance_201_250 <- read.csv("data/results/odonata_rf_variable_importance_201_250.csv")
variable_importance_251_300 <- read.csv("data/results/odonata_rf_variable_importance_251_300.csv")
variable_importance_301_350 <- read.csv("data/results/odonata_rf_variable_importance_301_350.csv")
variable_importance_351_385 <- read.csv("data/results/odonata_rf_variable_importance_351_385.csv")

variable_importance <- rbind(
  variable_importance_1_50,
  variable_importance_51_100,
  variable_importance_101_150,
  variable_importance_151_200,
  variable_importance_201_250,
  variable_importance_251_300,
  variable_importance_301_350,
  variable_importance_351_385)

rm(list=setdiff(ls(), c("variable_importance", "odonata_rf_results", "odonata_obs")))

# performance : simply bind together when needed, since not enough storage to store full version.
# but for the others:
write.csv(variable_importance, "data/results/odonata_rf_variable_importance", append=FALSE, row.names=FALSE)
write.csv(odonata_rf_results, "data/results/odonata_rf_performance_results.csv",append=FALSE, row.names=FALSE)
