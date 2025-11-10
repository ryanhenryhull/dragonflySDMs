# ------------------------------------------------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: Run RF training function over 10 randomly selected dragonflies,
# to learn what a good middle ground hyperparamaterization would be.
# ------------------------------------------------------------------------

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




# 2. Data
species_list <- read.csv("data/processed/odonata_species_list_with_obs.csv")
odonata_hydroatlas_overlay <- st_read("data/processed/odonata_hydroatlas_overlay.gpkg")




# 3. Selecting ten species randomly
set.seed(244)

random_numbers = sample.int(321, 10, replace=FALSE)
random_species = species_list[random_numbers, ]
rm(species_list)
rm(random_numbers)




# 4. Create dataframe to hold results
# this tuning grid lists all combinations of the RF hyperparameters
tgrid <- expand.grid(
  mtry = c(2,3,4,5,10),
  splitrule = "gini",
  min.node.size = c(5,7,10)
)

random_species_hyperparameters <- random_species
random_species_hyperparameters$mtry <- NA
random_species_hyperparameters$splitrule <- NA
random_species_hyperparameters$min_node_size <- NA
rm(random_species)




# 5. Setting up overlay for rf training

# re-join watersheds w/o odonata obs 
all_basins <- st_read("data/raw/CAN_USA_atlas.gpkg") # this has the env. vars., IDs, and geom

# take out cols to avoid duplication
odonata_hydroatlas_overlay = odonata_hydroatlas_overlay[, c(1, 18:ncol(odonata_hydroatlas_overlay))] #removes env.vars.
odonata_hydroatlas_overlay$geom <- NULL

# merge. note, obviously the non-odonate-pfafs will not be selected for RF.
# but we need them for our final map projection.
odonata_hydroatlas_overlay <- merge(all_basins, odonata_hydroatlas_overlay, by="PFAF_ID", all.x=TRUE)
odonata_hydroatlas_overlay[is.na(odonata_hydroatlas_overlay)] <- 0
odonata_hydroatlas_overlay$HYBAS_ID <- NULL

rm(all_basins)

# remove now useless columns: (this bit may also be unnecessary)
odonata_hydroatlas_overlay <- odonata_hydroatlas_overlay[
  c("PFAF_ID", "pre_mm_syr", "ele_mt_sav", "slp_dg_sav", "ari_ix_sav",
    "tmp_dc_syr",  "snd_pc_sav", "soc_th_sav", "wet_cl_smj", "lka_pc_sse",
    "dis_m3_pyr", "gad_id_smj", "snw_pc_syr", "for_pc_sse", "sgr_dk_sav",
    "aet_mm_syr", "crp_pc_sse", "watershed_obs_count",
    "GBIF_species_count", "geom", "epitheca_petechialis", "brachymesia_herbida", "aeshna_clepsydra",
    "aeshna_tuberculifera", "libellula_cyanea", "argia_oenea",
    "enallagma_exsulans", "lestes_rectangularis", "epitheca_canis",
    "ophiogomphus_rupinsulensis")
]




# 6. Run caret train function for all species

# this requires species name in different format:
random_species_hyperparameters$species <-
  tolower(gsub(" ", "_", random_species_hyperparameters$species))

all_species_training_results <- data.frame()

for (species in random_species_hyperparameters$species){
  # from src:
  species_rf_df <- create_rf_dataframe(odonata_hydroatlas_overlay, species)
  
  # keep some data away from training, for testing later
  number_watersheds_for_training <- floor(0.75 * nrow(species_rf_df))
  set.seed(849)
  training_indeces <- sample(seq_len(nrow(species_rf_df)),
                             size = number_watersheds_for_training)
  
  training_watersheds <- species_rf_df[training_indeces,]
  test_watersheds <- species_rf_df[-training_indeces,]
  
  # creates RF trees with different hyperparameter combos, evaluating their 
  # performance to learn best choices
  
  # need to dynamically paste in species name I believe
  formula <- as.formula(
                paste0("factor(", species, ")~ pre_mm_syr+ele_mt_sav+
                slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+soc_th_sav+
                wet_cl_smj+lka_pc_sse+dis_m3_pyr+snw_pc_syr+for_pc_sse+
                sgr_dk_sav+aet_mm_syr+crp_pc_sse"))
  
  species_rf_train <- train(formula,
                            data = training_watersheds,
                            method = "ranger",
                            tuneGrid = tgrid,
                            num.trees = 500,
                            importance = "impurity")
  
  rf_training_results <- species_rf_train$results
  rf_training_results$species <- species 
  all_species_training_results <- rbind(all_species_training_results, rf_training_results)
}

# Should we use kappa or accuracy to measure ideal hyperparameterization?
# Kappa best when presence/absence is unbalanced. accuracy best when ~balanced
species_balance <- data.frame()

for (species in random_species_hyperparameters$species){
  species_rf_df <- create_rf_dataframe(odonata_hydroatlas_overlay, species)
  
  number_watersheds_for_training <- floor(0.75 * nrow(species_rf_df))
  set.seed(849)
  training_indeces <- sample(seq_len(nrow(species_rf_df)),
                             size = number_watersheds_for_training)
  
  training_watersheds <- species_rf_df[training_indeces,]
  test_watersheds <- species_rf_df[-training_indeces,]
  
  results <- as.data.frame(table(training_watersheds[[species]]))
  results$species <- species
  species_balance <- rbind(species_balance, results)
} # they're quite balanced hence use accuracy




# 7. Which are the most common?
for (species in random_species_hyperparameters$species){
  
  species_results <- all_species_training_results[
    all_species_training_results$species==species,]
  
  best_mtry <- species_results[which.max(species_results$Accuracy),"mtry"]
  
  best_node_size <- species_results[which.max(species_results$Accuracy), "min.node.size"]
  
  best_splitrule <- "gini" #always used
  
  random_species_hyperparameters[
    random_species_hyperparameters$species == species, "mtry"] <- best_mtry
  
  random_species_hyperparameters[
    random_species_hyperparameters$species == species, "min_node_size"] <- best_node_size
  
  random_species_hyperparameters[
    random_species_hyperparameters$species == species, "splitrule"] <- best_splitrule
}
# most frequent combination is mtry=5, nodesize=5
optimal <- c("optimal",NA,5,"gini",5)
random_species_hyperparameters <- rbind(random_species_hyperparameters, optimal)




# 8. Write out beautiful precious data
write.csv(all_species_training_results,
          "data/processed/ten_random_species_rf_training_results.csv",
          append=FALSE,
          row.names=FALSE)

write.csv(random_species_hyperparameters,
          "data/processed/optimal_hyperparameterization.csv",
          row.names=FALSE)