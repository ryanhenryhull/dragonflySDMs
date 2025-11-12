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

hyperparameterization <- read.csv("data/processed/optimal_hyperparameterization.csv")
optimal_mtry = hyperparameterization[hyperparameterization$species=="optimal", "mtry"]
optimal_splitrule = hyperparameterization[hyperparameterization$species=="optimal", "splitrule"]
optimal_min_node_size = hyperparameterization[hyperparameterization$species=="optimal", "min_node_size"]


# Set up overlay for rf
# re-join watersheds w/o odonata obs 
all_basins <- st_read("data/raw/CAN_USA_atlas.gpkg") # this has the env. vars., IDs, and geom
odonata_hydroatlas_overlay = odonata_hydroatlas_overlay[, c(1, 18:ncol(odonata_hydroatlas_overlay))] #removes env.vars. to avoid duplication
odonata_hydroatlas_overlay$geom <- NULL

# merge. note, obviously the non-odonate-pfafs will not be selected for RF. We need them for our final map projection.
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
    "GBIF_species_count", "geom", "epitheca_petechialis", "brachymesia_herbida",
    "aeshna_clepsydra", "aeshna_tuberculifera", "libellula_cyanea", "argia_oenea",
    "enallagma_exsulans", "lestes_rectangularis", "epitheca_canis",
    "ophiogomphus_rupinsulensis")
]




# 3. Run loop and gather results

for (species in species_list$species){
  
  # Create dataframe to be used in random forest model
  species_rf_df <- create_rf_dataframe(odonata_hydroatlas_overlay, species)
  
  # Obtain results from running rf ten times with different training/test data
  
  results_question_mark <-
    run_rf_for_one_species(species_rf_df, optimal_mtry, optimal_splitrule,
                           optimal_min_node_size)
  
  
}