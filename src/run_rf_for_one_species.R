# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Execute rf and capture results for a given species
# -----------------------------------------------------------------------------

# Inputs:
#   species_rf_df: the output of src/creating_rf_df
# Outputs:
#   a table containing accuracy, false positive rate, false negative rate,



run_rf_for_one_species <- function(species_rf_df, optimal_mtry, optimal_splitrule,
                              optimal_min_node_size){
  
  species_name <- ...
  
  # keep some data away from training, for testing later
  number_watersheds_for_training <- floor(0.75 * nrow(species_rf_df))
  set.seed(849)
  training_indeces <- sample(seq_len(nrow(species_rf_df)), size = number_watersheds_for_training)
  
  training_watersheds <- species_rf_df[training_indeces,]
  test_watersheds <- species_rf_df[-training_indeces,]
  
}