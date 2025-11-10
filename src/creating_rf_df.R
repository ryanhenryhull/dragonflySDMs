# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Provide function to produce dataframe to be used in RF model for 
#          one species of interest
# -----------------------------------------------------------------------------

# Inputs:
#   A GBIF/Hydroatlas overlay with many species, watershed_obs_count column
#   Name of column for species of interest
#
# Outputs:
#   dataframe ready to run RF

create_rf_dataframe <- function(overlay, species_name){
  
  # Calculate total number of observations across all PFAFs
  nb_total_obs <- sum(overlay$watershed_obs_count)
  
  # dividing based on presence/absence
  species_presence_hydroatlas <- overlay[which(overlay[[species_name]]==1),]
  species_absence_hydroatlas <- overlay[which(overlay[[species_name]]==0),]
  
  # assigning weights to the absence watersheds based on dragonfly sampling effort:
  species_absence_hydroatlas$prob <- 
    species_absence_hydroatlas$watershed_obs_count/nb_total_obs
  
  # Select pseudoabsences randomly with the influence of assigned weight.
  # Select a number to match nb of presences
  set.seed(1080)
  species_pseudoabsences <-
    sample(1:nrow(species_absence_hydroatlas),
           size = nrow(species_presence_hydroatlas),
           prob = species_absence_hydroatlas$prob)
  
  species_rf_df <- as.data.frame(
    rbind(species_absence_hydroatlas[species_pseudoabsences,-ncol(species_absence_hydroatlas)], # removes prob
          species_presence_hydroatlas))
  
  return(species_rf_df)
}