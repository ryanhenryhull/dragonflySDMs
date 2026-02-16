# ------------------------------------------------------------------------------
# Author: Ryan Hull
# Date: December 2025
# Purpose: Generate heat map of where we can make good predictions
# ------------------------------------------------------------------------------



# I think this whole approach is cursed since it relies on comparing predictions to
# true presence /absence. but we have no true absence. and we only have
# true presence for a certain number of PFAFs. the minority of PFAFs actually.
# so if we wanna do this subtraction business, we cant use just the true presences.
# we'd need to use the 1/0 of the rf? but that obviously means nothing

# solution: simply map the pfafs that we can.
#           - for presences, that will be only 37792 pfafs of the total 147318.
#           - for absences, we will use pseudoabsences from test data only, but
#           - since we have ten different test data samplings, we can hit a lot
#           - of pfafs

# however: predictions are means across multiple iterations for each species,
#          each iteration having 10 unique test/training data splits. 
#          So if I use the combined 10 test datasets, then many of them were
#          used to train other iterations of the model... 
#          It would be thus very complicated and perhaps completely futile to
#          account for the training-ness or testing-ness of certain pseudoabsence
#          PFAFs before using them to measure error.
#          ..................post-hoc......

# ...........but what about during....?
# in our run_rf src program, prediction_dataframe is the combination of ten 
# spatial_prediction dataframes, and the mean_prediction is calculated for each
# pfaf later on.
# we could thus, before cbind()ing them in.... do all these calculations, knowing
# what test data is.
#    i.e.
# 1. we 

# solution: Simply resample pseudoabsences, and use that...









# 1. libraries
rm(list=ls())
library(ggplot2)
library(dplyr)
library(sf)
library(data.table)



# 2. Data
predictions <- read.csv("data/results/odonata_rf_predictions.csv")
odonata_hydroatlas_overlay <- st_read("data/processed/odonata_hydroatlas_overlay.gpkg")
colnames(predictions)



# 3. Prepare dataset for making heat map

# idea: convert overlay to long format like the predictions, then join by species & PFAF
setDT(odonata_hydroatlas_overlay)
setDT(predictions)

# remove nonspecies cols from overlay
odonata_hydroatlas_overlay$pre_mm_syr <- NULL
odonata_hydroatlas_overlay$ele_mt_sav <- NULL
odonata_hydroatlas_overlay$slp_dg_sav <- NULL
odonata_hydroatlas_overlay$ari_ix_sav <- NULL
odonata_hydroatlas_overlay$tmp_dc_syr <- NULL
odonata_hydroatlas_overlay$snd_pc_sav <- NULL
odonata_hydroatlas_overlay$soc_th_sav <- NULL
odonata_hydroatlas_overlay$wet_cl_smj <- NULL
odonata_hydroatlas_overlay$lka_pc_sse <- NULL
odonata_hydroatlas_overlay$dis_m3_pyr <- NULL
odonata_hydroatlas_overlay$gad_id_smj <- NULL
odonata_hydroatlas_overlay$snw_pc_syr <- NULL
odonata_hydroatlas_overlay$for_pc_sse <- NULL
odonata_hydroatlas_overlay$sgr_dk_sav <- NULL
odonata_hydroatlas_overlay$aet_mm_syr <- NULL
odonata_hydroatlas_overlay$crp_pc_sse <- NULL
odonata_hydroatlas_overlay$species_obs_count <- NULL
odonata_hydroatlas_overlay$watershed_obs_count <- NULL

# long
overlay_long <- melt(
  odonata_hydroatlas_overlay,
  id.vars="PFAF_ID",
  variable.name="species",
  value.name = "present"
)

# now overlay_long and predictions both have $PFAF_ID, and $species
# but overlay has the actual presence 1/0 (note 0 is not absent; just NOT present)
# and predictions has the decimal value prediction.
colnames(overlay_long) # PFAF_ID, species, present
colnames(predictions) # species, PFAF, mean_prediction
colnames(predictions)[2] <- "PFAF_ID" # to match overlay

# join them
setkey(overlay_long, species, PFAF_ID)
setkey(predictions, species, PFAF_ID)
  
predictions_with_actual_presence_data <-
  overlay_long[predictions, on = .(species, PFAF_ID)]

# keep only cols where presence = 1 (0 or NULL is useless, the 0 is not true absence)
predictions_with_actual_presence_data <-
  predictions_with_actual_presence_data[predictions_with_actual_presence_data$present == 1,]

# above fails. try chatgpt recommendation cause no time
# 1. Create present_any column from list
#predictions_with_actual_presence_data[
#  , present_any := vapply(present, function(x) any(x == 1), logical(1))
#]

# 2. Filter only confirmed presences
#predictions_present_only <- predictions_with_actual_presence_data[present_any == TRUE, ]

# 3. Optional: remove list column to free memory
#predictions_present_only[, present := NULL]



# 4. Make heat map 
ggplot(sf) +
  geom_sf(aes(fill = error_col), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal()