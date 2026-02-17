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




# 1. libraries
rm(list=ls())
library(ggplot2)
library(dplyr)
library(sf)
library(data.table)
library(tidyr)




# 2. Data
rf_performance_results <- read.csv("data/results/odonata_rf_performance_with_latitude_stats.csv") # not useful here
rf_predictions <- read.csv("data/results/odonata_rf_predictions.csv")
species_list <- read.csv("data/processed/full_odonata_species_list_with_obs.csv")

overlay <- st_read("data/processed/odonata_hydroatlas_overlay.gpkg")
colnames(overlay)[1] <- "PFAF"
# we will get this and the following from hydroatlas. remove to avoid duplication later
overlay$geom <- NULL
overlay$pre_mm_syr <- NULL
overlay$ele_mt_sav <- NULL
overlay$slp_dg_sav <- NULL
overlay$ari_ix_sav <- NULL
overlay$tmp_dc_syr <- NULL
overlay$snd_pc_sav <- NULL
overlay$soc_th_sav <- NULL
overlay$wet_cl_smj <- NULL
overlay$lka_pc_sse <- NULL
overlay$dis_m3_pyr <- NULL
overlay$gad_id_smj <- NULL
overlay$snw_pc_syr <- NULL
overlay$for_pc_sse <- NULL
overlay$sgr_dk_sav <- NULL
overlay$aet_mm_syr <- NULL
overlay$crp_pc_sse <- NULL
overlay$fec_cl_smj <- NULL

overlay <- overlay %>%
  relocate("GBIF_species_count", .after="watershed_obs_count") # relocate column
colnames(overlay)[5:ncol(overlay)] <-
  paste0(colnames(overlay)[5:ncol(overlay)],  "_presence_absence")


hydroatlas <- st_read("data/raw/NA_CA_atlas.gpkg")
hydroatlas$HYBAS_ID <- NULL
colnames(hydroatlas)[1] <- "PFAF"




# 3. convert predictions df to wide format (pivot species into columns)
# the overlay is in wide format (one row per PFAF, columns for each species)
# the predictions is in long format (a species column and a PFAF column, thus
#                                    each species has nrow = nb_pfafs)
# Mapping would be easier with wide format predictions.
rf_predictions_wide <- rf_predictions |>
  pivot_wider(names_from = species,
              values_from = mean_prediction,
              values_fill = NA) # if any species-pfaf combo is missing... shouldn't happen




# 4 join data

# join hydroatlas data (this has all pfafs, whereas overlay only has pfafs where there are presences)
rf_predictions_wide <- left_join(rf_predictions_wide, hydroatlas, by="PFAF")
# join species presence/absence columns from the overlay
rf_predictions_wide <- left_join(rf_predictions_wide, overlay, by="PFAF")
rf_predictions_wide$species_obs_count <- NULL




# 5. Calculate false-negativity using rows where presence=1
# rows where there's NA = rows where none of the species are present
rf_predictions_wide_present <-
  rf_predictions_wide[!is.na(rf_predictions_wide$erythemis_mithroides_presence_absence),] # any col would do

pfaf_false_negativity_results <- data.frame(
  column1=character(),
  column2=numeric())
colnames(pfaf_false_negativity_results)[1] <- "pfaf"
colnames(pfaf_false_negativity_results)[2] <- "false_negativity"
  
presence_absence_cols <- grep("_presence_absence$", colnames(rf_predictions_wide_present), value = TRUE)

for (i in 1:nrow(rf_predictions_wide_present)){
  print(i)
  pfaf <- rf_predictions_wide_present[i,"PFAF"]
  
  row_as_vector <- as.numeric(rf_predictions_wide_present[i, presence_absence_cols])
  present_colnames <- presence_absence_cols[row_as_vector == 1]
  
  pfaf_false_negativities <- c()
  
  for (species_name in present_colnames){
    species_name <- sub("_presence_absence$", "", species_name) # now this is the colname for the prediction
    false_negativity <- 1 - rf_predictions_wide_present[[species_name]][i]
    pfaf_false_negativities <- c(pfaf_false_negativities, false_negativity)
  }
  
  if (length(pfaf_false_negativities) > 0){ # this should be every pfaf that we're using, by definition of the overlay.... but it was failing at some rows. we'll evaluate losses after 
    pfaf_false_negativity_results <- rbind(
      pfaf_false_negativity_results,
      data.frame(pfaf=pfaf, false_negativity=mean(pfaf_false_negativities)))
  }
  else {
    print("strangely, no species present at this pfaf desipte it being present in overlay.")
  }
}

nb_strange_missed_pfafs <- nrow(rf_predictions_wide_present) - nrow(pfaf_false_negativity_results)  #<1% 

pfaf_geom <-  hydroatlas[,c("PFAF","geom")]
pfaf_false_negativity_results <- 
  left_join(pfaf_false_negativity_results, pfaf_geom, by="PFAF")

# write out these results:
pfaf_false_negativity_results <- st_as_sf(pfaf_false_negativity_results)
st_write(pfaf_false_negativity_results, "data/results/pfaf_false_negativity_results.gpkg",
         append=FALSE)

# modify to avoid aleutian island mapping annoyance
hydroatlas <- st_wrap_dateline(
  hydroatlas,
  options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
  quiet = FALSE
)

# Make chloropeth map based on this
false_positivity_map <-
  ggplot()+
  geom_sf(data=hydroatlas, fill="grey", color=NA) + # bottom layer: grey for all pfafs
  geom_sf(data=pfaf_false_negativity_results, aes(fill = false_negativity), color = NA) + # top layer: color for evaluated pfafs
  scale_fill_viridis_c(option = "magma") +
  coord_sf(
    xlim = c(-170, -50)  # limit mapped longitudes to avoid aleutian wrapping
  )+
  theme_minimal()
false_positivity_map
ggsave("outputs/false_positivity_map.png",false_positivity_map)



# 6. Make spatial plot of false positivity using... unideal data


