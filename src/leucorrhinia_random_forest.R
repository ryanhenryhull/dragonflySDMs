# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Run random forest model to assess habitat suitability of north
# american whitefaces
# Output: 
# Based on: Lars Iversen milfoil experiment random forest code
# -----------------------------------------------------------------------------

# 1. Libraries
library(sf)
library(ggplot2)
library(ranger)
library(shapviz)
#install.packages("future.apply", dependencies = TRUE, type = "binary")
library(future.apply)
#install.packages("caret", dependencies = TRUE, type = "binary")
library(caret)
library(recipes)
library(caret)
library(viridis)
library(gridExtra)
library(dplyr)
library(lessR)
library(visreg)
library(randomForest)
# issues with rtools required some more precise installation
#install.packages("rcompanion", type = "binary", repos = "https://cloud.r-project.org/")
library(rcompanion)
#install.packages("kernelshap", type = "binary", repos = "https://cloud.r-project.org/")
library(kernelshap)

# 2. Initial data processing
rm(list=ls())
odonata_hydroatlas_overlay <- st_read("data/odonata_hydroatlas_overlay.gpkg")

intacta_presence_hydroatlas <- odonata_hydroatlas_overlay[which(odonata_hydroatlas_overlay$leucorrhinia_intacta==1),]
intacta_absence_hydroatlas <- odonata_hydroatlas_overlay[which(odonata_hydroatlas_overlay$leucorrhinia_intacta==0),]

# Calculate total number of observations across all PFAFs
unique_pfafs_with_obs <- odonata_hydroatlas_overlay[
  !duplicated(odonata_hydroatlas_overlay$PFAF_ID),
  c("PFAF_ID","watershed_obs_count")] # result shows there were no duplicate PFAFs to begin with. makes sense
nb_total_obs <- sum(odonata_hydroatlas_overlay$watershed_obs_count)

# assigning weights to the absence watersheds based on dragonfly sampling effort:
intacta_absence_hydroatlas$prob <- 
  #intacta_absence_hydroatlas$GBIF_species_count/sum(no_milfoil$GBIF_species_count)
  intacta_absence_hydroatlas$watershed_obs_count/nb_total_obs

# Select pseudoabsences randomly with the influence of assigned weight.
# Select a number to match nb of presences
set.seed(1080)
intacta_pseudoabsences <-
  sample(1:nrow(intacta_absence_hydroatlas),
         size = nrow(intacta_presence_hydroatlas),
         prob = intacta_absence_hydroatlas$prob)

# Create dataframe used in RF
intacta_rf <- as.data.frame(
  rbind(intacta_absence_hydroatlas[intacta_pseudoabsences,-ncol(intacta_absence_hydroatlas)], # removes prob
        intacta_presence_hydroatlas)
)


# 3. Initial Random Forest Predictions
number_watersheds_for_training <- floor(0.75 * nrow(intacta_rf))
set.seed(849)
training_indeces <- sample(seq_len(nrow(intacta_rf)), size = number_watersheds_for_training)

training_watersheds <- intacta_rf[training_indeces,]
test_watersheds <- intacta_rf[-training_indeces,]

# running the RF to create trained model
rf_model <-
  ranger(factor(leucorrhinia_intacta)~
               pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
               soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr+gad_id_smj,
               data = training_watersheds)

# estimating accuracy:
pred <- predict(rf_model, test_watersheds)
estimated_accuracy <- # proportion of correct classifications
  sum(pred$predictions==test_watersheds$leucorrhinia_intacta)/length(test_watersheds$leucorrhinia_intacta) 

false_positive_rate <- 
  sum(as.numeric(pred$predictions[which(test_watersheds$Leucorrhinia_intacta==1)])-1)/
  length(which(test_watersheds$leucorrhinia_intacta==1)) # note = 0... so model may be very strongly biased toward predicting absences
false_negative_rate <-
  sum(as.numeric(pred$predictions[which(test_watersheds$leucorrhinia_intacta==0)])-1)/
  length(which(test_watersheds$leucorrhinia_intacta==0))

table(test_watersheds$leucorrhinia_intacta) # shows #absences and #presences for reference (each about 350)
table(pred$predictions) # shows prediction predicts 400 presences 300 absences
# prob some issue above



# 4. Tuning the model

# this tuning grid lists all combinations of the RF hyperparameters
tgrid <- expand.grid(
  mtry = c(2,3,4,5,10), # removed 15,20 and added 10 since we only have 11 parameters? ask lars
  splitrule = "gini",
  min.node.size = c(5,7,10)
)

# creates random forest trees with all those combinations, evaluating their 
# performance to learn best hyperparameter combinations
train(factor(leucorrhinia_intacta)~
        pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
        soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr+gad_id_smj,
        data = training_watersheds,
        method = "ranger",
        tuneGrid = tgrid,
        num.trees = 500,
        importance = "impurity")
# the above spits out mtry=2, splitrule = gini, and min.mode.size = 10 as the optimal. use this later

# 5. Evaluating this tuned model over multiple iterations

# Creating empty data structures to contain results of our RF evaluations
rf_accuracy <- data.frame(accuracy=double(),fn=double(),fp=double(),
                       data=character(),
                       stringsAsFactors=FALSE)
prediction_dataframe <- as.data.frame(odonata_hydroatlas_overlay)[,1:2]
prediction_dataframe[,2]<-NA
variable_importance <- data.frame(importance=double(),varnames=character(), stringsAsFactors=FALSE)

# Run over 10? 100? 1000? iterations. I dont really understand the importance of this
for (i in 1:10){
  # the sampling part
  intacta_absence_hydroatlas$prob <- 
    intacta_absence_hydroatlas$watershed_obs_count/nb_total_obs
  
  intacta_pseudoabsences <-
    sample(1:nrow(intacta_absence_hydroatlas),
           size = nrow(intacta_presence_hydroatlas),
           prob = intacta_absence_hydroatlas$prob)
  
  intacta_rf <- as.data.frame(
    rbind(intacta_absence_hydroatlas[intacta_pseudoabsences,-ncol(intacta_absence_hydroatlas)], # removes prob
          intacta_presence_hydroatlas))
  
  number_watersheds_for_training <- floor(0.75 * nrow(intacta_rf))
  training_indeces <- sample(seq_len(nrow(intacta_rf)), size = number_watersheds_for_training)
  training_watersheds <- intacta_rf[training_indeces,]
  test_watersheds <- intacta_rf[-training_indeces,]
  
  # running the model and evaluating it
  rf_model <-
    ranger(factor(leucorrhinia_intacta)~
             pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
             soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr+gad_id_smj,
           data = training_watersheds)
  
  pred <- predict(rf_model, test_watersheds)
  
  # assessing accuracy
  rf_accuracy[i,1] <- sum(pred$predictions==test_watersheds$leucorrhinia_intacta)/
    length(test_watersheds$leucorrhinia_intacta) 
  rf_accuracy[i,4] <- "name_foo_bar"
  
  # FPRs and FNRs:
  rf_accuracy[i,2] <- 
    sum(as.numeric(pred$predictions[which(test_watersheds$Leucorrhinia_intacta==1)])-1)/
    length(which(test_watersheds$leucorrhinia_intacta==1))
  rf_accuracy[i,3] <-
    sum(as.numeric(pred$predictions[which(test_watersheds$leucorrhinia_intacta==0)])-1)/
    length(which(test_watersheds$leucorrhinia_intacta==0))
  
  # assessing variable importance
  # getting the values for this iteration
  iteration_importance <- as.data.frame(rf_model$variable.importance)
  iteration_importance$varnames <- rownames(iteration_importance)
  rownames(iteration_importance) <- NULL
  colnames(iteration_importance)[1] <- "importance"
  # adding it to the larger importance table
  variable_importance <- rbind(variable_importance, iteration_importance)
  
  
  
  # The previous RF was useful for training whether to classify a watershed
  # as 1 or 0 (presence/absence). Tells us about accuracy, FPRs, FNRs, var imp.
  # But we cannot use this for mapping - we need a probability of pres/absence.
  # That is what the following RF does
  
  probability_rf_model <- 
    ranger(factor(leucorrhinia_intacta)~
          pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
          soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr+gad_id_smj,
          data = training_watersheds,
          importance = "impurity",
          mtry = 2, #from what our training suggested
          min.node.size = 10, # ditto
          probability = TRUE) # key difference
  
  spatial_prediction <- predict(probability_rf_model,
                                as.data.frame(odonata_hydroatlas_overlay))
  
  # add to big prediction dataframe
  prediction_dataframe <- cbind(prediction_dataframe,
                                spatial_prediction$predictions[,2]) # from lars
}
