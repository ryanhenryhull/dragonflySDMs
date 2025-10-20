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
odonata_hydroatlas_overlay <- st_read("data/odonata_hydroatlas_overlay.gpkg") # note this includes an na col.... so its just some odonate I guess?

# note we'd excluded watersheds w/o odonata obs, we must re-join them
all_basins <- st_read("data/CAN_USA_atlas.gpkg")
odonata_hydroatlas_overlay = odonata_hydroatlas_overlay[, c(1, 13:ncol(odonata_hydroatlas_overlay))]
odonata_hydroatlas_overlay$geom <- NULL
odonata_hydroatlas_overlay <- merge(all_basins, odonata_hydroatlas_overlay, by="PFAF_ID", all.x=TRUE)
odonata_hydroatlas_overlay[is.na(odonata_hydroatlas_overlay)] <- 0

# dividing based on presence/absence
intacta_presence_hydroatlas <- odonata_hydroatlas_overlay[which(odonata_hydroatlas_overlay$leucorrhinia_intacta==1),]
intacta_absence_hydroatlas <- odonata_hydroatlas_overlay[which(odonata_hydroatlas_overlay$leucorrhinia_intacta==0),]

# Calculate total number of observations across all PFAFs
unique_pfafs_with_obs <- odonata_hydroatlas_overlay[
  !duplicated(odonata_hydroatlas_overlay$PFAF_ID),
  c("PFAF_ID","watershed_obs_count")] # result shows there were no duplicate PFAFs to begin with. makes sense
nb_total_obs <- sum(odonata_hydroatlas_overlay$watershed_obs_count) # this checks out

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
intacta_rf_df <- as.data.frame(
  rbind(intacta_absence_hydroatlas[intacta_pseudoabsences,-ncol(intacta_absence_hydroatlas)], # removes prob
        intacta_presence_hydroatlas))



# 3. Initial Random Forest Predictions
number_watersheds_for_training <- floor(0.75 * nrow(intacta_rf_df))
set.seed(849)
training_indeces <- sample(seq_len(nrow(intacta_rf_df)), size = number_watersheds_for_training)

training_watersheds <- intacta_rf_df[training_indeces,]
test_watersheds <- intacta_rf_df[-training_indeces,]

# running the RF. This builds tons of decision trees using different combinations
# of our parameters and different subsets of our data,
# then combines them to make one averaged model.
rf_model <-
  ranger(factor(leucorrhinia_intacta)~
               pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
               soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr,
               data = training_watersheds)

# estimating accuracy:
pred <- predict(rf_model, test_watersheds)
estimated_accuracy <- # proportion of correct classifications
  sum(pred$predictions==test_watersheds$leucorrhinia_intacta)/length(test_watersheds$leucorrhinia_intacta) 
test_watersheds$prediction = pred$predictions

test = test_watersheds %>%
  select(c("prediction", "leucorrhinia_intacta"))

test$prediction <- as.numeric(test$prediction)



# note as.numeric strangely converts factor of 0 and 1 to 1 and 2, hence our -1
false_negative_rate <- # 0.91 is high...
  sum(as.numeric(pred$predictions[which(test_watersheds$leucorrhinia_intacta==1)])-1)/
  length(which(test_watersheds$leucorrhinia_intacta==1))
false_positive_rate <- # 0.3 is ok
  sum(as.numeric(pred$predictions[which(test_watersheds$leucorrhinia_intacta==0)])-1)/
  length(which(test_watersheds$leucorrhinia_intacta==0))



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
        soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr
        data = training_watersheds,
        method = "ranger",
        tuneGrid = tgrid,
        num.trees = 500,
        importance = "impurity")
# the above spits out mtry=2, splitrule = gini, and min.mode.size = 10
#as the optimal hyperparameters. use this later



# 5. Runnning tuned model over multiple iterations

# Creating empty data structures to contain results of our RF evaluations
rf_accuracy <- data.frame(accuracy=double(),fn=double(),fp=double(),
                       data=character(),
                       stringsAsFactors=FALSE)
prediction_dataframe <- as.data.frame(odonata_hydroatlas_overlay)[,1]
variable_importance <- data.frame(importance=double(),varnames=character(), stringsAsFactors=FALSE)

# Run over 10? 100? 1000? iterations. 
# I dont really understand the importance of this
# seed is not supposed to be set, but isn't it set earlier?
for (i in 1:10){
  # the sampling part
  intacta_absence_hydroatlas$prob <- 
    intacta_absence_hydroatlas$watershed_obs_count/nb_total_obs
  
  intacta_pseudoabsences <-
    sample(1:nrow(intacta_absence_hydroatlas),
           size = nrow(intacta_presence_hydroatlas),
           prob = intacta_absence_hydroatlas$prob)
  
  intacta_rf_df <- as.data.frame(
    rbind(intacta_absence_hydroatlas[intacta_pseudoabsences,-ncol(intacta_absence_hydroatlas)], # removes prob
          intacta_presence_hydroatlas))
  
  number_watersheds_for_training <- floor(0.75 * nrow(intacta_rf_df))
  training_indeces <- sample(seq_len(nrow(intacta_rf_df)), size = number_watersheds_for_training)
  training_watersheds <- intacta_rf_df[training_indeces,]
  test_watersheds <- intacta_rf_df[-training_indeces,]
  
  # running the model and evaluating it
  rf_model <-
    ranger(factor(leucorrhinia_intacta)~
             pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
             soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr,
           data = training_watersheds)
  
  pred <- predict(rf_model, test_watersheds)
  
  # assessing accuracy
  rf_accuracy[i,1] <- sum(pred$predictions==test_watersheds$leucorrhinia_intacta)/
    length(test_watersheds$leucorrhinia_intacta) 
  rf_accuracy[i,4] <- "name_foo_bar"
  
  # FPRs and FNRs:
  rf_accuracy[i,2] <- 
    sum(as.numeric(pred$predictions[which(test_watersheds$leucorrhinia_intacta==1)])-1)/
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
          soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr,
          data = training_watersheds,
          importance = "impurity",
          mtry = 2, #from what our training suggested
          min.node.size = 10, # ditto
          probability = TRUE) # key difference
  
  spatial_prediction <- predict(probability_rf_model,
                                as.data.frame(odonata_hydroatlas_overlay))
  
  # add this iteration to big prediction dataframe
  prediction_dataframe <- cbind(prediction_dataframe,
                                spatial_prediction$predictions[,2]) # from lars
}



# 6. Evaluating average performance after multiple iterations

# accuracy
accuracy_lm <- lm(accuracy~1, data=rf_accuracy)
coef(accuracy_lm) # mean accuracy
confint(accuracy_lm) # 95% confidence intervals

# do the same for FP and FN
fp_lm <- lm(fp~1, data=rf_accuracy)
coef(fp_lm)
confint(fp_lm)

fn_lm <- lm(fn~1, data=rf_accuracy)
coef(fn_lm) # strange...
confint(fn_lm) # strange ...



# 7. Predicting and projecting distributions
#identical(odonata_hydroatlas_overlay$PFAF_ID, prediction_dataframe$PFAF_ID)

odonata_hydroatlas_overlay$intacta_prediction <-
  rowMeans(prediction_dataframe[,2:ncol(prediction_dataframe)])

summary(odonata_hydroatlas_overlay$intacta_prediction)

# what proportion of total area is suitable? Estimation.
# may need a gpkg col I dont have. also whats the point of this

# plotting probability of species presence

# the following may help depth error
odonata_hydroatlas_overlay <- st_make_valid(odonata_hydroatlas_overlay)

intacta_model1 <- ggplot() +
  geom_sf(data = odonata_hydroatlas_overlay, aes(fill = intacta_prediction, colour=intacta_prediction)) +
  scale_fill_viridis() +
  scale_colour_viridis()+
  theme_bw()
intacta_model1

ggsave(intacta_model1, filename="outputs/intacta_map.png")


# Visualizing variable importance:
intacta_var_imp <- as.data.frame(coef(lm(importance~-1+varnames,data = variable_importance)))
intacta_var_imp$varnames <- rownames(intacta_var_imp) # row names to column
rownames(intacta_var_imp) <- NULL  
colnames(intacta_var_imp)[1] <- "Importance"

intacta_var_imp_plot <-
  ggplot(intacta_var_imp, aes(x=reorder(varnames, Importance), weight=Importance)) +
  geom_bar() +
  ylab("Variable Importance") + 
  xlab("Variable Name") +
  coord_flip() + theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE) +
  theme_classic() + theme(legend.position = "none")
intacta_var_imp_plot

# How do our predictors influence the presence probability? Visualization.
rf_to_visualize_predictors <- randomForest(factor(leucorrhinia_intacta)~
      pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
      soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr,
      data = intacta_rf_df,
      probability = TRUE,
      mtry = 2,
      min.node.size = 10)

# ask lars... which ones should I choose??
v1<- visreg(fit_m, "pre_mm_syr", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()

v2<- visreg(fit_m, "tmp_dc_syr", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()

v3<- visreg(fit_m, "ele_mt_sav",  partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()

v4<- visreg(fit_m, "slp_dg_sav", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()

v5<- visreg(fit_m, "ari_ix_sav", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()

grid.arrange(v1,v2,v3,v4,v5,ncol=1)
