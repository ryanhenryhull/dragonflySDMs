# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Execute rf and capture results for a given species
# -----------------------------------------------------------------------------




# Inputs:
#   species_rf_df: the output of src/creating_rf_df
# Outputs:
#   a table containing accuracy, false positive rate, false negative rate,




run_rf_for_one_species <- function(species_rf_df, optimal_mtry,
                                   optimal_splitrule, optimal_min_node_size){
  
  
  
  
  # 1. Creating empty data structures to contain results of our RF evaluations
  rf_accuracy <- data.frame(accuracy=double(),fn=double(),fp=double(),
                            data=character(),
                            stringsAsFactors=FALSE)
  prediction_dataframe <- as.data.frame(odonata_hydroatlas_overlay)[,1] # PFAF column
  variable_importance <- data.frame(importance=double(),varnames=character(), stringsAsFactors=FALSE)
  
  species_name <- ...
  
  
  
  
  # 2. Run loop to collect rf results
  for (i in 1:10){
    
    # sample training and test data randomly (no seed thus different each time)
    number_watersheds_for_training <- floor(0.75 * nrow(species_rf_df))
    training_indeces <- sample(seq_len(nrow(species_rf_df)),
                               size = number_watersheds_for_training)
    training_watersheds <- intacta_rf_df[training_indeces,]
    test_watersheds <- intacta_rf_df[-training_indeces,]
    
    # running the model and evaluating it
    rf_model <-
      ranger(factor(species_name)~ # may need to paste actual name?
            pre_mm_syr+ele_mt_sav+slp_dg_sav+ari_ix_sav+tmp_dc_syr+snd_pc_sav+
            soc_th_sav+wet_cl_smj+lka_pc_sse+dis_m3_pyr+gad_id_smj+
            snw_pc_syr+for_pc_sse+sgr_dk_sav+aet_mm_syr+crp_pc_sse,
            data = training_watersheds,
            mtry = optimal_mtry,
            splitrul = optimal_splitrule,
            min.node.size = optimal_min_node_size,
            probability = FALSE)
    
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
  
  
  
  
  # 3. Process rf performance results
  species_rf_performance_results <- data.frame(
    species = character(),
    mean_accuracy = double(),
    accuracy_2.5_CI = double(),
    accuracy_97.5_CI = double(),
    mean_fn_rate = double(),
    fn_2.5_CI = double(),
    fn_97.5_CI = double(),
    mean_fp_rate = double(),
    fp_2.5_CI = double(),
    fp_97.5_CI = double(),)
  
  # accuracy
  accuracy_lm <- lm(accuracy~1, data=rf_accuracy)
  coef(accuracy_lm) # mean accuracy
  confint(accuracy_lm) # 2.5 and 97.5% confidence intervals
  
  # false positives
  fp_lm <- lm(fp~1, data=rf_accuracy)
  coef(fp_lm)
  confint(fp_lm)
  
  # false negatives
  fn_lm <- lm(fn~1, data=rf_accuracy)
  coef(fn_lm)
  confint(fn_lm)
  
  # assign values
  species_rf_results$species <- species_name
  species_rf_results$mean_accuracy <- coef(accuracy_lm)
  species_rf_results$accuracy_2.5_CI <- confint(accuracy_lm)[1,1]
  species_rf_results$accuracy_97.5_CI <- confint(accuracy_lm)[1,2]
  species_rf_results$mean_fn_rate <- coef(fn_lm)
  species_rf_results$fn_2.5_CI <- confint(fn_lm)[1,1]
  species_rf_results$fn_97.5_CI <- confint(fn_lm)[1,2]
  species_rf_results$mean_fp_rate <- coef(fp_lm)
  species_rf_results$fp_2.5_CI <- confint(fp_lm)[1,1]
  species_rf_results$fp_97.5_CI <- confint(fp_lm)[1,2]
  
  
  
  
  # 4. Process variable importance results
  species_variable_importance <- data.frame(
    species = character(), varnames = character(), mean_importance = double()
  )
  
  
  
  
  # 5. Process spatial prediction results
  species_prediction_dataframe <- data.frame(
    species = species_name,
    PFAF = prediction_dataframe$prediction_dataframe,
    mean_prediction = rowMeans(prediction_dataframe[,2:ncol(prediction_dataframe)])
  )
    
  
  
  
  # 6. Return results
  
}

# up next: see if we need variable importance, and find way to return map
# into an organized folder structure with following code

odonata_hydroatlas_overlay <- st_make_valid(odonata_hydroatlas_overlay)

intacta_model1 <- ggplot() +
  geom_sf(data = odonata_hydroatlas_overlay, aes(fill = intacta_prediction, colour=intacta_prediction)) +
  scale_fill_viridis() +
  scale_colour_viridis()+
  theme_bw()
intacta_model1

ggsave(intacta_model1, filename="outputs/intacta_map.png")