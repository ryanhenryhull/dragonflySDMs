# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Execute rf and capture results for a given species
# -----------------------------------------------------------------------------




# Inputs:
#   species_name
#   species_rf_df: the output of src/creating_rf_df
#   optimal hyperparameters
# Outputs:
#   species_rf_results: a table containing accuracy,
#                       false positive rate, false negative rate,
#   species_variable_importance: table showing each hydroatlas variable's 
#                                importance in the model
#   species_prediction_dataframe: will show prob. for each PFAF 




run_rf_for_one_species <- function(species_name, species_rf_df, optimal_mtry,
                                   optimal_splitrule, optimal_min_node_size){
  
  
  
  
  # 1. Creating empty data structures to contain results of our RF evaluations
  rf_accuracy <- data.frame(species_name = character(), accuracy=double(),
                            fn=double(),fp=double(),
                            stringsAsFactors=FALSE)
  prediction_dataframe <- as.data.frame(odonata_hydroatlas_overlay)[,1, drop=FALSE] # PFAF column
  colnames(prediction_dataframe) <- "PFAF"
  variable_importance <- data.frame(species_name = character(),
                                    importance=double(),
                                    varnames=character(),
                                    stringsAsFactors=FALSE)
  
  hydroatlas_variables <-
    c("pre_mm_syr","ele_mt_sav","slp_dg_sav","ari_ix_sav","tmp_dc_syr","snd_pc_sav",
        "soc_th_sav","wet_cl_smj","lka_pc_sse","dis_m3_pyr","gad_id_smj",
        "snw_pc_syr","for_pc_sse","sgr_dk_sav","aet_mm_syr","crp_pc_sse")
  
  
  
  
  # 2. Run loop to collect rf results
  for (i in 1:10){
    
    # sample training and test data randomly (no seed thus different each time)
    number_watersheds_for_training <- floor(0.75 * nrow(species_rf_df))
    training_indeces <- sample(seq_len(nrow(species_rf_df)),
                               size = number_watersheds_for_training)
    training_watersheds <- species_rf_df[training_indeces,]
    test_watersheds <- species_rf_df[-training_indeces,]
    
    # running the model and evaluating it
    rf_model <-
      ranger(
        formula = as.formula(
          paste0("factor(", species_name, ") ~ ",
                 paste(hydroatlas_variables, collapse = "+")
          )
        ),
        data = training_watersheds,
        mtry = optimal_mtry,
        splitrule = optimal_splitrule,
        min.node.size = optimal_min_node_size,
        probability = FALSE,
        importance = "impurity")
    
    pred <- predict(rf_model, test_watersheds)
    
    # assessing accuracy
    rf_accuracy[i,1] <- species_name
    rf_accuracy[i,2] <- sum(pred$predictions==test_watersheds[[species_name]])/
      length(test_watersheds[[species_name]])
    
    # False positive rate:
    rf_accuracy[i,4] <-
      sum(as.numeric(pred$predictions[which(test_watersheds[[species_name]]==0)])-1)/
      length(which(test_watersheds[[species_name]]==0))
    # how to read above:
    # sum(as.numeric(pred$predictions[which(test_watersheds[[species_name]]==0)])-1)
    # means
    #                                     (find test wtersheds where species=0)
    #                                 [convert to indeces locations (from T/F) ]
    #               (extract from pred the prediction (0 or 1) at those indeces )
    #    (as.numeric returns numbers for factors: here,1 for 0,and 2 for 1. so must -1
    # so the sum counts how many 1s were predicted in the PFAFs where there was in truth a 0.
    # then divide by the total number of test PFAFs that were 0 to get false positive rate.
    
    # False negative rate:
    true_positive_rate <- 
      sum(as.numeric(pred$predictions[which(test_watersheds[[species_name]]==1)])-1)/
      length(which(test_watersheds[[species_name]]==1))
    # note the above calculates, among test PFAFs that have species present, 
    # proportion of the places where we predicted present. thus this is the 
    # "true positive rate".
    # thus FNR:
    rf_accuracy[i,3] <- 1 - true_positive_rate
    
    
    # assessing variable importance
    # getting the values for this iteration
    iteration_importance <- as.data.frame(rf_model$variable.importance)
    iteration_importance$varnames <- rownames(iteration_importance) # turn the vars from row names to a column of values
    rownames(iteration_importance) <- NULL
    colnames(iteration_importance)[1] <- "importance"
    iteration_importance$species_name = species_name
    iteration_importance <- iteration_importance[, c("species_name","varnames","importance")] # reorder cols
    # adding it to the larger importance table
    variable_importance <- rbind(variable_importance, iteration_importance)
    
    # The previous RF was useful for training whether to classify a watershed
    # as 1 or 0 (presence/absence). Tells us about accuracy, FPRs, FNRs, var imp.
    # But we cannot use this for mapping - we need a probability of pres/absence.
    # That is what the following RF does
    probability_rf_model <- 
      ranger(
        formula = as.formula(
          paste0("factor(", species_name, ") ~ ", paste(hydroatlas_variables, collapse = "+"))
        ),
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
  
  
  
  
  # 3. Process rf results

  # accuracy, false positives, false negatives based on the non-probability rfs
  accuracy_lm <- lm(accuracy~1, data=rf_accuracy)
  fp_lm <- lm(fp~1, data=rf_accuracy)
  fn_lm <- lm(fn~1, data=rf_accuracy)
  
  # assign values
  species_rf_results <- data.frame(
    species = species_name,
    mean_accuracy = coef(accuracy_lm)[1],
    accuracy_2.5_CI = confint(accuracy_lm)[1,1],
    accuracy_97.5_CI = confint(accuracy_lm)[1,2],
    mean_fn_rate = coef(fn_lm)[1],
    fn_2.5_CI = confint(fn_lm)[1,1],
    fn_97.5_CI = confint(fn_lm)[1,2],
    mean_fp_rate = coef(fp_lm)[1],
    fp_2.5_CI = confint(fp_lm)[1,1],
    fp_97.5_CI = confint(fp_lm)[1,2])
  
  # Process spatial prediction results
  species_prediction_dataframe <- data.frame(
    PFAF = prediction_dataframe$PFAF,
    mean_prediction = rowMeans(prediction_dataframe[,2:ncol(prediction_dataframe)]),
    species = species_name
  )
  species_prediction_dataframe <- species_prediction_dataframe[,c("species", "PFAF", "mean_prediction")]
  
  # for variable importance, simply return what the above loop provides
    
  
  
  
  # 4. Return results
  return(list(species_rf_results = species_rf_results,
              species_variable_importance = variable_importance,
              species_prediction_dataframe = species_prediction_dataframe))
}

# note the visualization of what PFAFs produce good predictions 
# (from prediction dataframe) will be best done with table produced from combination
# with all species

# the range maps themselves we can make for each species individually based on 
# species_rf_results in some other loop if we wish

# variable importance - prob wanna do average across species, lets do that in
# another script