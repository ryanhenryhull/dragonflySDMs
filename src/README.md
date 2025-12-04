---
title: "README"
output: html_document
---

# What's in src/ ?
These files will not be ran directly. They will either be called by scripts/, or are simply versions of code that won't be used in the main process.

## leucorrhinia_gbif_processing.R
A previous version of scripts/01_full_odonata_gbif_processing for just the family

## Iversen_milfoil_rf_short.R
Prof. Iversen's rf code from a previous study

## leucorrhinia_random_forest.R
The first run at RF using a well observed whiteface.
Note this was a rough run with mistakes. 
For instance, no need to run RF once before the for(1:10) runs. Lars had done that to compare 2 functions.
For instance, I forgot to add the optimal hyperparameterization to the 1:10 for 
the probability=false runs.

## creating_rf_df.R
Given a large GBIF obs - hydroatlas overlay, trims down to given species of interest and prepares dataframe to be used in RF model.

## run_rf_for_one_species.R
Use the rf_df to run model for a given species. returns the performance results for 
the model, variable importance results, and a dataframe of spatial predictions.
