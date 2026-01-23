---
title: "README"
output: html_document
---

## odonata_obs_clean.csv
output of 01_full_odonata_gbif_processing.R

## odonata_species_list_with_obs.csv
Selected species and associated number of observations
WITHOUT mexico and central american obs

## full_odonata_species_list_with_obs
Selected species and associated number of observations
WITH mexico and central american obs
output of 01_full_odonata_gbif_processing.R

## ten_random_species_rf_training_results.csv
All possible hyperparameter combinations and associated accuracies for ten 
randomly selected species

## optimal_hyperparameterization.csv
The combinations of hyperparameters that led to the most accuracy for those 
ten species, and the overall average optimal combination from these.

## odonata_hydroatlas_overlay.gpkg
product of 03_overlay_gbif_with_hydroatlas.R