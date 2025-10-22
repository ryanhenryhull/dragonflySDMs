---
title: "README"
output: html_document
---

# What's in scripts/ ?

This is code that is directly ran to produce this project's outputs.

## 01_full_odonata_gbif_processing.R

To process all USA/CAN dragonfly observations.

## 02_hydroatlas_processing.R
To process initial hydroatlas shapefile into refined geopackage.

## 03_overay_gbif_with_hydroatlas.R

To make a single spatial file from the hydroatlas and the processed observations

## 04_setting_rf_hyperparameters.R

to find a middle ground rf hyperparamaterization before we run the model over all dragonflies.

## 05_odonata_randomforest_loop.R

To run the model over all qualifying odonates
