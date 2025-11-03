---
title: "README"
output: html_document
---

# What's in src/ ?

These files will not be ran directly. They will either be called by scripts/, or are simply versions of code that won't be used in the main process.

## leucorrhinia_gbif_processing.R

A previous version of scripts/01_full_odonata_gbif_processing for just the family

## comparing_intacta_mydownload_larsdownload.R

I was skeptical about the difference between our two downloads

## Iversen_milfoil_rf_short.R

Prof. Iversen's rf code from a previous study

## leucorrhinia_random_forest.R

The first run at RF using a well observed whiteface

## creating_rf_df
Given a large GBIF obs - hydroatlas overlay, trims down to given species of
interest and prepares dataframe to be used in RF model.
