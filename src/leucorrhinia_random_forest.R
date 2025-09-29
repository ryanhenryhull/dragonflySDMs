# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Run random forest model to assess habitat suitability of north
# american whitefaces
# Output: 
# Based on: Lars Iversen milfoil experiment random forest code
# -----------------------------------------------------------------------------

# 1. Packages
library(sf)
library(ranger)
library(rcompanion)
library(kernelshap)
library(shapviz)
library(caret)
library(ggplot2)
library(viridis)
library(gridExtra)
library(dplyr)
library(lessR)
library(visreg)
library(randomForest)
# 2. Initial data processing
odonata_hydroatlas_overlay <- st_read("data/odonata_hydroatlas_overlay.gpkg")

intacta_presence_hydroatlas <- odonata_hydroatlas_overlay[which(odonata_hydroatlas_overlay$leucorrhinia_intacta==1),]
intacta_absence_hydroatlas <- odonata_hydroatlas_overlay[which(odonata_hydroatlas_overlay$leucorrhinia_intacta==0),]

# assigning weights to the absence watersheds based on dragonfly sampling effort:
total_odonata_species <- ncol(odonata_hydroatlas_overlay) - 2
intacta_absence_hydroatlas$prob <- 
  #intacta_absence_hydroatlas$GBIF_species_count/sum(no_milfoil$GBIF_species_count)
  intacta_absence_hydroatlas$GBIF_species_count/total_odonata_species
