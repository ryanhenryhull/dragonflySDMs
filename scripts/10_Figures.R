# Author: Ryan Hull
# Date: February 2026
# Purpose: Generate figures for poster presentation of research project



# 1. Libraries
rm(list=ls())
library(ggplot2)
library(lattice)
library(sf)
library(dplyr)



# 2. Data
rf_predictions <- read.csv("data/results/odonata_rf_predictions.csv")
leucorrhinia_intacta_predictions <- rf_predictions[
  rf_predictions$species == "leucorrhinia_intacta",]

hydroatlas <- st_read("data/raw/NA_CA_atlas.gpkg")
hydroatlas <- hydroatlas[,c("PFAF_ID","geom")]
colnames(hydroatlas)[colnames(hydroatlas)=="PFAF_ID"] <- "PFAF"



# 3. Simple map of dragonfly distributions

# joining them 
leucorrhinia_intacta_pred_with_geom <- 
  leucorrhinia_intacta_predictions %>%
  left_join(hydroatlas, by="PFAF") %>%
  st_as_sf()
  


ggplot(rf_preditions, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_minimal()