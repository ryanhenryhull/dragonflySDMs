rm(list=ls())
species_list <- read.csv("data/processed/odonata_species_list_with_obs.csv")
max(species_list$observations)
min(species_list$observations)


all_odonata_obs <- read.csv("data/raw/gbif_NA_odonata.csv",header=TRUE)
