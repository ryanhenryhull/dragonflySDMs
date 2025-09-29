# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Process raw gbif data for Leucorrhinia observations in Canda and USA
# Output: a gpkg file containing hydroatlas + gbif data
# -----------------------------------------------------------------------------

# 1. loading packages
library(readr)

# 2. Data - reading in all occurrences
# Download reference: GBIF.org (24 September 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.4ynuga
rm(list=ls())
all_odonata_obs <- read_tsv("data/gbif_NA_odonata.csv")
whiteface_obs <- read_tsv("data/gbif_whiteface_observations_24sept.csv")

# 3. Data Cleaning
# keep only useful columns
all_odonata_obs <- all_odonata_obs[c("gbifID","order","family","genus","species",
                                     "taxonRank","countryCode", "stateProvince",
                                     "individualCount","decimalLatitude",
                                     "decimalLongitude","coordinateUncertaintyInMeters",
                                     "day","month","year","institutionCode")]
whiteface_obs <- whiteface_obs[c("gbifID","order","family","genus","species",
                                 "taxonRank","countryCode", "stateProvince",
                                 "individualCount","decimalLatitude",
                                 "decimalLongitude","coordinateUncertaintyInMeters",
                                 "day","month","year","institutionCode")]

# Cleaning out observations with no location
whiteface_obs <-
  whiteface_obs[!is.na(whiteface_obs$decimalLatitude) & !is.na(whiteface_obs$decimalLongitude),]
all_odonata_obs <-
  all_odonata_obs[!is.na(all_odonata_obs$decimalLatitude) & !is.na(all_odonata_obs$decimalLongitude),]

# Cleaning out observations whose locations are too uncertain (we're using 10x10km gridcells)
summary(all_odonata_obs$coordinateUncertaintyInMeters)

all_odonata_obs <- 
  all_odonata_obs[is.na(all_odonata_obs$coordinateUncertaintyInMeters) |
                  all_odonata_obs$coordinateUncertaintyInMeters < 10000,]
whiteface_obs <-
  whiteface_obs[is.na(whiteface_obs$coordinateUncertaintyInMeters) | 
                  whiteface_obs$coordinateUncertaintyInMeters < 10000,]

summary(all_odonata_obs$coordinateUncertaintyInMeters)

# Keep only the last 25 years of observations
all_odonata_obs <- 
  all_odonata_obs[all_odonata_obs$year >= 2000,]
whiteface_obs <- 
  whiteface_obs[whiteface_obs$year >= 2000,]


# 4. Subsetting whitefaces by species:

# borealis_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia borealis",]
# hudsonica_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia hudsonica",]
# intacta_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia intacta",] # most observations
# frigida_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia frigida",]
# proxima_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia proxima",]
# dubia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia dubia",]
# glacialis_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia glacialis",]
# intermedia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia intermedia",]
# patricia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia patricia",]
# genus_level_obs <- whitefaces_obs[whitefaces_obs$taxonRank=="GENUS",]

intacta_obs_from_lars_list <- all_odonata_obs[!is.na(all_odonata_obs$species) & 
                                 all_odonata_obs$species == "Leucorrhinia intacta",]
intacta_obs_from_my_list <-
  whiteface_obs[!is.na(whiteface_obs$species) &
                  whiteface_obs$species == "Leucorrhinia intacta",]

# 6. Writing out processed data of choice:
write.csv(intacta_obs_from_my_list, "data/intacta_obs.csv", row.names=FALSE)
write.csv(all_odonata_obs, "data/all_odonata_obs.csv", row.names=FALSE)
