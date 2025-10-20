# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Process raw gbif data for Leucorrhinia observations in Canda and USA
# Output: A dataset of cleaned Leucorrhinia observations.
# -----------------------------------------------------------------------------



# 1. loading packages
rm(list=ls())
library(readr)



# 2. Data - reading in all occurrences
whiteface_obs <- read_tsv("data/gbif_whiteface_observations_24sept.csv")



# 3. Data Cleaning
# keep only useful columns
whiteface_obs <- whiteface_obs[c("gbifID","order","family","genus","species",
                                 "taxonRank","countryCode", "stateProvince",
                                 "individualCount","decimalLatitude",
                                 "decimalLongitude","coordinateUncertaintyInMeters",
                                 "day","month","year","institutionCode")]

# Cleaning out observations with no location
whiteface_obs <-
  whiteface_obs[!is.na(whiteface_obs$decimalLatitude) & !is.na(whiteface_obs$decimalLongitude),]

# Cleaning out observations whose locations are too uncertain (we're using 10x10km gridcells)
whiteface_obs <-
  whiteface_obs[is.na(whiteface_obs$coordinateUncertaintyInMeters) | 
                  whiteface_obs$coordinateUncertaintyInMeters < 10000,]

# Keep only the last 25 years of observations
whiteface_obs <- 
  whiteface_obs[whiteface_obs$year >= 2000,]



# 4. Subsetting whitefaces by species:

# borealis_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia borealis",]
# hudsonica_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia hudsonica",]
intacta_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia intacta",] # most observations
# frigida_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia frigida",]
# proxima_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia proxima",]
# dubia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia dubia",]
# glacialis_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia glacialis",]
# intermedia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia intermedia",]
# patricia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia patricia",]
# genus_level_obs <- whitefaces_obs[whitefaces_obs$taxonRank=="GENUS",]



# 5. Writing out processed data of choice:
write.csv(intacta_obs, "data/intacta_obs.csv", row.names=FALSE)