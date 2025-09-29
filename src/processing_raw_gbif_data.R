# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Process raw gbif data for Leucorrhinia observations in Canda and USA
# Output: a gpkg file containing hydroatlas + gbif data
# -----------------------------------------------------------------------------

# 1. loading packages
library(readr)

# 2. reading in all occurrences
# Download reference: GBIF.org (24 September 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.4ynuga
whitefaces_obs <- read_tsv("data/north_american_whiteface_occurences_gbif_24sep2025.csv")
all_odonata_obs <- read_tsv("data/gbif_NA_odonata.csv")

# 3. Keep only wanted columns
whitefaces_obs <- whitefaces_obs[c("gbifID","species","taxonRank","countryCode",
                            "stateProvince","individualCount","decimalLatitude",
                            "decimalLongitude","coordinateUncertaintyInMeters",
                            "day","month","year","institutionCode")]
all_odonata_obs <- all_odonata_obs[c("gbifID","order","family","genus","species",
                                     "taxonRank","countryCode", "stateProvince",
                                     "individualCount","decimalLatitude",
                                     "decimalLongitude","coordinateUncertaintyInMeters",
                                     "day","month","year","institutionCode")]


# 4. Cleaning out observations whose locations are too uncertain (maybe later)
summary(all_odonata_obs$coordinateUncertaintyInMeters)
# can remove all that have coordinate uncertainty >10km since gridcell is 10x10
# remove duplicates.... maybe

# 5. Subsetting whitefaces by species:
borealis_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia borealis",]
hudsonica_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia hudsonica",]
intacta_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia intacta",] # most observations
frigida_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia frigida",]
proxima_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia proxima",]
dubia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia dubia",]
glacialis_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia glacialis",]
intermedia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia intermedia",]
patricia_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia patricia",]
# european rubicunda_obs <- whitefaces_obs[whitefaces_obs$species=="Leucorrhinia rubicunda",]
genus_level_obs <- whitefaces_obs[whitefaces_obs$taxonRank=="GENUS",]

# 6. Writing out processed data of choice:
write.csv(intacta_obs, "data/intacta_obs.csv", row.names=FALSE)
write.csv(proxima_obs, "data/proxima_obs.csv", row.names=FALSE)
write.csv(all_odonata_obs, "data/all_odonata_obs.csv", row.names=FALSE)