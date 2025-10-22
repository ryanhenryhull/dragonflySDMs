# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: Process GBIF data to see observations for all
#           dragonfly species in North America.
# Output: A dataset of cleaned observations for all model-suitable dragonflies
# -----------------------------------------------------------------------------



# 1. Loading packages
rm(list=ls())
library(readr)
library(dplyr)



# 2. Data cleaning
all_odonata_obs <- read_tsv("data/raw/gbif_NA_odonata.csv")

# keep useful columns
all_odonata_obs <- all_odonata_obs[c("gbifID","order","family","genus","species",
                                     "taxonRank","countryCode", "stateProvince",
                                     "individualCount","decimalLatitude",
                                     "decimalLongitude","coordinateUncertaintyInMeters",
                                     "day","month","year","institutionCode")]

# Cleaning out observations with no location
all_odonata_obs <-
  all_odonata_obs[!is.na(all_odonata_obs$decimalLatitude) & !is.na(all_odonata_obs$decimalLongitude),]

# Keep only observations whose coordinate uncertainty doesn't exceed our gridsize
all_odonata_obs <- 
  all_odonata_obs[is.na(all_odonata_obs$coordinateUncertaintyInMeters) |
                    all_odonata_obs$coordinateUncertaintyInMeters < 10000,]

# Keep only the last 25 years of observations
all_odonata_obs <- 
  all_odonata_obs[all_odonata_obs$year >= 2000,]

# Keep only species level observations
all_odonata_obs <- all_odonata_obs[which(all_odonata_obs$taxonRank=="SPECIES"), ]

# Keep only species that occur in Canada? Or not.... or only species that have a certain delta-lat??



# 3. Making a species table to see #obs per species and select those that >100 obs

species_counts <- all_odonata_obs %>%
  group_by(species) %>%
  summarise(observations = n())   # count rows per species

qualified_species <- species_counts[species_counts$observations>=100,]

# opportunity for making cool visual here. Potentially collapsibletree.......... could be really cool



# 4. Reducing dataframe to qualified species and adding num_obs_species column

# 161 species were unqualified, hence we should lose ~50*161 rows,
# on the order of 5000-15000rows. looks like we lose 6000so checks out.
qualified_odonata_obs <-
  all_odonata_obs[all_odonata_obs$species %in% qualified_species$species,]

qualified_odonata_obs <- merge(
  qualified_odonata_obs,
  qualified_species,
  by = "species",
  all.x = TRUE # keeps all qualified_odonata_obs ie like a left join
)

qualified_odonata_obs <- rename(qualified_odonata_obs, species_obs_count = observations)



# 5. Writing out data

write.csv(qualified_odonata_obs, "data/processed/all_odonata_obs_clean.csv", row.names=FALSE)
write.csv(qualified_species, "data/processed/odonata_species_list_with_obs.csv", row.names=FALSE)
