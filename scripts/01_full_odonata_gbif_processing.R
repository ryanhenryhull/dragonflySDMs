# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: Process GBIF data to obtain clean, select observations for all
#         dragonfly species in North America.
# Output: A dataset of cleaned observations for all model-suitable dragonflies
# -----------------------------------------------------------------------------




# 1. Loading packages
rm(list=ls())
library(readr)
library(dplyr)
library(data.table)




# 2. Data cleaning - US/CAN observations
can_usa_odonata_obs  <- fread("data/raw/gbif_USCAN_odonata.csv", showProgress=TRUE) #fixes read_tsv error of only reading first 400 000 lines due to (gbif syntax error?)
can_usa_odonata_obs <- as.data.frame(can_usa_odonata_obs)

# keep useful columns
can_usa_odonata_obs <- can_usa_odonata_obs[c("gbifID","order","family","genus","species",
                                     "taxonRank","countryCode", "stateProvince",
                                     "individualCount","decimalLatitude",
                                     "decimalLongitude","coordinateUncertaintyInMeters",
                                     "day","month","year","institutionCode")]

# Cleaning out observations with no location
can_usa_odonata_obs <-
  can_usa_odonata_obs[!is.na(can_usa_odonata_obs$decimalLatitude) & !is.na(can_usa_odonata_obs$decimalLongitude),]

# Keep only observations whose coordinate uncertainty doesn't exceed our gridsize
can_usa_odonata_obs <- 
  can_usa_odonata_obs[is.na(can_usa_odonata_obs$coordinateUncertaintyInMeters) |
                    can_usa_odonata_obs$coordinateUncertaintyInMeters < 500,]

# Keep only the last 25 years of observations
can_usa_odonata_obs <- 
  can_usa_odonata_obs[can_usa_odonata_obs$year >= 2000,]

# Keep only species level observations
can_usa_odonata_obs <- can_usa_odonata_obs[which(can_usa_odonata_obs$taxonRank=="SPECIES"), ]




# 3. Data cleaning - Mexico / Central America observations
# plus forgotten honduras observations

mex_cen_odonata_obs <- fread("data/raw/gbif_mexico_central_america_odonata.csv")
mex_cen_odonata_obs <- as.data.frame(mex_cen_odonata_obs)

mex_cen_odonata_obs <- mex_cen_odonata_obs[c("gbifID","order","family","genus","species",
                                             "taxonRank","countryCode", "stateProvince",
                                             "individualCount","decimalLatitude",
                                             "decimalLongitude","coordinateUncertaintyInMeters",
                                             "day","month","year","institutionCode")]
mex_cen_odonata_obs <-
  mex_cen_odonata_obs[!is.na(mex_cen_odonata_obs$decimalLatitude) & !is.na(mex_cen_odonata_obs$decimalLongitude),]
mex_cen_odonata_obs <- 
  mex_cen_odonata_obs[is.na(mex_cen_odonata_obs$coordinateUncertaintyInMeters) |
                        mex_cen_odonata_obs$coordinateUncertaintyInMeters < 10000,]
mex_cen_odonata_obs <- 
  mex_cen_odonata_obs[mex_cen_odonata_obs$year >= 2000,]
mex_cen_odonata_obs <- mex_cen_odonata_obs[which(mex_cen_odonata_obs$taxonRank=="SPECIES"), ]




honduras_odonata_obs <- fread("data/raw/gbif_honduras_odonata.csv")
honduras_odonata_obs <- as.data.frame(honduras_odonata_obs)

honduras_odonata_obs <- honduras_odonata_obs[c("gbifID","order","family","genus","species",
                                             "taxonRank","countryCode", "stateProvince",
                                             "individualCount","decimalLatitude",
                                             "decimalLongitude","coordinateUncertaintyInMeters",
                                             "day","month","year","institutionCode")]
honduras_odonata_obs <-
  honduras_odonata_obs[!is.na(honduras_odonata_obs$decimalLatitude) & !is.na(honduras_odonata_obs$decimalLongitude),]
honduras_odonata_obs <- 
  honduras_odonata_obs[is.na(honduras_odonata_obs$coordinateUncertaintyInMeters) |
                        honduras_odonata_obs$coordinateUncertaintyInMeters < 10000,]
honduras_odonata_obs <- 
  honduras_odonata_obs[honduras_odonata_obs$year >= 2000,]
honduras_odonata_obs <- honduras_odonata_obs[which(honduras_odonata_obs$taxonRank=="SPECIES"), ]



# 4. Making a species table to see #obs per species
#    in USACAN, and retaining those that >=100 obs

species_counts <- can_usa_odonata_obs %>%
  group_by(species) %>%
  summarise(observations = n())   # count rows per species

qualified_species <- species_counts[species_counts$observations>=100,]

# opportunity for making cool visual here. Potentially collapsibletree.......... could be really cool




# 5. Reducing obs dataframes to qualified species & adding num_obs_species column

# 161 species were unqualified, hence we should lose ~50*161 rows,
# on the order of 5000-15000rows. looks like we lose 6000, so checks out.
qualified_uscan_odonata_obs <-
  can_usa_odonata_obs[can_usa_odonata_obs$species %in% qualified_species$species,]

# do the same for mexican/c.a. obs:
qualified_mex_cen_obs <-
  mex_cen_odonata_obs[mex_cen_odonata_obs$species %in% qualified_species$species,]

# do the same for forgotten honduras
qualified_honduras_obs <-
  honduras_odonata_obs[honduras_odonata_obs$species %in% qualified_species$species,]

# Merge them all and recount species observations with the added obs
qualified_all_obs <- rbind(qualified_uscan_odonata_obs, qualified_mex_cen_obs, qualified_honduras_obs)

qualified_species_obs_counts_with_mex_cen_honduras <- qualified_all_obs %>%
  group_by(species) %>%
  summarise(observations = n())   # count rows per species

qualified_all_obs <- merge(
  qualified_all_obs,
  qualified_species_obs_counts_with_mex_cen_honduras,
  by = "species",
  all.x = TRUE # keeps all qualified_all_obs ie like a left join
)
qualified_all_obs <- rename(qualified_all_obs, species_obs_count = observations)





# 6. Writing out data
write.csv(qualified_all_obs, "data/processed/odonata_obs_clean.csv", row.names=FALSE)
write.csv(qualified_species_obs_counts_with_mex_cen_honduras, "data/processed/full_odonata_species_list_with_obs.csv", row.names=FALSE)

