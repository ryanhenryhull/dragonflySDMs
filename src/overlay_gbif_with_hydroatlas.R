# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Overlay desired GBIF observations onto GeoPackage from Hydroatlas
# Output: a gpkg file containing hydroatlas + gbif data
# -----------------------------------------------------------------------------

# 1. Load Libraries
library("sf") ## a major package for geospatial data - encoded as "simple features" in R. both shapefiles and gpkgs will be treated as simple features.
library("ggplot2")
library("dplyr")
library("tidyr")
library("janitor")
sf::sf_use_s2(FALSE) # to work on a flat earth - ask lars about this

# 2. Reading in species observations containing coordinates and convert them to sf:
intacta_obs <- read.csv("data/intacta_obs.csv")
intacta_obs <- intacta_obs[!is.na(intacta_obs$decimalLatitude),]
intacta_obs <- intacta_obs[!is.na(intacta_obs$decimalLongitude),]
intacta_obs_sf <- st_as_sf(
  intacta_obs,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326 # the standard coord system
)

proxima_obs <- read.csv("data/proxima_obs.csv")
proxima_obs <- proxima_obs[!is.na(proxima_obs$decimalLatitude),]
proxima_obs <- proxima_obs[!is.na(proxima_obs$decimalLongitude),]
proxima_obs_sf <- st_as_sf(
  proxima_obs,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326 # the standard coord system
)

all_odonata_obs <- read.csv("data/all_odonata_obs.csv")
all_odonata_obs <- all_odonata_obs[!is.na(all_odonata_obs$decimalLatitude),]
all_odonata_obs <- all_odonata_obs[!is.na(all_odonata_obs$decimalLongitude),]
all_odonata_obs <- st_as_sf(
  all_odonata_obs,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326 # the standard coord system
)

# 3. Reading in the hydroatlas data:
CAN_USA_atlas <- st_read("data/CAN_USA_atlas.gpkg")


# 4. Overlaying the observations on the hydroatlas

# Do they use the same crs?
st_crs(CAN_USA_atlas)
st_crs(all_odonata_obs)

# What watersheds contain our dragonflies? Only watershed IDs where one of our 
# observations is found are kept, and possibly multiplicated if there are 
# multiple observations within that polygon.
odonata_obs_with_hydroatlas <- st_join(CAN_USA_atlas, all_odonata_obs, left=FALSE)

# Remove unneeded geometry data.
# Remove duplicate PFAFs + species combos, since we only care if the species is present.
# Widen dataframe to include presence/absence cols for each species
odonata_obs_with_hydroatlas <-

  odonata_obs_with_hydroatlas |>
  st_drop_geometry() |>
  distinct(PFAF_ID, species) |>
  mutate(presence=1) |>
  pivot_wider(names_from=species,
              values_from=presence,
              values_fill=list(presence=0)
  ) |>
  clean_names() # replace spaces with _ in colnames

odonata_obs_with_hydroatlas$GBIF_species_count <-
  rowSums(odonata_obs_with_hydroatlas[, 2:474])

# 5. Writing out our new file
st_write(odonata_obs_with_hydroatlas, "data/odonata_hydroatlas_overlay.gpkg")
