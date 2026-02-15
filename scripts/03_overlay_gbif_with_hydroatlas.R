# -----------------------------------------------------------------------------
# Author: Ryan Hull
# Date: September 2025
# Purpose: Overlay desired GBIF observations onto GeoPackage from Hydroatlas
# Output: a gpkg file containing hydroatlas + gbif data
# -----------------------------------------------------------------------------


# 1. Load Libraries
rm(list=ls())
library("sf") # good package for working with simple features, a way of working with vector data
library("ggplot2")
library("dplyr")
library("tidyr")
library("janitor")
library("mapview")
sf::sf_use_s2(FALSE) # to work on a flat earth. Should be ok for below work.
                     # when I calculate area I should use spherical



# 2. Reading in data. Convert species obs to sf:

all_odonata_obs <- read.csv("data/processed/odonata_obs_clean.csv")
all_odonata_obs_sf <- st_as_sf( # st stands for spatial type
  all_odonata_obs,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326 # standard crs - wgs84
)

NA_CA_atlas <- st_read("data/raw/NA_CA_atlas.gpkg")



# 3. Overlaying the observations on the hydroatlas

# Do they use the same crs?
st_crs(NA_CA_atlas)
st_crs(all_odonata_obs_sf) # yes wgs84

# Keep only watershed IDs where one of our observations is found. thats what the left=false does.
# note if many obs per watershed we will have a row for each. Thus we expect same # rows as obs df
# (conceptual goal is to keep all observations, and add in hydroatlas data)
odonata_obs_with_hydroatlas <- st_join(NA_CA_atlas, all_odonata_obs_sf, left=FALSE) # 2000 less obs... I guess they are outside of the watershed polygons

# tally number of observations per PFAF and include it as a column
odonata_obs_with_hydroatlas <- odonata_obs_with_hydroatlas %>%
  add_count(PFAF_ID, name = "watershed_obs_count")

# Remove duplicate PFAFs + species combos, since we only care if the species is present.
# Widen dataframe to include presence/absence cols for each species
odonata_obs_with_hydroatlas_long <-

  odonata_obs_with_hydroatlas |>
  st_drop_geometry() |>
  distinct(PFAF_ID, species) |>
  mutate(presence=1) |>
  pivot_wider(names_from=species,
              values_from=presence,
              values_fill=list(presence=0)
  ) |>
  clean_names() # replace spaces with _ in colnames. now theres one col for each species

odonata_obs_with_hydroatlas_long <- dplyr::rename(
  odonata_obs_with_hydroatlas_long,
  PFAF_ID = pfaf_id)
  

# how many different species per PFAF?
odonata_obs_with_hydroatlas_long$GBIF_species_count <-
  rowSums(odonata_obs_with_hydroatlas_long[, 2:390]) ## hard coded 390 #species here.


# Adding back in columns of interest 
# note since the original, final, and long all end up having the same number
# of rows, no PFAF-species combos were lost by the joining.
odonata_obs_with_hydroatlas <- 
  odonata_obs_with_hydroatlas[!duplicated(odonata_obs_with_hydroatlas$PFAF_ID),]

odonata_obs_with_hydroatlas_final <- left_join(
  odonata_obs_with_hydroatlas, odonata_obs_with_hydroatlas_long, by="PFAF_ID")

odonata_obs_with_hydroatlas_final$institutionCode <- NULL
odonata_obs_with_hydroatlas_final$HYBAS_ID <- NULL
odonata_obs_with_hydroatlas_final$gbifID <- NULL
odonata_obs_with_hydroatlas_final$order <- NULL
odonata_obs_with_hydroatlas_final$family <- NULL
odonata_obs_with_hydroatlas_final$genus <- NULL
odonata_obs_with_hydroatlas_final$species <- NULL
odonata_obs_with_hydroatlas_final$taxonRank <- NULL
odonata_obs_with_hydroatlas_final$countryCode <- NULL
odonata_obs_with_hydroatlas_final$stateProvince <- NULL
odonata_obs_with_hydroatlas_final$individualCount <- NULL
odonata_obs_with_hydroatlas_final$coordinateUncertaintyInMeters <- NULL
odonata_obs_with_hydroatlas_final$day <- NULL
odonata_obs_with_hydroatlas_final$month <- NULL
odonata_obs_with_hydroatlas_final$year <- NULL




# 4. Writing out our new file
st_write(odonata_obs_with_hydroatlas_final, "data/processed/odonata_hydroatlas_overlay.gpkg",
         append=FALSE)



# 5. Visualisation of obs on map of America
northamerica <- st_read("data/raw/NA_shapefile/boundaries_p_2021_v3.shp")
world <- st_read("data/raw/world_shapefile/ne_10m_admin_0_countries.shp")
obs_map <-
  ggplot() +
    geom_sf(data = world, fill = "grey90", color = "grey60") +
    geom_sf(data = odonata_obs_with_hydroatlas_final, color = "red", size = 0.6) +
    theme_minimal()

ggsave("outputs/obs_map.png",obs_map)