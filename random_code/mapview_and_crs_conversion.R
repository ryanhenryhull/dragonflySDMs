
 # 4. Overlaying ecoregions & constructing new column
 ecoregions <- st_read("data/raw/terrestrial_ecoregions_level_1_shapefile/terr_ecoregions_v2_level_i_shapefile/NA_TerrEcoregions_I/data/NA_Terrestrial_Ecoregions_v2_level1.shp")
 st_crs(ecoregions) #Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area, EPSG 1027
 ecoregions$NameL1_Es <- NULL
 ecoregions$NameL1_Fr <- NULL
 
 
 # since we will need to calculate area later, might as well shift from wgs84 to 
 # the lambert azimuthal equal area (LAEA). I don't believe this will change the topology
 # (ie overlay) manipulations
 st_crs(odonata_obs_with_hydroatlas_final) # wgs84
 odonata_obs_with_hydroatlas_final <- st_transform(
   odonata_obs_with_hydroatlas_final,
   st_crs(ecoregions) # LAEA
 )
 odonata_obs_with_hydroatlas_final <- st_transform(
   odonata_obs_with_hydroatlas_final,
   st_crs(CAN_USA_atlas) # LAEA
 )
 st_crs(odonata_obs_with_hydroatlas_final) # now LAEA
 
 mapView(
   x = ecoregions,
   zcol = "NameL1_En",
   col.regions = rainbow(length(unique(ecoregions$NameL1_En)))
 )
 mapView(
   x=odonata_obs_with_hydroatlas_final,
 )
 #the actual overlay:
 
 pfaf_centroids <- st_centroid(odonata_obs_with_hydroatlas_final$geom)