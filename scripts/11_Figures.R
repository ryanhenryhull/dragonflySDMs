################################################################################
# Author: Ryan Hull
# Date: February 2026
# Purpose: Generate figures for poster presentation of research project. Much
#          is recycled from previous scripts
################################################################################




# 1. Libraries
rm(list=ls())
library(ggplot2)
library(lattice)
library(sf)
library(dplyr)
library(car)




# 2. Data
rf_predictions <- read.csv("data/results/odonata_rf_predictions.csv")
leucorrhinia_intacta_predictions <- rf_predictions[
  rf_predictions$species == "leucorrhinia_intacta",]


rf_results_with_lat_data <- read.csv("data/results/odonata_rf_performance_with_latitude_stats.csv")



hydroatlas <- st_read("data/raw/NA_CA_atlas.gpkg")
hydroatlas <- hydroatlas[,c("PFAF_ID","geom")]
colnames(hydroatlas)[colnames(hydroatlas)=="PFAF_ID"] <- "PFAF"




# 3. Simple map of dragonfly distributions

# joining them 
leucorrhinia_intacta_pred_with_geom <- 
  leucorrhinia_intacta_predictions %>%
  left_join(hydroatlas, by="PFAF") %>%
  st_as_sf()
  

#?
ggplot(rf_preditions, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_minimal()




# 5. 2D partial dependence plot of influence of obs density and centroid lat
#    on rf accuracy, based on multiple regression with interaction values
#    (taken from 08_)
lat_logdensity_accuracy_multiple_regression <-
  lm(data=rf_results_with_lat_data,
     mean_accuracy ~ centroid_decimalLat * log(observation_density_obs_per_km2))
summary(lat_logdensity_accuracy_multiple_regression)

avPlots(lat_logdensity_accuracy_multiple_regression) # crappy visual

# create grid of density and lat values, run predictions for each combo, then plot as 2D partial dependence plot
lat_seq <- seq(min(rf_results_with_lat_data$centroid_decimalLat),
               max(rf_results_with_lat_data$centroid_decimalLat),
               length.out=100) #creates 100 evenspaced values between min and max centroid lat

density_seq <- seq(min(rf_results_with_lat_data$observation_density_obs_per_km2),
                   max(rf_results_with_lat_data$observation_density_obs_per_km2),
                   length.out=100)

lat_density_grid <- expand.grid(
  centroid_decimalLat = lat_seq,
  observation_density_obs_per_km2 = density_seq)

lat_density_grid$accuracy_prediction <-
  predict(lat_logdensity_accuracy_multiple_regression, newdata=lat_density_grid)

ggplot(data=lat_density_grid,
       mapping=aes(x=centroid_decimalLat,y=observation_density_obs_per_km2,fill=accuracy_prediction))+
  geom_tile()+ # creates the surface of color pixels needed for heatmap
  labs(x="Centroid Latitude",y="Observation Density",fill="Predicted Accuracy")

# it would be better to draw contour lines over discrete value ranges to visualize better:
partial_dependence_plot_2d <-
  ggplot(data=lat_density_grid,
         mapping=aes(x=centroid_decimalLat,y=observation_density_obs_per_km2,z=accuracy_prediction))+
  geom_tile()+ # creates the surface of color pixels needed for heatmap
  geom_contour_filled(bins=10)+
  labs(x="Centroid Latitude (°)",y="Observation Density (obs/km2)",fill="Predicted Accuracy")+
  theme_minimal()+
  theme(panel.grid=element_blank(), # remove gridlines
        axis.text.x = element_text(angle=45, hjust=1),  # tilt xaxis numbers
        theme(aspect.ratio=1))+ # make it square
  scale_x_continuous(expand=c(0,0))+ # these remove the padding between axes numbers and the heatmap
  scale_y_continuous(expand=c(0,0))

partial_dependence_plot_2d

# the 2D partial dependence plot
ggsave("outputs/rf_2D_partial_dependence_plot_meanlat_obsdensity_rfaccuracy.png",
       plot=partial_dependence_plot_2d, width=7, height=5)




# with log y axis: will better represent distribution of species in the lat-dens space
partial_dependence_plot_2d_w_log <-
  ggplot(data=lat_density_grid,
         mapping=aes(x=centroid_decimalLat,y=observation_density_obs_per_km2,z=accuracy_prediction))+
  geom_tile()+ # creates the surface of color pixels needed for heatmap
  geom_contour_filled(bins=10)+
  labs(x="Centroid Latitude (°)",y="Observation Density (obs/km2)",fill="Predicted Accuracy")+
  theme_minimal()+
  theme(panel.grid=element_blank(), # remove gridlines
        axis.text.x = element_text(angle=45, hjust=1),  # tilt xaxis numbers
        theme(aspect.ratio=1))+ # make it square
  scale_x_continuous(expand=c(0,0))+ # these remove the padding between axes numbers and the heatmap
  scale_y_continuous(expand=c(0,0)) +
  scale_y_continuous(
    expand = c(0, 0), 
    trans = "log10",  # Apply log10 transformation to the y-axis
    labels = scales::trans_format("log10", scales::math_format(10^.x))  # Format y-axis labels in log scale
  )

partial_dependence_plot_2d_w_log 

# save it 
ggsave("outputs/rf_2D_partial_dependence_plot_meanlat_obsdensity_rfaccuracy_with_log_axis.png",
       plot=partial_dependence_plot_2d_w_log, width=7, height=5)





# 6. Create heat map of where good predictions can be made.
rf_performance_results <- read.csv("data/results/odonata_rf_performance_with_latitude_stats.csv") # not useful here
rf_predictions <- read.csv("data/results/odonata_rf_predictions.csv")
species_list <- read.csv("data/processed/full_odonata_species_list_with_obs.csv")
overlay <- st_read("data/processed/odonata_hydroatlas_overlay.gpkg")
colnames(overlay)[1] <- "PFAF"

# the overlay is in wide format (one row per PFAF, columns for each species)
# the predictions is in long format (a species column and a PFAF column, thus
#                                    each species has nrow = nb_pfafs)

# Mapping would be easier with wide format predictions.

# 6.1 convert predictions df to wide format (pivot species into columns)
rf_predictions_wide <- rf_predictions |>
  pivot_wider(names_from = species,
              values_from = mean_prediction,
              values_fill = NA) # if any species-pfaf combo is missing... shouldn't happen

# 6.2 join in actual presence/absence data based on pfaf
rf_predictions_wide <- left_join(rf_predictions_wide, overlay, by="PFAF")

sanity <-  nrow(rf_predictions_wide[which(rf_predictions_wide$aeshna_canadensis.y == 0),])
# so 37792 pfafs in overlay...
# but 
nb_pfafs <- length(unique(rf_predictions_wide$PFAF))
# were that many pfafs truly added w/CA?
# lets map the pfafs from overlay
plot(st_geometry(overlay)) # so it has CA, but missing pfafs with no obs
















# Actual false negative rate map

hydroatlas <- st_read("data/raw/NA_CA_atlas.gpkg")
pfaf_false_negativity_results <- st_read("data/results/pfaf_false_negativity_results.gpkg")

# modify to avoid aleutian island mapping annoyance
hydroatlas <- st_wrap_dateline(
  hydroatlas,
  options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
  quiet = FALSE
)


# Make chloropeth map based on this
false_negativity_map <-
  ggplot()+
  geom_sf(data=hydroatlas, fill="grey", color=NA) + # bottom layer: grey for all pfafs
  geom_sf(data=pfaf_false_negativity_results, aes(fill = false_negativity), color = NA) + # top layer: color for evaluated pfafs
  scale_fill_viridis_c(option = "magma", name = "False Negative Rate") +
  coord_sf(
    xlim = c(-170, -50) # limit mapped longitudes to avoid aleutian wrapping
  )+
  theme_minimal()
false_negativity_map
ggsave("outputs/false_negativity_map.png",false_negativity_map)



# Make CROPPED to USCANish chloropeth map based on this
false_negativity_map_cropped <-
  ggplot()+
  geom_sf(data=hydroatlas, fill="grey", color=NA) + # bottom layer: grey for all pfafs
  geom_sf(data=pfaf_false_negativity_results, aes(fill = false_negativity), color = NA) + # top layer: color for evaluated pfafs
  scale_fill_viridis_c(option = "magma", name = "False Negative Rate") +
  coord_sf(
    xlim = c(-170, -50), # limit mapped longitudes to avoid aleutian wrapping
    ylim = c(25,70)
  )+
  theme_minimal()
false_negativity_map_cropped
ggsave("outputs/false_negativity_map_cropped.png",false_negativity_map_cropped)







# actual false positive rate map
pfaf_false_positivity_results <- st_read("data/results/pfaf_false_positivity_results.gpkg")

false_positivity_map <-
  ggplot()+
  geom_sf(data=hydroatlas, fill="grey", color=NA) + # bottom layer: grey for all pfafs
  geom_sf(data=pfaf_false_positivity_results, aes(fill = false_positivity), color = NA) + # top layer: color for evaluated pfafs
  scale_fill_viridis_c(option = "magma", name="False Positive Rate") +
  coord_sf(
    xlim = c(-170, -50)  # limit mapped longitudes to avoid aleutian wrapping
  )+
  theme_minimal()
false_positivity_map
ggsave("outputs/false_positivity_map.png",false_positivity_map)
