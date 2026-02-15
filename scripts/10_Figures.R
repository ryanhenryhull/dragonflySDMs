# Author: Ryan Hull
# Date: February 2026
# Purpose: Generate figures for poster presentation of research project




# 1. Libraries
rm(list=ls())
library(ggplot2)
library(lattice)
library(sf)
library(dplyr)




# 2. Data
rf_predictions <- read.csv("data/results/odonata_rf_predictions.csv")
leucorrhinia_intacta_predictions <- rf_predictions[
  rf_predictions$species == "leucorrhinia_intacta",]

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




# 4. 

# 1. Libraries
rm(list=ls())
library(ggplot2)
library(car)




# 5. 2D partial dependence plot of influence of obs density and centroid lat
#    on rf accuracy, based on multiple regression values
#    (taken from 08_)
lat_logdensity_accuracy_multiple_regression <-
  lm(data=rf_results_with_lat_data,
     mean_accuracy ~ centroid_decimalLat + log(observation_density_obs_per_km2))
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