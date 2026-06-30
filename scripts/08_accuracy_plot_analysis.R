# ------------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Generate final figures/results
#               - plot of mean species latitude vs accuracy of model
#               - num obs
#               - obs density
# ------------------------------------------------------------------------------




# 1. Libraries
rm(list=ls())
library(ggplot2)
library(car)




# 2. Data
rf_results_with_lat_data <- read.csv("data/results/odonata_rf_performance_with_latitude_stats.csv")

# how many have good accuracy
length(which(rf_results_with_lat_data$mean_accuracy > 0.8))/318




# 3. Plot mean species latitude against accuracy of their rf model - using centroid decimal lat
mean_lat_vs_rf_accuracy <- ggplot(data = rf_results_with_lat_data, aes(x=centroid_decimalLat, y=mean_accuracy))+
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se=TRUE) +
  labs(
    title = "Random forest SDM accuracy across North American Odonates",
    x = "Centroid latitude of convex hull of select GBIF species observations",
    y = "Model accuracy"
  )+
  theme(
    plot.title = element_text(face = "bold", size=13),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 50, vjust=1, hjust=1, size=10))

mean_lat_vs_rf_accuracy



# 4. Assess and plot mean sp. lat. vs model accuracy - multiple regression to assess
#    whether effect is independent of observation density

lat_logdensity_accuracy_multiple_regression <-
  lm(data=rf_results_with_lat_data,
     mean_accuracy ~ centroid_decimalLat + log(observation_density_obs_per_km2)) #!!!!!!!!!! ********* to look for effect of bias 
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

# note geom_contour_filled kinda does together what the following do individually
# 1. scale_fill_steps(n.breaks=10) (the discrete colors), and
# 2. geom_contour(aes(z=accuracy_predition),bins=10) (lines only)






















# 5. plot number of observations against model accuracy.
number_observations_vs_rf_accuracy <- ggplot(data = rf_results_with_lat_data, aes(x=num_obs, y=mean_accuracy))+
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se=TRUE) +
  labs(
    title = "Random forest SDM accuracy across North American Odonates",
    x = "Number of observations",
    y = "Model accuracy"
  )+
  theme(
    plot.title = element_text(face = "bold", size=13),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 50, vjust=1, hjust=1, size=10))

number_observations_vs_rf_accuracy



# 5.2 plot number of observations against model accuracy.- log scale
number_observations_vs_rf_accuracy_log <- ggplot(data = rf_results_with_lat_data, aes(x=num_obs, y=mean_accuracy))+
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se=TRUE) +
  scale_x_log10() +   #log transformation
  labs(
    title = "Random forest SDM accuracy across North American Odonates",
    x = "Number of observations",
    y = "Model accuracy"
  )+
  theme(
    plot.title = element_text(face = "bold", size=13),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 50, vjust=1, hjust=1, size=10))

number_observations_vs_rf_accuracy_log




# 6. plot observation density against model accuracy (w/ log transformation)
obs_density_vs_rf_accuracy_log <- ggplot(data = rf_results_with_lat_data, aes(x=observation_density_obs_per_km2, y=mean_accuracy))+
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se=TRUE) +
  scale_x_log10() +   #log transformation
  labs(
    title = "Random forest SDM accuracy across North American Odonates",
    x = "Observation density (obs/km2 at log10 scale)",
    y = "Model accuracy"
  )+
  theme(
    plot.title = element_text(face = "bold", size=13),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 50, vjust=1, hjust=1, size=10))

obs_density_vs_rf_accuracy_log




# 7. Model stats
lm_lat_accuracy <- lm(mean_accuracy ~ mean_lat, data=rf_results_with_lat_data)
summary(lm_lat_accuracy)

lm_obs_accuracy <- lm(mean_accuracy ~ num_obs, data = rf_results_with_lat_data)
summary(lm_obs_accuracy)

lm_density_accuracy <- lm(mean_accuracy ~ observation_density_obs_per_km2, data = rf_results_with_lat_data)




# 8. Write out beautiful results
ggsave("outputs/rf_accuracy_vs_centroid_latitude_of_convex_hull.png", plot = mean_lat_vs_rf_accuracy,
       width = 10, height = 7)

ggsave("outputs/rf_accuracy_vs_num_obs.png", plot = number_observations_vs_rf_accuracy,
       width = 10, height = 7)

ggsave("outputs/rf_accuracy_vs_num_obs_log.png", plot = number_observations_vs_rf_accuracy_log,
       width = 10, height = 7)

ggsave("outputs/rf_accuracy_vs_obs_density_log.png", plot = obs_density_vs_rf_accuracy_log,
       width = 10, height = 7)


# the 2D partial dependence plot
ggsave("outputs/rf_2D_partial_dependence_plot_meanlat_obsdensity_rfaccuracy.png",
       plot=partial_dependence_plot_2d, width=7, height=5)
