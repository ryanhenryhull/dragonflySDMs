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
lat_density_accuracy_multiple_regression <-
  lm(data=rf_results_with_lat_data,
     mean_accuracy ~ centroid_decimalLat + observation_density_obs_per_km2)
summary(lat_density_accuracy_multiple_regression)



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



# 6. plot observation density against model accuracy with  #log transformation
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
