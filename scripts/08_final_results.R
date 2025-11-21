# ------------------------------------------------------------------------------
# Author: Ryan Hull
# Date: November 2025
# Purpose: Generate final figures/results
#               - plot of mean species latitude vs accuracy of model
#               - heat map of where we can make good predictions
# ------------------------------------------------------------------------------



# 1. Libraries
rm(list=ls())
library(ggplot2)



# 2. Data
rf_results_with_lat_data <- read.csv("data/results/odonata_rf_performance_with_latitude_stats.csv")



# 3. Plot mean species latitude against accuracy of their rf model
mean_lat_vs_rf_accuracy <- ggplot(data = rf_results_with_lat_data, aes(x=mean_lat, y=mean_accuracy))+
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se=TRUE) +
  labs(
    title = "Random forest SDM accuracy across North American Odonates",
    x = "Mean latitude of select GBIF species observations",
    y = "Model accuracy"
  )+
  theme(
    plot.title = element_text(face = "bold", size=13),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 50, vjust=1, hjust=1, size=10))

mean_lat_vs_rf_accuracy



# 4. plot number of observations against model accuracy
number_observations_vs_rf_accuracy <- ggplot(data = rf_results_with_lat_data, aes(x=num_obs, y=mean_accuracy))+
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se=TRUE) +
  labs(
    title = "Random forest SDM accuracy across North American Odonates",
    x = "Number of GBIF observations",
    y = "Model accuracy"
  )+
  theme(
    plot.title = element_text(face = "bold", size=13),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 50, vjust=1, hjust=1, size=10))

number_observations_vs_rf_accuracy



# 5. Model stats
lm_lat_accuracy <- lm(mean_accuracy ~ mean_lat, data=rf_results_with_lat_data)
summary(lm_lat_accuracy)

lm_obs_accuracy <- lm(mean_accuracy ~ num_obs, data = rf_results_with_lat_data)
summary(lm_obs_accuracy)



# 5. Make heat map of prediction accuracy across PFAFs



# 6. Write out beautiful results
ggsave("outputs/rf_accuracy_vs_mean_latitude.png", plot = mean_lat_vs_rf_accuracy,
       width = 10, height = 7)

ggsave("outputs/rf_accuracy_vs_num_obs.png", plot = number_observations_vs_rf_accuracy,
       width = 10, height = 7)
