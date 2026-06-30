################################################################################
# Author: Ryan Hull
# Date: June 2026
# Purpose: Calculating AUC, a more meaningful metric
#          of SDM performance than straight accuracy.
# Outputs: odonata_rf_performance_latitude_AUC.csv
################################################################################

# 0. Context
# AUC (Area under the ROC Curve) is a more truthful measure of SDM performance
# than straight accuracy. Unlike standard accuracy, it will be independent of 
# thresholds chosen for the binary classification. Model performance will be 
# evaluated by plotting TP rate (sensitivity) vs FN rate (1-specificity) 
# across all possible thresholds from 0.01 to 0.99 or so, producing
# the ROC curve. If this curve is straight diagonal, ie AUC = 0.5,  then the 
# model predictive power is equal to chance. The more curved, the better (above
# 0.8 is good to excellent, but too close to 1 may suggest overfitted model.

# 1. Libraries

# 2. Data
results_no_AUC <- read.csv("data/results/odonata_rf_performance_results_with_latitude.csv")

# 3. Write out our beautiful results
write.csv(results_with_AUC, "data/results/odonata_rf_performance_latitude_AUC.csv", row.names=FALSE)
