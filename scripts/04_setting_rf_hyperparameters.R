# ------------------------------------------------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: Run RF training function over 10 randomly selected dragonflies,
# to learn what a good middle ground hyperparamaterization would be.
# ------------------------------------------------------------------------

## 1. Libraries
rm(list=ls())
library(ggplot2)
library(lattice)
library(caret)


# 2. Data
species_list <- read.csv("data/processed/odonata_species_list_with_obs.csv")



# 3. Selecting ten species randomly
set.seed(244)

random_numbers = sample.int(321, 10, replace=FALSE)
random_species = species_list[random_numbers, ]



# 4. Run caret train function


