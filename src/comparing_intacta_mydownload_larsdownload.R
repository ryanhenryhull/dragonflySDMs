# --------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: Make sure that our data downloads are okay by comparing lars' download to my own
rm(list=ls())

larsdata <- read.csv("data/all_odonata_obs_clean.csv")
larsintacta <- larsdata[larsdata$species=="Leucorrhinia intacta",]

myintacta <- read.csv("data/intacta_obs.csv")

# 10000 obs difference.....
# potential causes:
# Mine was all of NA, lars seems to be just USA/CAN
# His has occurence status = present, mine doesnt
# mine had sci. name = Leucorrhinia Brittinger, 1850, maybe has more than just leucorrhinia???
# Assume there's some issue with mine, since his doi + my gbif check with those parameters
# matched and it's pretty crystal.