# -----------------------------------------------------------------------------
# Author: Ryan Hull, help from Christophe Brabant
# Date: October 2025
# Purpose: From shapefile hydroatlas north america download, choose relevant
# countries and layers, and export as gpkg
# -----------------------------------------------------------------------------

# 1. Libraries
rm(list=ls())
library(sf)
library(dplyr)
library(ggplot2)


# 2. Data - very heavy so hardcode shared file
hydroatlas <- read_sf("C:/Users/Dell/OneDrive - McGill University/IversenLab_Group - HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.shp")



# 3. Filtering
#Canada GADM ID = 41 
#United States GADM ID = 240

CAN_USA = hydroatlas %>%
  filter(gad_id_smj == 41 | gad_id_smj == 240) %>%
  select(c(HYBAS_ID, PFAF_ID, pre_mm_syr, ele_mt_sav, slp_dg_sav, ari_ix_sav,
           tmp_dc_syr,  snd_pc_sav, soc_th_sav, wet_cl_smj, lka_pc_sse,
           dis_m3_pyr, gad_id_smj, snw_pc_syr, for_pc_sse, sgr_dk_sav, aet_mm_syr,
           crp_pc_sse, geometry))

#Sanity check: make sure we have Canada and USA
ggplot()+  geom_sf(data=CAN_USA, aes(fill=gad_id_smj))+  theme_minimal()

st_crs(CAN_USA)

st_write(CAN_USA, "data/raw/CAN_USA_atlas.gpkg")
