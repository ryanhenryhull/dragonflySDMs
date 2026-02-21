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
# or: for GIC computer
hydroatlas <- read_sf("U:/hydroatlas/BasinATLAS_v10_lev12.shp")

# 3. Filtering
# Canada GADM ID = 41 
# United States GADM ID = 240
# Mexico GADM ID = 145
# Cuba GADM ID = 58
# Haiti GADM ID = 99
# Dominica GADM ID = 65
# Dominican Republic GADM ID = 66
# Puerto Rico GADM ID = 181.00
# Jamaica GADM ID = 113
# Guatemala GADM ID = 94
# Belize GADM ID = 23
# El Salvador GADM ID = 70
# Nicaragua GADM ID = 161
# Costa Riva GADM ID = 55
# Panama GADM ID = 173
# Bahamas GADM ID = 17
# Turks and Caicos = 234
# Honduras = 101 

CAN_USA = hydroatlas %>%
  filter(gad_id_smj == 41 | gad_id_smj == 240 | gad_id_smj == 145 
         | gad_id_smj == 58 | gad_id_smj == 99 | gad_id_smj == 65
         | gad_id_smj == 66 | gad_id_smj == 181 | gad_id_smj == 113 
         | gad_id_smj == 94 | gad_id_smj == 23 | gad_id_smj == 70
         | gad_id_smj == 161 | gad_id_smj == 55 | gad_id_smj == 173
         | gad_id_smj == 17 | gad_id_smj == 234 | gad_id_smj == 101) %>%
  select(c(HYBAS_ID, PFAF_ID, pre_mm_syr, ele_mt_sav, slp_dg_sav, ari_ix_sav,
           tmp_dc_syr,  snd_pc_sav, soc_th_sav, wet_cl_smj, lka_pc_sse,
           dis_m3_pyr, gad_id_smj, snw_pc_syr, for_pc_sse, sgr_dk_sav, aet_mm_syr,
           crp_pc_sse, fec_cl_smj, geometry)) #fec_cl_smj to include ecoregions

#Sanity check: make sure we have countries we want
ggplot()+  geom_sf(data=CAN_USA, aes(fill=gad_id_smj))+  theme_minimal()

st_crs(CAN_USA)

st_write(CAN_USA, "data/raw/NA_CA_atlas.gpkg", append=FALSE) #st_write requires you write false or true, no default