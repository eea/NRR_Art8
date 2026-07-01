##
## Script name: TCD_WVL_comparison.r
##
## Purpose of script: Compare TCD and WVL data for test cities.
##
## Author: Karl Ruf
##
## Date Created: 2026-02-24
##
## Copyright (c) space4environment, 2026
## Email: ruf@space4environment.com
##
## ---------------------------


## ---- Setup ----
# Load required libraries
pacman::p_load(here, sf, tidyverse, terra, tidyterra, exactextractr, skimr, extrafont, scales, writexl)


# Folders
output_folder <- here('f02_data', 'f03_output')
tcd_folder <-  here('f02_data','f01_input', 'TCD', 'TCD2024')

# Data 
lau <- st_read(here('f02_data', 'f02_processing', 'T20_db.gpkg'), layer='tcd24_wvl_comparison__dgurba_rg_01m_2021_3035_selection')
wvl_folder  <- '//cwsfileserver.eea.dmz1/Copernicus/SmallLandscapeFeatures/WoodyVegetationLayer/WVL2021_5m'

wvl <- rast(paste0( wvl_folder, '/WVL_S2021_005m_E40N29_BE_DE_FR_LU_03035_V01_R01/', 'WVL_S2021_005m_E40N29_BE_DE_FR_LU_03035_V01_R01.tif'))

dgurba <- rast(here('f02_data', 'f01_input', 'URBAN_CLST_2021.tif'))
# TCD loaded later


wvl
