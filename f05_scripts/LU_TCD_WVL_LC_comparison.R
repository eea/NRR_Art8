##
## Script name: create_VLCC_vrt.r
##
## Purpose of script: Create an annual vrt for VLCC products
##
## Author: Karl Ruf
##
## Date Created: 2025-03-25
##
## Copyright (c) space4environment, 2025
## Email: ruf@space4environment.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## ---------------------------
##
## Script name: create_VLCC_vrt.r
##
## Purpose of script: Create an annual vrt for VLCC products
##
## Author: Karl Ruf
##
## Date Created: 2025-03-25
##
## Copyright (c) space4environment, 2025
## Email: ruf@space4environment.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## ---- Load required libraries
pacman::p_load(here, sf, tidyverse, terra, tidyterra, exactextractr, skimr, extrafont, scales)

# Import fonts
font_import(pattern = "calibri", prompt = FALSE)
loadfonts(device = "win")  # For Windows users

## ---- Paths and variables tables 

TCD_18_E40N29 <- rast(here("f02_data","f01_input","CLMS_HRLVLCC_TCD_S2018_R10m_E40N29_03035_V01_R00.tif"))
TCD_18_E40N30 <- rast(here("f02_data","f01_input","CLMS_HRLVLCC_TCD_S2018_R10m_E40N30_03035_V01_R00.tif"))

TCD_21_E40N29 <- rast(here("f02_data","f01_input", "CLMS_HRLVLCC_TCD_S2021_R10m_E40N29_03035_V01_R00", "CLMS_HRLVLCC_TCD_S2021_R10m_E40N29_03035_V01_R00.tif"))
TCD_21_E40N30 <- rast(here("f02_data","f01_input", "CLMS_HRLVLCC_TCD_S2021_R10m_E40N30_03035_V01_R00", "CLMS_HRLVLCC_TCD_S2021_R10m_E40N30_03035_V01_R00.tif"))

WVL <- rast("R:/INSPIRE_Annex_II/02_Land_cover/08_Copernicus_High_Resolution_Layer/01_Forest/small_woody_features/2021/26691/Results/WVL_2021_005m_eu_03035_V01_R01/WVL_2021_005m_eu_03035_V01_R01/WVL_2021_005m_eu_03035_V01_R01.tif")

# In-situ 
LC18 <- rast("R:/Ref_Data_Luxembourg/Land_Use_Land_cover/S4E_landuse_and_landcover_products/landcover/f02_raster/LC2018_Lux/lc2018_20cm_tif/LC_2018_20cm.tif")
LC21 <- rast("R:/Ref_Data_Luxembourg/Land_Use_Land_cover/S4E_landuse_and_landcover_products/landcover/LandCover2021_raster/LandCover2021.tif")

# Extent 
lu_ext_3035 <- ext(4013600, 4072400, 2932700, 3016100)

ref_grid_2169 <- st_read("N:/C2409_ANF/f02_data/luref-grid/luref_grid.shp")

processing_folder <- here('f02_data','f02_processing', 'copernicus_products_clipped')
output_folder <- here('f02_data','f03_output')

dgurba_3035 <- st_read(here('f02_data','f02_processing','T20_db.gpkg'), layer='DGURBA_1km_2021')
dgurba_2169 <- st_read(here('f02_data','f02_processing','T20_db.gpkg'), layer='DGURBA_1km_2021_EPSG2169') %>% select(-fid)

## ---- Processing ----


# Clip TCD 

tcd_18_mosaic <- merge(TCD_18_E40N29, TCD_18_E40N30)
tcd_18_3035 <- crop(tcd_18_mosaic,lu_ext_3035)
writeRaster(tcd_18_3035, here(processing_folder, 'tcd_18_bb_3035.tif'), 
            wopt = list(gdal=c("COMPRESS=LZW", datatype='INT1U' )), overwrite=TRUE )



tcd_21_mosaic <- merge(TCD_21_E40N29, TCD_21_E40N30)
tcd_21_3035 <- crop(tcd_21_mosaic,lu_ext_3035)
writeRaster(tcd_21_3035, here(processing_folder, 'tcd_21_bb_3035.tif'), 
            wopt = list(gdal=c("COMPRESS=LZW", datatype='INT1U' )), overwrite=TRUE )

wvl_21_3035 <- crop(WVL, lu_ext_3035)
writeRaster(tcd_18_3035, here(processing_folder, 'wvl_21_bb_3035.tif'), 
            wopt = list(gdal=c("COMPRESS=LZW", datatype='INT1U' )), overwrite=FALSE )


rm(tcd_18_mosaic,tcd_21_mosaic)

# Project to Lux projection
tcd_18_2169 <- project(tcd_18_3035, "EPSG:2169")
writeRaster(tcd_18_2169, here(processing_folder, 'tcd_18_bb_2169.tif'), 
            wopt = list(gdal=c("COMPRESS=LZW", datatype='INT1U' )), overwrite=FALSE )

tcd_21_2169 <- project(tcd_21_3035, "EPSG:2169")
writeRaster(tcd_21_2169, here(processing_folder, 'tcd_21_bb_2169.tif'), 
            wopt = list(gdal=c("COMPRESS=LZW", datatype='INT1U' )), overwrite=FALSE )

wvl_21_2169 <- project(wvl_21_3035, "EPSG:2169")
writeRaster(wvl_21_2169, here(processing_folder, 'wvl_21_bb_2169.tif'), 
            wopt = list(gdal=c("COMPRESS=LZW", datatype='INT1U' )), overwrite=FALSE )

## ---- Zonal statistcs Copernicus ----

# WVL
zs_wvl_21 <- exact_extract(wvl_21_3035, 
                           dgurba_3035, 
                           c('count', 'frac'), 
                           append_cols = c('cell_id', 'DGURBA')) %>% 
  mutate(tcd_pct_wvl21 = frac_1)


# TCD 18
zs_tcd_21 <- exact_extract(tcd_21_3035, 
                           dgurba_3035, 
                           c('count', 'frac'), 
                           append_cols = c('cell_id', 'DGURBA'))  %>%
  pivot_longer(
    cols = starts_with("frac_"),
    names_to = "canopy_value",
    values_to = "fraction"
  ) %>%
  mutate(
    canopy_value = as.numeric(sub("frac_", "", canopy_value))
  ) %>%
  group_by(cell_id) %>%   # <-- replace with your 1 km grid ID
  summarise(
    tcd_pct_tcd21 = sum(canopy_value * fraction, na.rm = TRUE)/ 100,
    .groups = "drop"
  )

 
## ---- Zonal statistcs in situ ----

#Calculate zonal statistics
zs_lc_21 <- exact_extract(LC21, 
                           dgurba_2169, 
                           c('count', 'frac'), 
                           append_cols = c('cell_id')
)

zs_lc_21 <- zs_lc_21 %>% rowwise () %>% mutate(lc_woody = 
  frac_70 + frac_80)
  
# Export
write.csv(zs_lc_21, here('f02_data','f02_processing','zs_lc_21.csv'), row.names = FALSE)



# Coorect labeling
zonal_stats_final <- zs_lc_21 %>%   
  left_join(zs_wvl_21, by=c('cell_id' = 'cell_id')) %>%  left_join(zs_tcd_21,by=c('cell_id' = 'cell_id')) %>% 
  select(cell_id, DGURBA, lc_woody, tcd_pct_wvl21, tcd_pct_tcd21) 

zonal_stats_grid <- dgurba_3035 %>% left_join(zonal_stats_final)

zonal_stats_grid$DGURBA <- factor(zonal_stats_grid$DGURBA, levels = 
c('Urban Centre', 'Dense Urban Custer', 'Semi dense Urban cluster', 'Suburban/peri-urban grid cell',
'Rural cluster', 'Low density rural grid cell', 'Very low density rural grid cell', 'Water'))

zs_summary <- zonal_stats_grid %>%  group_by(DGURBA) %>% summarise(lc_pct_sum = sum(lc_woody)  ,
                                                    wvl_pct_sum = sum(tcd_pct_wvl21),
                                                    tcd_pct_sum = sum(tcd_pct_tcd21))


# 60,70,80, 91,92

zs_lc_21_long <- read.csv(here('f02_data','f02_processing','zs_lc_21.csv')) %>%
  pivot_longer(
    cols = starts_with("frac_"),
    names_to = "lc_code",
    values_to = "pct"
  ) %>% mutate(lc_code = str_remove(lc_code, "frac_" )) %>%  
  left_join(dgurba_3035) %>% 
  filter(lc_code %in% c(60,70,80,91,93)) %>% 
  group_by(DGURBA) %>% 
  summarize(gua_pct = sum(pct)) 


# asdf
gua_wide <- read.csv(here('f02_data','f02_processing','zs_lc_21.csv')) %>%
  rowwise () %>% mutate(gua_frac = frac_60 + frac_70 + frac_80 + frac_91 + frac_93,
                        gua_area_m2 = (count*0.2^2)*gua_frac) %>%   left_join(dgurba_3035) %>% 
  group_by(DGURBA) %>% 
  summarize(gua_area_sum = sum(gua_area_m2)) 
  
dgurba_tbl <- 
dgurba_3035 %>% group_by(DGURBA)  %>%  summarise(dgurba_area_km = n()) %>% st_drop_geometry() %>% mutate(dgurba_area_m = dgurba_area_km*1000^2)

gua_wide %>% left_join(dgurba_tbl) %>%  mutate(lc_gua = gua_area_sum / dgurba_area_m)

## ---- Plots ----

zs_long <- zs_summary %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = ends_with("_pct_sum"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      lc_pct_sum  = "national LC",
      wvl_pct_sum = "WVL",
      tcd_pct_sum = "TCD"
    #),
    #DGURBA = forcats::fct_reorder(DGURBA, value, .fun = sum, .desc = T)
  ))

zs_long$metric <- factor(zs_long$metric, levels =c('national LC', 'WVL', 'TCD'))

tableau_cols <- c(
  "national LC"        = "#F5CE2A",
  "WVL"  = "#FA6E25",
  "TCD"       = "#2C85A4"
)




#
ggplot(zs_long %>% filter(DGURBA != 'Water'), aes(x=metric, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = tableau_cols) +
  labs(
    x = NULL,
    y = expression("km"^2),
    fill = NULL,
    title = bquote(bold("Tree canopy area (" * "km"^2 * ") for Luxembourg by product (2021)")),
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
    strip.text = element_text(
      margin = margin(t = 6, b = 6, l = 20, r = 20)
  ))+facet_wrap(~DGURBA, scales = "free_y")


ggsave( filename=here('f04_documents', 'DURBA_by_product.tiff'), width = 17, height = 18, dpi = 300, units =c("cm") )


## Pct grid_cell_stats <- zonal_stats_grid %>%  st_drop_geometry() %>% group_by(DGURBA ) %>% summarize(area_km2 = n(), .groups = "drop")

zs_plot <- zs_summary %>% st_drop_geometry() %>%  left_join(grid_cell_stats) %>% 
  mutate(
    across(
      c(lc_pct_sum, wvl_pct_sum, tcd_pct_sum),
      ~ .x / area_km2,
      .names = "{.col}_pct_cover"
    )
  ) %>%  pivot_longer(
    cols = ends_with("_pct_cover"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      lc_pct_sum_pct_cover  = "national LC",
      wvl_pct_sum_pct_cover = "WVL",
      tcd_pct_sum_pct_cover = "TCD"
      #),
      #DGURBA = forcats::fct_reorder(DGURBA, value, .fun = sum, .desc = T)
    ))

zs_plot$metric <- factor(zs_plot$metric, levels =c('national LC', 'WVL', 'TCD'))

ggplot(zs_plot %>% filter(DGURBA != 'Water'), aes(x=metric, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = tableau_cols) +
  scale_y_continuous(
    labels = label_percent()
  )+
  labs(
    x = NULL,
    y = "Tree canopy cover",
    fill = NULL,
    title = bquote(bold("Tree canopy cover for LU (%) by degree of urbanisation (2021)")),
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
    strip.text = element_text(
      margin = margin(t = 6, b = 6, l = 20, r = 20)
    ))+
  facet_wrap(~DGURBA, scales = "free_y")


ggsave( filename=here('f04_documents', 'DURBA_by_product.tiff'), width = 17, height = 18, dpi = 300, units =c("cm") )





skim(zs_plot %>% group_by(DGURBA)) 



