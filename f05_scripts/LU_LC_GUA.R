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
pacman::p_load(here, sf, tidyverse, terra, tidyterra, exactextractr, skimr, extrafont, scales,doParallel, foreach, progressr)

# Import fonts
font_import(pattern = "calibri", prompt = FALSE)
loadfonts(device = "win")  # For Windows users

## ---- Paths and variables tables 


# In-situ 

LC21 <- here("R:/Ref_Data_Luxembourg/Land_Use_Land_cover/S4E_landuse_and_landcover_products/landcover/LandCover2021_raster/LandCover2021.tif")

clcBB <- rast(here('f02_data','f02_processing', 'clcplus_bb_lu_2021.tif'))

output_folder <- here('f02_data','f02_processing')


dgurba_path = "N:/C2506_Copernicus_2526/T20_CLMS_UU/f02_data/f02_processing/T20_db.gpkg"
dgurba_layer = "DGURBA_2021_2169"

dgurba <- st_read("N:/C2506_Copernicus_2526/T20_CLMS_UU/f02_data/f02_processing/T20_db.gpkg", layer="limites_com_dgurb_2021")

## ---- Processing ----

## PROCESSING

# Load data 
dgurba <- st_read(dgurba_path, 
                layer=dgurba_layer)  


# Start cluster 
cls <- makeCluster(8)
registerDoParallel(cls)



# Working loop
zonal_stats <- foreach(i = 1:length(rownames(dgurba)), 
                       .packages = c("terra", "sf", "exactextractr", "tidyverse"),
                       .export = c("dgurba_layer", "dgurba_path", "LC21", "output_folder"),
                       .errorhandling = 'pass',
                       .combine = dplyr::bind_rows,
                       #.combine = rbind,
                       .inorder = TRUE) %dopar% {
                         
                         # Load NUTS tile
                         query_id <- paste0("SELECT * FROM ", dgurba_layer, " LIMIT 1 OFFSET ", i)
                         commune_geom <- st_read(dgurba_path, layer = dgurba_layer, query=query_id)
                         commune_id <- commune_geom$COMMUNE
                         
                         # Load NUTS raster file
                         lc <- rast(LC21)
                         #Calculate zonal statistics
                         zonal_stats_commune <- exact_extract(lc, 
                                                              commune_geom, 
                                                           c('count', 'frac'), 
                                                           append_cols = c('COMMUNE', 'area_m2')
                         )
                         
                         # Reformat data
                         zonal_stats_commune <- zonal_stats_commune %>% 
                           rename(commune_area_m2 = area_m2, lc21_px_cnt = count) %>% 
                           pivot_longer(cols=contains("frac") ,names_to = 'lc_code', values_to = "class_pct") %>% rowwise() %>% 
                           mutate(lc_code= as.numeric(str_split(lc_code, pattern='_')[[1]][2]),
                                  class_area_m2= lc21_px_cnt*0.2^2*class_pct)
                         
                         # Buffer for processing
                         
                         Sys.sleep(0.1)
                         
                         return(zonal_stats_commune)
                         
                         
                       }                

stopCluster(cls)

# Coorect labeling
zonal_stats_final <-zonal_stats %>%  left_join(., alf_lookup_tbl ) %>% relocate(c(lbl_lvl1,AA), .after=code_lvl1)

# Export
write.csv(zonal_stats_final, here('f03_processing', 'eea100km_alf_lvl1', 'NUTS_zonal_statistics_lvl1_AA_masked.csv'))



























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



