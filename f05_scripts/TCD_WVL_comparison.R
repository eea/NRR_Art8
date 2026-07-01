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

# Take into account that crunch is executing in a nested environment
setwd('//cwsfileserver.eea.dmz1/projects/Nature/Nature Restoration/Spatial data/Art8_Urban_CLMS')
here::i_am("//cwsfileserver.eea.dmz1/projects/Nature/Nature Restoration/Spatial data/Art8_Urban_CLMS/Art8_Urban_CLMS.Rproj")


# Import fonts
font_import(pattern = "calibri", prompt = FALSE)
loadfonts(device = "win")  # For Windows users

# Folders
output_folder <- here('f02_data', 'f03_output')
tcd_folder <-  here('f02_data','f01_input', 'TCD', 'TCD2024')

# Data 
lau <- st_read(here('f02_data', 'f02_processing', 'T20_db.gpkg'), layer='tcd24_wvl_comparison__dgurba_rg_01m_2021_3035_selection')
wvl <- rast(here('f02_data', 'f01_input', 'WVL', 'WVL_S2021_005m_eu_03035_V01_R02', 'WVL_S2021_005m_eu_03035_V01_R02.tif'))
wvl <- rast("//cwsfileserver.eea.dmz1/projects/Nature/Nature Restoration/Spatial data/Art8_Urban_CLMS/f02_data/f01_input/WVL/WVL_S2021_005m_eu_03035_V01_R02/WVL_2021_005m_eu_03035_V01_R02.tif")
dgurba <- rast(here('f02_data', 'f01_input', 'URBAN_CLST_2021.tif'))
# TCD loaded later

# Variables
lau_id_selection <- c('7315000', '7111000', '40101', '35033', '1004221436101' )


## ---- TCC Indicator ----

# Create a TCD mosaic

tcd_tiles <- list.files(
  path = tcd_folder,
  pattern = '//.tif$',
  full.names = TRUE
)

tcd_vrt <- vrt(tcd_tiles)

## ---- Zonal statistics LAU ----

# WVL 21
lau_wvl_21 <- exact_extract(wvl, 
                           lau, 
                           c('count', 'frac'), 
                           append_cols = c('GISCO_ID', 'CNTR_CODE', 'DGURBA', 'LAU_ID', 'LAU_NAME', 'area_m')) %>% 
  mutate(tcc_pct_wvl21 = frac_1)


# TCD 24
lau_tcd_24 <- exact_extract(tcd_vrt, 
                           lau, 
                           c('count', 'frac'), 
                           append_cols = c('GISCO_ID', 'CNTR_CODE', 'DGURBA', 'LAU_ID', 'LAU_NAME', 'area_m'))  %>%
  pivot_longer(
      cols = starts_with("frac_"),
      names_to = "canopy_value",
      values_to = "fraction"
    ) %>%
    mutate(
      canopy_value = as.numeric(sub("frac_", "", canopy_value))
    ) %>%
    group_by(GISCO_ID) %>%   
    summarise(
      tcc_pct_tcd24 = sum(canopy_value * fraction, na.rm = TRUE)/ 100, # This presents the sum of fraction per value
      .groups = "drop"
    )



# Merge & clean dataframes
lau_zs <- lau_wvl_21 %>% 
  left_join(lau_tcd_24) %>% 
  select(-starts_with("frac_"), count)
 

# Add labeling and calculate percentage differences between tcd24 and wvl 21
lau_zs <- lau_zs %>%
  mutate(
    lau_selection = ifelse(LAU_ID %in% lau_id_selection == TRUE, 1,0),
    DGURBA_lbl = case_when(
      DGURBA == 1 ~ 'City',
      DGURBA == 2 ~ 'Towns or suburbs',
      DGURBA == 3 ~ 'Rural areas'
    ),
    abs_diff = case_when(
      is.na(tcc_pct_wvl21) ~ NA_real_,
      tcc_pct_wvl21 == 0 ~ NA_real_,   
      TRUE ~ tcc_pct_tcd24 - tcc_pct_wvl21
    ),
    pct_diff = case_when(
      is.na(tcc_pct_wvl21) ~ NA_real_,
      tcc_pct_wvl21 == 0 ~ NA_real_,   
      TRUE ~ ((tcc_pct_tcd24 - tcc_pct_wvl21) / tcc_pct_wvl21) * 100
    )
  ) %>% relocate(DGURBA_lbl, .after=DGURBA) %>% 
  rename(lau_area_m = area_m)

# Export
write_xlsx(lau_zs, here('f02_data','f02_processing','zs_lau.xlsx'))


## ---- Plots and testing

skim(lau_zs)


df_long <- lau_zs %>%
  select(DGURBA_lbl, tcc_pct_wvl21, tcc_pct_tcd24 ) %>%
  pivot_longer(
    cols = c(tcc_pct_wvl21, tcc_pct_tcd24 ),
    names_to = "dataset",
    values_to = "tcc_pct"
  ) %>%
  mutate(dataset = recode(dataset,
                          tcc_pct_wvl21 = "WVL21",
                          tcc_pct_tcd24 = "TCD24"
  ))

df_long$DGURBA_lbl <- factor(df_long$DGURBA_lbl , levels =c("City", "Towns or suburbs", "Rural areas"))

ggplot(df_long, aes(x = DGURBA_lbl, y = tcc_pct, fill = dataset  )) +
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.color = "grey40") +
  #geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  scale_fill_manual(values = c(
    "WVL21" = "#FA6E25",
    "TCD24" = "#2C85A4"
  )) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0,1),
    breaks = seq(0,1,0.2)
  ) +
  labs(
    x = NULL,
    y = "Tree canopy cover per LAU (%)",
    fill = NULL 
    # title = "Comparison of Tree Canopy Cover Estimates"
  ) +
  theme_minimal(base_family = "Calibri", base_size = 13) +
  theme(
    #legend.position = "none",
    guides(fill = guide_legend(title = NULL)),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )


## ---- OLD Plots ----


# Continue for degree of urbanisation 
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



