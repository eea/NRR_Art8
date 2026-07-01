##
## Script name: create_VLCC_vrt.r
##
## Purpose of script: Calcualte degree of urbanisation for lux
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
pacman::p_load(here, sf, tidyverse, terra, tidyterra, exactextractr, skimr, extrafont, scales, igraph, flexurba)

# Import fonts
font_import(pattern = "calibri", prompt = FALSE)
loadfonts(device = "win")  # For Windows users

## ---- Paths and variables tables 
popgrid <- st_read(here("f02_data","f02_processing","T20_db.gpkg"), layer="population_par_grille_dun_km_2021")

pop <- rast(here("f02_data","f02_processing", "population_par_grille_1km_2021.tif"))

data_lu <- DoU_preprocess_grid("data/belgium")

# Define urban centre
urban_mask <- pop
urban_mask[pop < 1500] <- NA
urban_mask[pop >= 1500] <- 1

#define towns and suburbs
urban_mask_towns <- pop
urban_mask_towns[pop < 300 | pop >= 1500] <- NA
urban_mask_towns[pop >= 300 & pop < 1500] <- 2



urban_patches <- patches(
  urban_mask,
  directions = 4   # IMPORTANT: rook connectivity
)

urban_pop <- zonal(
  pop,
  urban_patches,
  fun = "sum",
  na.rm = TRUE
)


valid_centres <- urban_pop$patches[urban_pop$population_par_grille_1km_2021 >= 50000]


urban_centres <- urban_patches
urban_centres[!urban_patches %in% valid_centres] <- NA


cluster_mask <- pop
cluster_mask[pop < 300] <- NA
cluster_mask[pop >= 300] <- 1
cluster_mask[!is.na(urban_centres)] <- NA

cluster_patches <- patches(
  cluster_mask,
  directions = 8
)


cluster_pop <- zonal(pop, cluster_patches, "sum", na.rm = TRUE)
valid_clusters <- cluster_pop$patches[cluster_pop$population_par_grille_1km_2021 >= 5000]


r[r == 0] <- NA




# identify urban cluster/center
popgrid <- popgrid %>%
  mutate(
    urban_centre_candidate = Population >= 1500,
    urban_cluster_candidate = Population >= 300
  )


neighbors <- st_touches(popgrid)

edges <- do.call(
  rbind,
  lapply(seq_along(neighbors), function(i) {
    if (length(neighbors[[i]]) == 0) return(NULL)
    cbind(i, neighbors[[i]])
  })
)

g <- graph_from_edgelist(edges, directed = FALSE)
components <- components(g)

popgrid$component <- components$membership

urban_centres <- popgrid %>%
  filter(urban_centre_candidate) %>%
  group_by(component) %>%
  summarise(
    pop_cluster = sum(Population),
    .groups = "drop"
  ) %>%
  filter(pop_cluster >= 50000)


popgrid <- popgrid %>%
  left_join(
    urban_centres %>% mutate(degurba = 1) %>% st_drop_geometry(),
    by = "component"
  )


## identify urban clusters (towns & suburbs)

urban_clusters <- popgrid %>%
  filter(
    is.na(degurba),
    urban_cluster_candidate
  ) %>%
  group_by(component) %>%
  summarise(
    pop_cluster = sum(Population),
    .groups = "drop"
  ) %>%
  filter(pop_cluster >= 5000)

popgrid <- popgrid %>%
  left_join(
    urban_clusters %>% mutate(degurba = 2) %>% st_drop_geometry(),
    by = "component"
  )

popgrid <- popgrid %>% rename( degurba.x)
  mutate(
    degurba = case_when(
      degurba == 1 ~ 1,   # cities
      degurba == 2 ~ 2,   # towns & suburbs
      TRUE ~ 3            # rural
    )
  )

  
  grid <- grid %>%
    mutate(
      degurba_label = recode(
        degurba,
        `1` = "Cities",
        `2` = "Towns and suburbs",
        `3` = "Rural areas"
      )
    )
  
  

