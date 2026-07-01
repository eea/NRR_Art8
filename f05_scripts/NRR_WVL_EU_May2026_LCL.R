#####################################################################################
#                                                                                   #
#               PERCENTAGE OF TREE CANOPY COVER (TCC) IN URBAN                       ----
#     CENTRES AND URBAN CLUSTERS (UCUC) IN LOCAL ADMINISTRATIVE UNITS (LAU)          ----
#           THAT ARE CITIES (1) OR TOWNS AND SUBURBS (2) IN THE EU                   ----                

#        _______USING *WOODY VEGETATION LAYER* (WVL) TO CALCULATE TCC_______         ----
#                                                                                   #
#####################################################################################

# PACKAGES ----

pacman::p_load(here, terra, tictoc, tidyverse, sf, exactextractr, readxl, writexl)


#_______ (1) LOAD ORIGINAL DATASETS: LAU + DEGURBA, UCUC, WVL ----

## (1.1) DATASETS DESCRIPTION ----
#
# - LAU: 2024, polygon shapefile (DGURBA_2024_100K.shp), shared by email by Michael Harrop 27/10/2025.
#
# - DEGURBA: 2024, polygon shapefile (DGURBA_2024_100K.shp), shared by email by Michael Harrop 27/10/2025.
#
# - UCUC: 2021, raster (ucuc_2021.tif) including both Urban Centres and Urban Clusters, created by Grazia Zulian 17/10/25.
#
# - WVL: 2021, raster (WVL_2021_005m_eu_03035_V01_R02.tif), downloaded from CLMS 26/04/2026.


## (1.2) DATASETS UPLOAD ----

### LAU 2024  ----
# Local Administrative Units with information on degree of urbanization (DEGURBA). Year: 2024.

LAU <- vect(here("data", "dgurba_2024", "DGURBA_2024_100k.shp"))
LAU


### UCUC 2021 ----
# Urban Centres and Urban Clusters, raster at 1km, merged and prepared by Grazia Zulian. Year: 2021.

UCUC <- rast(here("data", "UCUC_2021", "CLUSTERS_2021_gdb", "ucuc_2021.tif"))
UCUC


### WVL 2021  ----
# Woody Vegetation Layer, raster at 5m, binary. Year: 2021.

WVL <- rast(here("data", "WVL_S2021_005m_eu_03035_V01_R02", "WVL_2021_005m_eu_03035_V01_R02.tif"))
WVL



#_______ (2) PRE-PROCESSING AND PRE-PROCESSED DATA ----

## (2.1) LAU ----

# LAU layer reproject to EPSG:3035, filter for urban LAUs, i.e., (1) cities and (2) towns and suburbs, and convert to 'sf' object

LAU_12 <- LAU[LAU$DEGURBA %in% c(1, 2), ]    # 18296 polygons
LAU_12

writeVector(LAU_12, here("outputs", "LAU", "LAU_12.gpkg"))


# Check CRS
crs(LAU,  describe = TRUE)   # ETRS89                           EPSG 4258
crs(WVL, describe = TRUE)    # ETRS89-extended / LAEA Europe    EPSG 3035


# Project LAU to EPSG:3035

LAU_12_3035 <- project(LAU_12, WVL)

writeVector(LAU_12_3035, here("outputs", "LAU", "LAU_12_3035.gpkg"))


# Convert to 'sf' object

LAU_12_3035_sf <- st_as_sf(LAU_12_3035)
LAU_12_3035_sf

LAU_12_3035_sf <- sf::st_transform(LAU_12_3035_sf, terra::crs(WVL)) # both were already in EPSG:3035, but there was a warning about the EPSG, probably because WVL comes from 'terra' and LAU_12_3035_sf from 'sf'


## (2.2) UCUC ----

# Resampling UCUC to the resolution of WVL, i.e., 5m

tic()

UCUC_5m <- terra::resample(UCUC,
                           WVL, 
                           method   = "near",
                           filename = here("outputs", "UCUC", "UCUC_5m.tif"),  # writes to file (in chunks)
                           datatype = "INT1U",                            # makes sure data type is integer
                           overwrite = TRUE)

toc() # 36369.05 sec elapsed = 606.15 min = 10.10 h



#_______ (3) TREE CANOPY COVER (TCC) WITH WVL ----

## (3.1) MASK WVL WITH UCUC_5m, TO KEEP WVL INSIDE UCUC ONLY ----

tic()

WVL_UCUC <- mask(WVL, 
                 UCUC_5m,
                 filename = here("outputs", "WVL", "WVL_UCUC.tif"),
                 datatype = "INT1U",
                 overwrite = TRUE)

toc() # 10635.09 sec elapsed = 177.25 min = 2.95 h

WVL_UCUC # note that the file is already saved because of this code line: filename = here("outputs", "WVL", "WVL_UCUC.tif")



## (3.2) ZONAL STATISTICS TO EXTRACT TCC FROM WVL IN THE UCUC INSIDE URBAN LAUs ----

tic()

LAU_12_3035_WVL <- exact_extract(WVL_UCUC,
                                 LAU_12_3035_sf,
                                 fun         = "mean",
                                 append_cols = c("GISCO_ID", 
                                                 "CNTR_CODE", 
                                                 "LAU_NAME",
                                                 "DEGURBA", 
                                                 "SHAPE_AREA", 
                                                 "SHAPE_LEN", 
                                                 "geometry"),
                                 progress = TRUE)

toc() # 13020.33 sec elapsed = 217.01 min = 3.62 h


str(LAU_12_3035_WVL)

LAU_12_3035_WVL <- LAU_12_3035_WVL %>% 
  mutate(WVL = mean*100) %>% 
  select(-mean) %>%
  rename(COUNTRY_CODE = CNTR_CODE)


## (3.3) MERGE TCC ZONAL STATISTICS WITH LAU AND SAVE VECTOR AND DATA FRAME ----

# Rejoin geometry from sf 

LAU_12_3035_WVL_vec <- left_join(LAU_12_3035_sf[, c("GISCO_ID", "geometry")],
                                 LAU_12_3035_WVL,
                                 by = "GISCO_ID") |>
                                 sf::st_as_sf()

sf::st_write(LAU_12_3035_WVL_vec, here("outputs", "Results_TCC_WVL", "LAU_12_3035_WVL_vec.gpkg"),
             overwrite = TRUE, append = FALSE)


# Save df

LAU_12_3035_WVL_df <- LAU_12_3035_WVL

write.csv(LAU_12_3035_WVL_df, here("outputs", "Results_TCC_WVL", "LAU_12_3035_WVL_df.csv"), row.names = FALSE)

write_xlsx(LAU_12_3035_WVL_df, here("outputs", "Results_TCC_WVL", "LAU_12_3035_WVL_df.xlsx"))


# Filter for EU27 and Split by country

LAU_12_3035_WVL_df<- read_excel(here("outputs", "Results_TCC_WVL", "LAU_12_3035_WVL_df.xlsx"))

EU27_list <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE")

LAU_12_3035_WVL_df <- LAU_12_3035_WVL_df %>% 
  mutate(EU27 = as.factor(ifelse(COUNTRY_CODE %in% EU27_list, "EU27", "no_EU27"))) %>% 
  filter(EU27 == "EU27") %>% 
  select(-c(EU27)) %>%
  select(COUNTRY_CODE, GISCO_ID, LAU_NAME, DEGURBA, SHAPE_AREA, SHAPE_LEN, WVL)

unique(LAU_12_3035_WVL_df$COUNTRY_CODE) # not sorted alphabetically
LAU_12_3035_WVL_df <- LAU_12_3035_WVL_df |> 
  arrange(COUNTRY_CODE)
unique(LAU_12_3035_WVL_df$COUNTRY_CODE) # not sorted alphabetically

LAU_12_3035_WVL_df_CNTR <- LAU_12_3035_WVL_df |>
  group_by(COUNTRY_CODE) |>
  group_split() |>                                   # produces a list of tibbles
  setNames(unique(LAU_12_3035_WVL_df$COUNTRY_CODE))  # names each list element after its category



# Add the original all-countries sheet at the front
LAU_12_3035_WVL_df_CNTR <- c(list(EU27 = LAU_12_3035_WVL_df), LAU_12_3035_WVL_df_CNTR)

# Write all sheets to a new Excel file
write_xlsx(LAU_12_3035_WVL_df_CNTR, here("outputs", "Results_TCC_WVL", "LAU_12_3035_WVL_df_CNTR.xlsx"))





