

# MERGING ORIGINAL HIGH DENSITY CLUSTERS WITH URBAN CLUSTERS

# Packages and options ----

pacman::p_load(here, terra, tictoc)

terraOptions(memfrac = 0.05) # my own need, based on the server where I am running analyses, feel free to ignore


# Load data (the links are available in the FAQ website of the NRR) ----

ucentres <- rast(here("data", "UCUC_2021", "HDENS-CLST-2021", "HDENS_CLST_2021.tif"))
ucentres

uclusters <- rast(here("data", "UCUC_2021", "URBAN-CLST-2021", "URBAN_CLST_2021.tif"))
uclusters


# Check extents and match if necessary ----

ext(ucentres)
ext(uclusters)
crs(ucentres) == crs(uclusters)
res(ucentres) == res(uclusters)

# The extent of 'uclusters' is larger than ucentres, so we need to go for the biggest extent, to keep everything

combined_ext <- ext(terra::union(ext(ucentres), ext(uclusters)))

# Extend ucentres to the larger extent

ucentres <- extend(ucentres, combined_ext)

# Check again

ext(ucentres) == ext(uclusters)
crs(ucentres) == crs(uclusters)
res(ucentres) == res(uclusters)

# All TRUE, so we proceed to merging them.


# Merge with terra::ifel ----
# The original rasters don't have 0 and 1, for some reason, each centre or cluster has either a seemingly random number or zero. That number indicates that there is cluster or centre there, so for the merge with ifelse, I used > 0 instead of == 1.

tic()

UCUC_1km <- terra::ifel(ucentres > 0 | uclusters > 0, 1, NA, 
                        filename = here("outputs", "UCUC", "UCUC_1km.tif"),
                        overwrite = TRUE,
                        datatype = "INT1U",
                        progress = TRUE)

toc() # 1.002 sec elapsed


UCUC_1km
names(UCUC_1km) <- "UCUC_1km"
varnames(UCUC_1km) <- "UCUC_1km"
UCUC_1km


# Compare with the one produced by Grazia Zulian ----

UCUC_gr <- rast(here("data", "UCUC_2021", "CLUSTERS_2021_gdb", "ucuc_2021.tif"))


compareGeom(UCUC_1km, UCUC_gr)

res(UCUC_1km) == res(UCUC_gr)

crs(UCUC_1km) == crs(UCUC_gr)
crs(UCUC_1km, describe = TRUE)
crs(UCUC_gr, describe = TRUE)


ext(UCUC_1km) == ext(UCUC_gr) # FALSE --> Grazia's has a larger extent (the same as CLC, I checked)
ext(UCUC_1km)
ext(UCUC_gr)


combined_ext_ucuc <- ext(terra::union(ext(UCUC_1km), ext(UCUC_gr)))

UCUC_1km <- extend(UCUC_1km, UCUC_gr)


UCUC_gr - UCUC_1km # with a simple subtraction, you see min and max value are zero, hence, all pixels are the same

# class       : SpatRaster 
# dimensions  : 4600, 6500, 1  (nrow, ncol, nlyr)
# resolution  : 1000, 1000  (x, y)
# extent      : 9e+05, 7400000, 9e+05, 5500000  (xmin, xmax, ymin, ymax)
# coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
# source(s)   : memory
# varname     : ucuc_2021 
# name        : Class 
# min value   :     0
# max value   :     0


# A bit fancier option

all(values(UCUC_1km) == values(UCUC_gr), na.rm = TRUE)
# TRUE

all.equal(UCUC_1km, UCUC_gr)
# some discrepancy in the metadata, but I don't think it is relevant



