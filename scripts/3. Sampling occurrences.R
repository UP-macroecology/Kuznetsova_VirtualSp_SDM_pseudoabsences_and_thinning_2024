## Sampling occurrences and creating pseudo-absences
library(geodata)       # Download and manage geographic data for analysis
library(terra)         # Manipulate and analyze geographic data
library(virtualspecies) # Generate virtual species distribution data for simulations


# Make sure you download the virtual species you created and
# upload the raster file of our region
sim_sp1_pa <- readRDS("data/MyVirtualSpecies.RDS")
australia_clim1km <- rast("data/australia_clim1km.tif") 

## a. Sampling  occurrences (x20, x50, x100, x500, x1000) ----

# This can be done with the function sampleOccurrences, with which you can 
# sample either “presence-absence” or “presence only” occurrence data. 
# The function sampleOccurrences also provides the possibility to introduce a 
# number of sampling biases, such as uneven spatial sampling intensity, 
# probability of detection, and probability of error.

# We will sample 'presence only' occurrences
# For later comparison we create data frames of 20, 50, 100, 500 and 1000 points

set.seed(123) #set the seed for reproducibility

sample.occ20 <- sampleOccurrences(sim_sp1_pa,
                                     n = 20, # The number of points to sample
                                     type = "presence only")
sample.occ50 <- sampleOccurrences(sim_sp1_pa,
                                       n = 50, # The number of points to sample
                                       type = "presence only")
sample.occ100 <- sampleOccurrences(sim_sp1_pa,
                                       n = 100, # The number of points to sample
                                       type = "presence only")
sample.occ500 <- sampleOccurrences(sim_sp1_pa,
                                       n = 500, # The number of points to sample
                                       type = "presence only")
sample.occ1000 <- sampleOccurrences(sim_sp1_pa,
                                       n = 1000, # The number of points to sample
                                       type = "presence only")

summary(sample.occ20)

# Since our sample points only have observed occurrences matching real occurrences,
# we will remove the redundant data and only leave the coordinates.

sp_coords20 <- sample.occ20$sample.points[1:2]
sp_coords50 <- sample.occ50$sample.points[1:2]
sp_coords100 <- sample.occ100$sample.points[1:2]
sp_coords500 <- sample.occ500$sample.points[1:2]
sp_coords1000 <- sample.occ1000$sample.points[1:2]


## b. Creating a 200 km buffer for pseudo-absences ----------------

# We place a buffer of 200 km around our virtual species records and sample
# background points randomly from within the buffer but excluding presence locations.

# Make SpatVector:
presences20 <- terra::vect(as.matrix(sp_coords20), crs=crs(australia_clim1km))
presences50 <- terra::vect(as.matrix(sp_coords50), crs=crs(australia_clim1km))
presences100 <- terra::vect(as.matrix(sp_coords100), crs=crs(australia_clim1km))
presences500 <- terra::vect(as.matrix(sp_coords500), crs=crs(australia_clim1km))
presences1000 <- terra::vect(as.matrix(sp_coords1000), crs=crs(australia_clim1km))

# Then, place a buffer of 200 km radius around our presence points
v_buf20 <- terra::buffer(presences20, width=200000)
v_buf50 <- terra::buffer(presences50, width=200000)
v_buf100 <- terra::buffer(presences100, width=200000)
v_buf500 <- terra::buffer(presences500, width=200000)
v_buf1000 <- terra::buffer(presences1000, width=200000)

# Create a background mask with target resolution and extent from climate layers
# Set all raster cells outside the buffer to NA.
bg <- australia_clim1km[[1]]

# Save the mask as a .grd file for future use
writeRaster(bg, "data/bg_australia_mask.grd", overwrite=TRUE)
values(bg)[!is.na(values(bg))] <- 1


## Plot the buffer for 20 sampled presences
region_buf20 <- terra::mask(bg, v_buf20)
plot(bg, col='grey90', legend=F)
plot(region_buf20, add=T, col='grey60', legend=F)
# Exclude presence locations:
sp_cells20 <- terra::extract(region_buf20, presences20, cells=T)$cell
region_buf_exclp20 <- region_buf20
values(region_buf_exclp20)[sp_cells20] <- NA


## for 50 sampled presences
# Exclude presence locations:
region_buf50 <- terra::mask(bg, v_buf50)
sp_cells50 <- terra::extract(region_buf50, presences50, cells=T)$cell
region_buf_exclp50 <- region_buf50
values(region_buf_exclp50)[sp_cells50] <- NA
plot(bg, col='grey90', legend=F)
plot(region_buf50, add=T, col='grey60', legend=F)

## for 100 sampled presences
# Exclude presence locations:
region_buf100 <- terra::mask(bg, v_buf100)
sp_cells100 <- terra::extract(region_buf100, presences100, cells=T)$cell
region_buf_exclp100 <- region_buf100
values(region_buf_exclp100)[sp_cells100] <- NA


## for 500 sampled presences
# Exclude presence locations:
region_buf500 <- terra::mask(bg, v_buf500)
sp_cells500 <- terra::extract(region_buf500, presences500, cells=T)$cell
region_buf_exclp500 <- region_buf500
values(region_buf_exclp500)[sp_cells500] <- NA


## for 1000 sampled presences
# Exclude presence locations:
region_buf1000 <- terra::mask(bg, v_buf1000)
sp_cells1000 <- terra::extract(region_buf1000, presences1000, cells=T)$cell
region_buf_exclp1000 <- region_buf1000
values(region_buf_exclp1000)[sp_cells1000] <- NA
