## Sampling occurrences and creating pseudo-absences
library(virtualspecies) # Generate virtual species distribution data for simulations
library(geodata)       # Download and manage geographic data for analysis
library(terra)         # Manipulate and analyze geographic data 
library(spThin)        # Perform spatial thinning of the background data

#Make sure you download the virtual species you created and
# upload the raster file of our region
my.species <- readRDS("MyVirtualSpecies.RDS")
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

summary(sample.o)
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


## Plot the buffer for 20 presences
region_buf20 <- terra::mask(bg, v_buf20)
plot(bg, col='grey90', legend=F)
plot(region_buf20, add=T, col='grey60', legend=F)
# Exclude presence locations:
sp_cells20 <- terra::extract(region_buf20, presences20, cells=T)$cell
region_buf_exclp20 <- region_buf20
values(region_buf_exclp20)[sp_cells20] <- NA


## 50 presences
# Exclude presence locations:
region_buf50 <- terra::mask(bg, v_buf50)
sp_cells50 <- terra::extract(region_buf50, presences50, cells=T)$cell
region_buf_exclp50 <- region_buf50
values(region_buf_exclp50)[sp_cells50] <- NA


## 100 presences
# Exclude presence locations:
region_buf100 <- terra::mask(bg, v_buf100)
sp_cells100 <- terra::extract(region_buf100, presences100, cells=T)$cell
region_buf_exclp100 <- region_buf100
values(region_buf_exclp100)[sp_cells100] <- NA


## 500 presences
# Exclude presence locations:
region_buf500 <- terra::mask(bg, v_buf500)
sp_cells500 <- terra::extract(region_buf500, presences500, cells=T)$cell
region_buf_exclp500 <- region_buf500
values(region_buf_exclp500)[sp_cells500] <- NA


## 1000 presences
# Exclude presence locations:
region_buf1000 <- terra::mask(bg, v_buf1000)
sp_cells1000 <- terra::extract(region_buf1000, presences1000, cells=T)$cell
region_buf_exclp1000 <- region_buf1000
values(region_buf_exclp1000)[sp_cells1000] <- NA




## c. Creating pseudo-absences (x10, x5, x3, x1) -------------

### 1. For 20 presences ----

# Pseudo-absences = presences x10 
# Randomly select background data within the buffer, excluding presence locations.
bg_rand_buf_20_10 <- terra::spatSample(region_buf_exclp20, length(presences20)*10, 
                                 "random", na.rm=T, as.points=TRUE, 
                                 exhaustive=T) # to ensure that we find enough samples
points(bg_rand_buf_20_10, pch=19, cex=0.2)
points(presences20, pch=19, cex=0.5, col='red')


## Joining presence and absence data
# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env_20_10 <- data.frame(sp_coords20, occ=1)
# Rename columns
colnames(sp_env_20_10)[colnames(sp_env_20_10) == "x"] <- "decimalLongitude"
colnames(sp_env_20_10)[colnames(sp_env_20_10) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df_20_10 <- data.frame(terra::geom(bg_rand_buf_20_10)[,c('x','y')])
names(bg_rand_buf_df_20_10) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df_20_10$occ <- 0
summary(bg_rand_buf_df_20_10)

# Third, we bind these two data sets
sp_env_20_10 <- rbind(sp_env_20_10, bg_rand_buf_df_20_10)
summary(sp_env_20_10)

# Last, we join this combined data set with the climate data.
sp_env_20_10 <- cbind(sp_env_20_10, terra::extract(x = australia_clim1km, 
                                       y = sp_env_20_10[,c('decimalLongitude','decimalLatitude')], 
                                       cells=T) )
summary(sp_env_20_10)

## Spatial thinning

# thin() expects that the data.frame contains a column with the species name
sp_env_20_10$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy_20_10 <- thin(sp_env_20_10, lat.col='decimalLatitude',long.col='decimalLongitude',
           spec.col='sp',
           thin.par=30,
           reps=1, 
           write.files=F,
           locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep_20_10 <- xy_20_10[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned_20_10 <- terra::cellFromXY(australia_clim1km, xy_keep_20_10)
sp_thinned_20_10 <- sp_env_20_10[sp_env_20_10$cell %in% cells_thinned_20_10,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned_20_10[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinne_20_10d$occ)], cex=0.3)

save(sp_thinned_20_10, file='data/VS_Presx20Absx10_thinned.RData')


### 3.2: n of background data points x5 -------------

# Randomly select background data within the buffer, excluding presence locations. 
# We sample 5 times as many background data as we have presences. 
# To ensure that we find enough samples, we use the argument exhaustive=T

bg_rand_buf5 <- terra::spatSample(region_buf_exclp, length(presences)*5, 
                                   "random", na.rm=T, as.points=TRUE, 
                                   exhaustive=T)
points(bg_rand_buf5, pch=19, cex=0.2)
points(presences, pch=19, cex=0.5, col='red')


## Joining presence and absence data

# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env5 <- data.frame(sp_coords, occ=1)
# Rename columns
colnames(sp_env5)[colnames(sp_env5) == "x"] <- "decimalLongitude"
colnames(sp_env5)[colnames(sp_env5) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df5 <- data.frame(terra::geom(bg_rand_buf5)[,c('x','y')])
names(bg_rand_buf_df5) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df5$occ <- 0
summary(bg_rand_buf_df5)

# Third, we bind these two data sets
sp_env5 <- rbind(sp_env5, bg_rand_buf_df5)
summary(sp_env5)

# Last, we join this combined data set with the climate data.
sp_env5 <- cbind(sp_env5, terra::extract(x = australia_clim1km, 
                                           y = sp_env5[,c('decimalLongitude','decimalLatitude')], 
                                           cells=T) )
summary(sp_env5)

## Spatial thinning

# thin() expects that the data.frame contains a column with the species name
sp_env5$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy5 <- thin(sp_env5, lat.col='decimalLatitude',long.col='decimalLongitude',
             spec.col='sp',
             thin.par=30,
             reps=1, 
             write.files=F,
             locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep5 <- xy5[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned5 <- terra::cellFromXY(australia_clim1km, xy_keep5)
sp_thinned5 <- sp_env5[sp_env5$cell %in% cells_thinned5,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned5[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned$occ)], cex=0.3)

save(sp_thinned5, file='data/VS_PresAbsx5_thinned.RData')


## 3.3: n of background data points x3 -------------

# Randomly select background data within the buffer, excluding presence locations. 
# We sample 3 times as many background data as we have presences. 
# To ensure that we find enough samples, we use the argument exhaustive=T

bg_rand_buf3 <- terra::spatSample(region_buf_exclp, length(presences)*3, 
                                  "random", na.rm=T, as.points=TRUE, 
                                  exhaustive=T)
points(bg_rand_buf3, pch=19, cex=0.2)
points(presences, pch=19, cex=0.5, col='red')


## Joining presence and absence data

# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env3 <- data.frame(sp_coords, occ=1)
# Rename columns
colnames(sp_env3)[colnames(sp_env3) == "x"] <- "decimalLongitude"
colnames(sp_env3)[colnames(sp_env3) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df3 <- data.frame(terra::geom(bg_rand_buf3)[,c('x','y')])
names(bg_rand_buf_df3) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df3$occ <- 0
summary(bg_rand_buf_df3)

# Third, we bind these two data sets
sp_env3 <- rbind(sp_env3, bg_rand_buf_df3)
summary(sp_env3)

# Last, we join this combined data set with the climate data.
sp_env3 <- cbind(sp_env3, terra::extract(x = australia_clim1km, 
                                         y = sp_env3[,c('decimalLongitude','decimalLatitude')], 
                                         cells=T) )
summary(sp_env3)

## Spatial thinning

# thin() expects that the data.frame contains a column with the species name
sp_env3$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy3 <- thin(sp_env3, lat.col='decimalLatitude',long.col='decimalLongitude',
            spec.col='sp',
            thin.par=30,
            reps=1, 
            write.files=F,
            locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep3 <- xy3[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned3 <- terra::cellFromXY(australia_clim1km, xy_keep3)
sp_thinned3 <- sp_env3[sp_env3$cell %in% cells_thinned3,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned3[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned3$occ)], cex=0.3)

save(sp_thinned3, file='data/VS_PresAbsx3_thinned.RData')



## 3.4: n of background data points x1 -------------

# Randomly select background data within the buffer, excluding presence locations. 
# We sample as many background data as we have presences. 
# To ensure that we find enough samples, we use the argument exhaustive=T

bg_rand_buf1 <- terra::spatSample(region_buf_exclp, length(presences)*1, 
                                  "random", na.rm=T, as.points=TRUE, 
                                  exhaustive=T)
points(bg_rand_buf1, pch=19, cex=0.2)
points(presences, pch=19, cex=0.5, col='red')


## Joining presence and absence data

# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env1 <- data.frame(sp_coords, occ=1)
# Rename columns
colnames(sp_env1)[colnames(sp_env1) == "x"] <- "decimalLongitude"
colnames(sp_env1)[colnames(sp_env1) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df1 <- data.frame(terra::geom(bg_rand_buf1)[,c('x','y')])
names(bg_rand_buf_df1) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df1$occ <- 0
summary(bg_rand_buf_df1)

# Third, we bind these two data sets
sp_env1 <- rbind(sp_env1, bg_rand_buf_df1)
summary(sp_env1)

# Last, we join this combined data set with the climate data.
sp_env1 <- cbind(sp_env1, terra::extract(x = australia_clim1km, 
                                         y = sp_env1[,c('decimalLongitude','decimalLatitude')], 
                                         cells=T) )
summary(sp_env1)

## Spatial thinning

# thin() expects that the data.frame contains a column with the species name
sp_env1$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy1 <- thin(sp_env1, lat.col='decimalLatitude',long.col='decimalLongitude',
            spec.col='sp',
            thin.par=30,
            reps=1, 
            write.files=F,
            locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep1 <- xy1[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned1 <- terra::cellFromXY(australia_clim1km, xy_keep1)
sp_thinned1 <- sp_env1[sp_env1$cell %in% cells_thinned1,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned1[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned1$occ)], cex=0.3)

save(sp_thinned1, file='data/VS_PresAbsx1_thinned.RData')