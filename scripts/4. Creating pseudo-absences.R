## Sampling occurrences and creating pseudo-absences
library(virtualspecies) # Generate virtual species distribution data for simulations
library(geodata)       # Download and manage geographic data for analysis
library(terra)         # Manipulate and analyze geographic data 
library(spThin)        # Perform spatial thinning of the background data



# Creating pseudo-absences (x10, x5, x3, x1) 

## a. For 20 sampled presences ----

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