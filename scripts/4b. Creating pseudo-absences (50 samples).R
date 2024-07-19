## Sampling occurrences and creating pseudo-absences
library(virtualspecies) # Generate virtual species distribution data for simulations
library(geodata)       # Download and manage geographic data for analysis
library(terra)         # Manipulate and analyze geographic data 
library(spThin)        # Perform spatial thinning of the background data


# Creating pseudo-absences (x10, x5, x3, x1) 

## b. For 50 sampled presences ----



### Pseudo-absences = presences x10 ----
# Randomly select background data within the buffer, excluding presence locations.
bg_rand_buf_50_10 <- terra::spatSample(region_buf_exclp50, length(presences50)*10, 
                                       "random", na.rm=T, as.points=TRUE, 
                                       exhaustive=T) # to ensure that we find enough samples

plot(bg, col='grey90', legend=F)
plot(region_buf50, add=T, col='grey60', legend=F)
points(bg_rand_buf_50_10, pch=19, cex=0.2)
points(presences20, pch=19, cex=0.5, col='red')


## Joining presence and absence data
# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env_50_10 <- data.frame(sp_coords50, occ=1)
# Rename columns
colnames(sp_env_50_10)[colnames(sp_env_50_10) == "x"] <- "decimalLongitude"
colnames(sp_env_50_10)[colnames(sp_env_50_10) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df_50_10 <- data.frame(terra::geom(bg_rand_buf_50_10)[,c('x','y')])
names(bg_rand_buf_df_50_10) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df_50_10$occ <- 0
summary(bg_rand_buf_df_50_10)

# Third, we bind these two data sets
sp_env_50_10 <- rbind(sp_env_50_10, bg_rand_buf_df_50_10)
summary(sp_env_50_10)

# Last, we join this combined data set with the climate data.
sp_env_50_10 <- cbind(sp_env_50_10, terra::extract(x = australia_clim1km, 
                                                   y = sp_env_50_10[,c('decimalLongitude',
                                                                       'decimalLatitude')], 
                                                   cells=T) )
summary(sp_env_50_10)




### Pseudo-absences = presences x5 ----
# Randomly select background data within the buffer, excluding presence locations.
bg_rand_buf_50_5 <- terra::spatSample(region_buf_exclp50, length(presences50)*5, 
                                      "random", na.rm=T, as.points=TRUE, 
                                      exhaustive=T) # to ensure that we find enough samples

plot(bg, col='grey90', legend=F)
plot(region_buf50, add=T, col='grey60', legend=F)
points(bg_rand_buf_50_5, pch=19, cex=0.2)
points(presences50, pch=19, cex=0.5, col='red')


## Joining presence and absence data
# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env_50_5 <- data.frame(sp_coords50, occ=1)
# Rename columns
colnames(sp_env_50_5)[colnames(sp_env_50_5) == "x"] <- "decimalLongitude"
colnames(sp_env_50_5)[colnames(sp_env_50_5) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df_50_5 <- data.frame(terra::geom(bg_rand_buf_50_5)[,c('x','y')])
names(bg_rand_buf_df_50_5) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df_50_5$occ <- 0
summary(bg_rand_buf_df_50_5)

# Third, we bind these two data sets
sp_env_50_5 <- rbind(sp_env_50_5, bg_rand_buf_df_50_5)
summary(sp_env_50_5)

# Last, we join this combined data set with the climate data.
sp_env_50_5 <- cbind(sp_env_50_5, terra::extract(x = australia_clim1km, 
                                                 y = sp_env_50_5[,c('decimalLongitude',
                                                                    'decimalLatitude')], 
                                                 cells=T) )
summary(sp_env_50_5)






### Pseudo-absences = presences x3 ----

# Randomly select background data within the buffer, excluding presence locations.
bg_rand_buf_50_3 <- terra::spatSample(region_buf_exclp50, length(presences50)*3, 
                                      "random", na.rm=T, as.points=TRUE, 
                                      exhaustive=T) # to ensure that we find enough samples

plot(bg, col='grey90', legend=F)
plot(region_buf50, add=T, col='grey60', legend=F)
points(bg_rand_buf_50_3, pch=19, cex=0.2)
points(presences50, pch=19, cex=0.5, col='red')


## Joining presence and absence data
# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env_50_3 <- data.frame(sp_coords50, occ=1)
# Rename columns
colnames(sp_env_50_3)[colnames(sp_env_50_3) == "x"] <- "decimalLongitude"
colnames(sp_env_50_3)[colnames(sp_env_50_3) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df_50_3 <- data.frame(terra::geom(bg_rand_buf_50_3)[,c('x','y')])
names(bg_rand_buf_df_50_3) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df_50_3$occ <- 0
summary(bg_rand_buf_df_50_3)

# Third, we bind these two data sets
sp_env_50_3 <- rbind(sp_env_50_3, bg_rand_buf_df_50_3)
summary(sp_env_50_3)

# Last, we join this combined data set with the climate data.
sp_env_50_3 <- cbind(sp_env_50_3, terra::extract(x = australia_clim1km, 
                                                 y = sp_env_50_3[,c('decimalLongitude',
                                                                    'decimalLatitude')], 
                                                 cells=T) )
summary(sp_env_50_3)




### Pseudo-absences = presences x1 ----

# Randomly select background data within the buffer, excluding presence locations.
bg_rand_buf_50_1 <- terra::spatSample(region_buf_exclp50, length(presences50)*1, 
                                      "random", na.rm=T, as.points=TRUE, 
                                      exhaustive=T) # to ensure that we find enough samples

plot(bg, col='grey90', legend=F)
plot(region_buf50, add=T, col='grey60', legend=F)
points(bg_rand_buf_50_1, pch=19, cex=0.2)
points(presences50, pch=19, cex=0.5, col='red')


## Joining presence and absence data
# First, we prepare the presences data to contain a column indicating 1 for presence.
sp_env_50_1 <- data.frame(sp_coords50, occ=1)
# Rename columns
colnames(sp_env_50_1)[colnames(sp_env_50_1) == "x"] <- "decimalLongitude"
colnames(sp_env_50_1)[colnames(sp_env_50_1) == "y"] <- "decimalLatitude"

# Second, we make sure the background data have the same columns, and indicate
# 0 for absence.
bg_rand_buf_df_50_1 <- data.frame(terra::geom(bg_rand_buf_50_1)[,c('x','y')])
names(bg_rand_buf_df_50_1) <- c('decimalLongitude','decimalLatitude')
bg_rand_buf_df_50_1$occ <- 0
summary(bg_rand_buf_df_50_1)

# Third, we bind these two data sets
sp_env_50_1 <- rbind(sp_env_50_1, bg_rand_buf_df_50_1)
summary(sp_env_50_1)

# Last, we join this combined data set with the climate data.
sp_env_50_1 <- cbind(sp_env_50_1, terra::extract(x = australia_clim1km, 
                                                 y = sp_env_50_1[,c('decimalLongitude',
                                                                    'decimalLatitude')], 
                                                 cells=T) )
summary(sp_env_50_1)