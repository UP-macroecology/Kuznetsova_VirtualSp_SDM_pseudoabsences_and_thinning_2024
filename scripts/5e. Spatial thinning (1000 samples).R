## Sampling occurrences and creating pseudo-absences
library(virtualspecies) # Generate virtual species distribution data for simulations
library(geodata)       # Download and manage geographic data for analysis
library(terra)         # Manipulate and analyze geographic data 
library(spThin)        # Perform spatial thinning of the background data

#Make sure you download the virtual species you created and
# upload the raster file of our region
my.species <- readRDS("MyVirtualSpecies.RDS")
australia_clim1km <- rast("data/australia_clim1km.tif") 



# b. Spatial thinning for dfs of 1000 presences 
## Pseudo-absences = presences x10 ----
# thin() expects that the data.frame contains a column with the species name
sp_env_1000_10$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy_1000_10 <- thin(sp_env_1000_10, lat.col='decimalLatitude', long.col='decimalLongitude',
                   spec.col='sp',
                   thin.par=30,
                   reps=1, 
                   write.files=F,
                   locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep_1000_10 <- xy_1000_10[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned_1000_10 <- terra::cellFromXY(australia_clim1km, xy_keep_1000_10)
sp_thinned_1000_10 <- sp_env_1000_10[sp_env_1000_10$cell %in% cells_thinned_1000_10,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned_1000_10[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned_1000_10$occ)], cex=0.3)

save(sp_thinned_1000_10, file='data/VS_Presx1000Absx10_thinned.RData')




## Pseudo-absences = presences x5 ----

# thin() expects that the data.frame contains a column with the species name
sp_env_1000_5$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy_1000_5 <- thin(sp_env_1000_5, lat.col='decimalLatitude', long.col='decimalLongitude',
                  spec.col='sp',
                  thin.par=30,
                  reps=1, 
                  write.files=F,
                  locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep_1000_5 <- xy_1000_5[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned_1000_5 <- terra::cellFromXY(australia_clim1km, xy_keep_1000_5)
sp_thinned_1000_5 <- sp_env_1000_5[sp_env_1000_5$cell %in% cells_thinned_1000_5,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned_1000_5[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned_1000_5$occ)], cex=0.3)

save(sp_thinned_1000_5, file='data/VS_Presx1000Absx5_thinned.RData')




## Pseudo-absences = presences x3 ----

# thin() expects that the data.frame contains a column with the species name
sp_env_1000_3$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy_1000_3 <- thin(sp_env_1000_3, lat.col='decimalLatitude', long.col='decimalLongitude',
                  spec.col='sp',
                  thin.par=30,
                  reps=1, 
                  write.files=F,
                  locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep_1000_3 <- xy_1000_3[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned_1000_3 <- terra::cellFromXY(australia_clim1km, xy_keep_1000_3)
sp_thinned_1000_3 <- sp_env_1000_3[sp_env_1000_3$cell %in% cells_thinned_1000_3,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned_1000_3[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned_1000_3$occ)], cex=0.3)

save(sp_thinned_1000_3, file='data/VS_Presx1000Absx3_thinned.RData')




## Pseudo-absences = presences x1 ----

# thin() expects that the data.frame contains a column with the species name
sp_env_1000_1$sp <- 'Virtual_species'

# Remove adjacent cells of presence/background data:
xy_1000_1 <- thin(sp_env_1000_1, lat.col='decimalLatitude', long.col='decimalLongitude',
                  spec.col='sp',
                  thin.par=30,
                  reps=1, 
                  write.files=F,
                  locs.thinned.list.return=T)

# Keep the coordinates with the most presence records
xy_keep_1000_1 <- xy_1000_1[[1]]

# Thin the dataset - here, we first extract the cell numbers for the thinned 
# coordinates and then use these to subset our data frame.
cells_thinned_1000_1 <- terra::cellFromXY(australia_clim1km, xy_keep_1000_1)
sp_thinned_1000_1 <- sp_env_1000_1[sp_env_1000_1$cell %in% cells_thinned_1000_1,]

# Plot the map and data
plot(bg, col='grey90', legend=F)
points(sp_thinned_1000_1[,1:2], pch=19,
       col=c('black','red')[as.factor(sp_thinned_1000_1$occ)], cex=0.3)

save(sp_thinned_1000_1, file='data/VS_Presx1000Absx1_thinned.RData')




## Plot the results ----

# Set up a 2x2 plotting layout
par(mfrow=c(2,2), oma=c(0, 0, 4, 0))  # Adjust outer margins to make space for the main title

# Plot for x10 absences
plot(bg, col='grey90', legend=FALSE, main="x10 absences")
points(sp_thinned_1000_10[,1:2], pch=19, col=c('black', 'red')[as.factor(sp_thinned_1000_10$occ)], cex=0.3)

# Plot for x5 absences
plot(bg, col='grey90', legend=FALSE, main="x5 absences")
points(sp_thinned_1000_5[,1:2], pch=19, col=c('black', 'red')[as.factor(sp_thinned_1000_5$occ)], cex=0.3)

# Plot for x3 absences
plot(bg, col='grey90', legend=FALSE, main="x3 absences")
points(sp_thinned_1000_3[,1:2], pch=19, col=c('black', 'red')[as.factor(sp_thinned_1000_3$occ)], cex=0.3)

# Plot for x1 absences
plot(bg, col='grey90', legend=FALSE, main="x1 absences")
points(sp_thinned_1000_1[,1:2], pch=19, col=c('black', 'red')[as.factor(sp_thinned_1000_1$occ)], cex=0.3)

# Add the main title to the entire plotting area
mtext("Thinned absences with thin(). Data with 1000 presences.", outer=TRUE, line=2, cex=1.5)

# Save the current plot to a PNG file
dev.copy(png, filename="data/thinned_abs_plots_1000prsncs.png", width=1600, height=1200, res=200)
dev.off()


