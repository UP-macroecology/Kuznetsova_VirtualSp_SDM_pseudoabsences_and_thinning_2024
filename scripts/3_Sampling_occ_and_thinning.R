library(geodata)       
library(terra)         
library(virtualspecies) 
library(spThin) 

# -------------------
# 3. Presence/Absence Sampling and Spatial Thinning ---------

# Load VS and region mask data
cat("Loading VS and region mask data... \n")
sim_sp1_pa <- readRDS("data/MyVirtualSpecies.RDS")
australia_clim1km <- rast("data/australia_clim1km.tif") # loading the climate data
bg <- rast("data/bg_australia_mask.grd")

# Load functions
source("scripts/0_Functions.R")

set.seed(123) # Set seed for reproducibility

# Define sample sizes and pseudo-absence ratios
sample_sizes <- c(20, 50, 100, 500, 1000)
pa_ratios <- c(10, 5, 3, 1)



## a. Sampling  occurrences (x20, x50, x100, x500, x1000) ----

# The function sample_presences() uses the function sampleOccurrences and generates 
# presence points for different sample sizes. It returns a list called "presence_data"
# containing occurrence data, coordinates, and a SpatVector for each sample size.

cat("Sampling occurrences...\n")
presence_data <- sample_presences(sim_sp1_pa, australia_clim1km, sample_sizes)



## b. Creating a 200 km buffer for pseudo-absences ----------------

# The function generate_buffers() creates a 200 km buffer around the presence points 
# created in (a), applies a background mask, and excludes presence locations 
# from the buffer, storing the results as SpatVector and SpatRaster objects in 
# the list called "buffer_data".

cat("Creating 200 km buffers...\n")
buffer_data <- generate_buffers(presence_data, sample_sizes, bg)



## c. Creating pseudo-absences (presences x10 / x5 / x3 / x1) ----------

# Pseudo-absences are generated within a 200 km buffer around the sampled presence 
# points while ensuring that presence locations are excluded. 

# The function generate_pseudo_absences() creates pseudo-absence points at 
# different ratios (10x, 5x, 3x, 1x) relative to the number of presence points. 
# It returns a structured list called pseudo_absence_data, where each entry 
# contains a data frame with decimal coordinates, presence-absence labels, and 
# extracted environmental values. The pseudo-absence points are sampled randomly 
# from the exclusion buffer (region_buf_exclp) to ensure they do not overlap 
# with presence locations.

cat("Sampling pseudo-absences...\n")
pseudo_absence_data <- generate_pseudo_absences(presence_data, buffer_data, australia_clim1km, sample_sizes, pa_ratios)



## d. Spatial thinning -----------------------

# The following script performs spatial thinning of presence and pseudo-absence points 
# using two methods: the spThin() function and a checkerboard method. The goal is to 
# reduce spatial autocorrelation and improve species distribution modeling. 

# The function thin_spThin() applies the spThin() method to remove closely located 
# occurrences based on a specified minimum distance. It returns a list of 
# spatially thinned datasets for different presence-absence ratios.

# The function thin_checkerboard() applies a checkerboard thinning approach, 
# filtering points based on a rasterized grid pattern. It returns a list of 
# datasets where occurrences are aligned with the checkerboard grid.

# The final section visualizes the results, comparing original presence-absence 
# data with spatially thinned versions to illustrate differences in point 
# distribution.


### i. Spatial thinning with spThin() 
cat("Thinning the data with spThin()...\n")
thinned_spThin_data <- thin_spThin(pseudo_absence_data, australia_clim1km)
saveRDS(thinned_spThin_data, "data/thinned_spThin_data.RDS")


### ii. Spatial thinning with checkerboard method 
cat("Thinning the data with thin_checkerboard()...\n")
thinned_checker_data <- thin_checkerboard(pseudo_absence_data, buffer_data)
saveRDS(thinned_checker_data, "data/thinned_checker_data.RDS") 




## e. Visualise for comparison ----------

par(mfrow = c(3, 2), oma = c(0, 0, 4, 0))  # 3x2 layout for plots


plot(australia_clim1km[[1]], col = "grey90", legend = FALSE, main = "100 pres, x10")
points(pseudo_absence_data[["100_pa10"]][, 1:2], pch = 19, 
       col = c("black", "red")[as.factor(pseudo_absence_data[["100_pa10"]]$occ)], cex = 0.3)

plot(australia_clim1km[[1]], col = "grey90", legend = FALSE, main = "100 pres, x5")
points(pseudo_absence_data[["100_pa5"]][, 1:2], pch = 19, 
       col = c("black", "red")[as.factor(pseudo_absence_data[["100_pa5"]]$occ)], cex = 0.3)

plot(australia_clim1km[[1]], col = "grey90", legend = FALSE, main = "100 pres, x10 (spThin)")
points(thinned_spThin_data[["100_pa10"]][, 1:2], pch = 19, 
       col = c("black", "red")[as.factor(thinned_spThin_data[["100_pa10"]]$occ)], cex = 0.3)

plot(australia_clim1km[[1]], col = "grey90", legend = FALSE, main = "100 pres, x5 (spThin)")
points(thinned_spThin_data[["100_pa5"]][, 1:2], pch = 19, 
       col = c("black", "red")[as.factor(thinned_spThin_data[["100_pa5"]]$occ)], cex = 0.3)

plot(australia_clim1km[[1]], col = "grey90", legend = FALSE, main = "100 pres, x10 (checker)")
points(thinned_checker_data[["100_pa10"]][, 3:4], pch = 19, 
       col = c("black", "red")[as.factor(thinned_checker_data[["100_pa10"]]$occ)], cex = 0.3)

plot(australia_clim1km[[1]], col = "grey90", legend = FALSE, main = "100 pres, x5 (checker)")
points(thinned_checker_data[["100_pa5"]][, 3:4], pch = 19, 
       col = c("black", "red")[as.factor(thinned_checker_data[["100_pa5"]]$occ)], cex = 0.3)

# Add title
mtext("Thinned Data Method Comparison", outer = TRUE, line = 2, cex = 1.5)

# Save plot
dev.copy(png, filename = "plots/3_thinned_data_comparison.png", width = 1600, height = 1200, res = 200)
dev.off()
