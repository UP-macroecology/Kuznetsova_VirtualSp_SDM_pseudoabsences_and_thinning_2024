library(virtualspecies) 
library(geodata)       
library(terra)         
library(spThin)        

#-----------------------
# 4. Spatial thinning

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


# Load region raster and pseudo-absence data
australia_clim1km <- rast("data/australia_clim1km.tif") 
pseudo_absence_data <- readRDS("data/pseudo_absence_data.RDS")

# Recreate the buffer
bg <- rast("data/bg_australia_mask.grd")
sample_sizes <- c(20, 50, 100, 500, 1000)
set.seed(123) # Set seed for reproducibility


generate_buffers <- function(presence_data, sample_sizes) {
  
  results <- list()
  
  for (n in sample_sizes) {
    # Access SpatVector from presence_data list
    presences <- presence_data[[as.character(n)]]$presences
    
    # Create buffer around presence points
    buffer_obj <- terra::buffer(presences, width=200000)
    
    # Mask the buffer using the background raster
    region_buf_obj <- terra::mask(bg, buffer_obj)
    
    # Extract presence locations within the buffer
    sp_cells <- terra::extract(region_buf_obj, presences, cells=TRUE)$cell
    
    # Create a copy of the region buffer and exclude presence locations
    region_buf_exclp_obj <- region_buf_obj
    values(region_buf_exclp_obj)[sp_cells] <- NA
    
    # Store results in a structured list
    results[[as.character(n)]] <- list(
      buffer = buffer_obj,
      region_buf = region_buf_obj,
      region_buf_exclp = region_buf_exclp_obj
    )
    
    # Plot buffer
    plot(bg, col='grey90', legend=FALSE)
    plot(region_buf_obj, add=TRUE, col='grey60', legend=FALSE)
  }
  
  return(results)
}

buffer_data <- generate_buffers(presence_data, sample_sizes)




## a. Spatial thinning with spThin() -----

thin_spThin <- function(pseudo_absence_data, clim_data, thin_distance = 30) {
  thinned_results <- list()
  
  for (key in names(pseudo_absence_data)) {
    # Add species name for spThin()
    pseudo_absence_data[[key]]$sp <- "Virtual_species"
    
    # Perform spatial thinning
    xy_thinned <- thin(
      pseudo_absence_data[[key]], 
      lat.col = "decimalLatitude", long.col = "decimalLongitude",
      spec.col = "sp",
      thin.par = thin_distance,
      reps = 1,
      write.files = FALSE,
      locs.thinned.list.return = TRUE
    )
    
    # Keep only thinned points
    xy_keep <- xy_thinned[[1]]
    
    # Extract cell numbers and subset dataset
    cells_thinned <- terra::cellFromXY(clim_data, xy_keep)
    sp_thinned <- pseudo_absence_data[[key]][pseudo_absence_data[[key]]$cell %in% cells_thinned, ]
    
    # Store result in the list
    thinned_results[[key]] <- sp_thinned
  }
  
  return(thinned_results)
}


thinned_spThin_data <- thin_spThin(pseudo_absence_data, australia_clim1km)

# Save thinned_spThin_data
saveRDS(thinned_spThin_data, "data/thinned_spThin_data.RDS")



## b. Spatial thinning to a checkerboard method ----

thin_checkerboard <- function(pseudo_absence_data, buffer_data) {
  thinned_results <- list()
  
  for (n in names(buffer_data)) {
    # Create checkerboard raster for each buffer
    r_chess <- mask(init(buffer_data[[n]]$region_buf, "chess"), buffer_data[[n]]$region_buf)
    values(r_chess)[values(r_chess) < 1] <- NA
    names(r_chess) <- "chess"
    
    for (key in names(pseudo_absence_data)) {
      if (grepl(paste0("^", n, "_"), key)) {  # Match sample size with pseudo-absence ratio
        # Apply checkerboard thinning by merging with raster cells
        sp_thinned <- merge(as.data.frame(r_chess, cell = TRUE), pseudo_absence_data[[key]], by = "cell")
        
        # Store result in the list
        thinned_results[[key]] <- sp_thinned
      }
    }
  }
  
  return(thinned_results)
}

thinned_checker_data <- thin_checkerboard(pseudo_absence_data, buffer_data)

# Save thinned_checker_data
saveRDS(thinned_checker_data, "data/thinned_checker_data.RDS")






## c. Visualise to compare ----------

par(mfrow = c(3, 2), oma = c(0, 0, 4, 0))  # 3x2 layout for plots

#
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
dev.copy(png, filename = "plots/4_thinned_data_comparison.png", width = 1600, height = 1200, res = 200)
dev.off()

