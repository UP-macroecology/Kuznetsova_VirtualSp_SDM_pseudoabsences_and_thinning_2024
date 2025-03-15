library(geodata)       
library(terra)         
library(virtualspecies) 

#-------------------

# Load VS and region mask data
sim_sp1_pa <- readRDS("data/MyVirtualSpecies.RDS")
australia_clim1km <- rast("data/australia_clim1km.tif")

## a. Sampling  occurrences (x20, x50, x100, x500, x1000) ----

# This can be done with the function sampleOccurrences, with which you can 
# sample either “presence-absence” or “presence only” occurrence data. 
# The function sampleOccurrences also provides the possibility to introduce a 
# number of sampling biases, such as uneven spatial sampling intensity, 
# probability of detection, and probability of error.

# The function sample_presences() generates presence points for different 
# sample sizes and returns a list called "presence_data" containing occurrence 
# data, coordinates, and a SpatVector for each sample size.

# Define sample sizes
sample_sizes <- c(20, 50, 100, 500, 1000)
set.seed(123) # Set seed for reproducibility


sample_presences <- function(sim_sp, clim_data, sample_sizes) {
  
  results <- list()
  
  for (n in sample_sizes) {
    # Sample occurrences
    sample_occurrences <- sampleOccurrences(sim_sp, n = n, type = "presence only")
    
    # Extract coordinates
    sp_coords <- sample_occurrences$sample.points[1:2]
    
    # Create presence vector
    presences <- terra::vect(as.matrix(sp_coords), crs=crs(clim_data))
    
    # Store results in a structured way
    results[[as.character(n)]] <- list(
      occurrences = sample_occurrences,
      coords = sp_coords,
      presences = presences
    )
  }
  
  return(results)
}


set.seed(123) # Set seed for reproducibility
presence_data <- sample_presences(sim_sp1_pa, australia_clim1km, sample_sizes)






## b. Creating a 200 km buffer for pseudo-absences ----------------

# We place a buffer of 200 km around our virtual species records and sample
# background points randomly from within the buffer but excluding presence locations.

# The function generate_buffers() creates a 200 km buffer around the presence points 
# created in (a), applies a background mask, and excludes presence locations 
# from the buffer, storing the results as SpatVector and SpatRaster objects in 
# the list called "buffer_data".

# Create background mask
bg <- australia_clim1km[[1]]
writeRaster(bg, "data/bg_australia_mask.grd", overwrite=TRUE)
values(bg)[!is.na(values(bg))] <- 1


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

# Save buffer_data
saveRDS(buffer_data, "data/buffer_data.RDS")




## c. Creating pseudo-absences (presences x10 / x5 / x3 / x1) ----------

# Pseudo-absences are generated within a 200 km buffer around the sampled presence 
# points while ensuring that presence locations are excluded. This process allows 
# for spatially constrained background sampling, improving the accuracy of species distribution models.

# The function generate_pseudo_absences() creates pseudo-absence points at 
# different ratios (10x, 5x, 3x, 1x) relative to the number of presence points. 
# It returns a structured list called pseudo_absence_data, where each entry 
# contains a data frame with decimal coordinates, presence-absence labels, and 
# extracted environmental values. The pseudo-absence points are sampled randomly 
# from the exclusion buffer (region_buf_exclp) to ensure they do not overlap 
# with presence locations.

# Define sample sizes and pseudo-absence ratios
sample_sizes <- c(20, 50, 100, 500, 1000)
pa_ratios <- c(10, 5, 3, 1)

generate_pseudo_absences <- function(presence_data, buffer_data, clim_data, sample_sizes, pa_ratios) {
  results <- list()
  
  for (n in sample_sizes) {
    # Access the correct presence points and exclusion buffer from lists
    presences <- presence_data[[as.character(n)]]$presences
    region_buf_exclp <- buffer_data[[as.character(n)]]$region_buf_exclp
    region_buf <- buffer_data[[as.character(n)]]$region_buf
    
    # Generate pseudo-absences at different ratios
    for (ratio in pa_ratios) {
      
      key <- paste0(n, "_pa", ratio)  # Unique key for dataset
      
      # Randomly select background data within the buffer, excluding presence locations
      bg_rand_obj <- terra::spatSample(region_buf_exclp, length(presences) * ratio, 
                                       "random", na.rm=TRUE, as.points=TRUE, 
                                       exhaustive=TRUE)
      
      # Plot presence and pseudo-absence points
      plot(clim_data[[1]], col='grey90', legend=FALSE)
      plot(region_buf, add=TRUE, col='grey60', legend=FALSE)
      points(bg_rand_obj, pch=19, cex=0.2)
      points(presences, pch=19, cex=0.5, col='red')
      
      # Prepare presence data
      sp_env_obj <- data.frame(presence_data[[as.character(n)]]$coords, occ=1)
      colnames(sp_env_obj) <- c("decimalLongitude", "decimalLatitude", "occ")
      
      # Prepare pseudo-absence data
      bg_rand_buf_df <- data.frame(terra::geom(bg_rand_obj)[, c('x', 'y')])
      names(bg_rand_buf_df) <- c("decimalLongitude", "decimalLatitude")
      bg_rand_buf_df$occ <- 0
      
      # Combine presence and pseudo-absence data
      sp_env_obj <- rbind(sp_env_obj, bg_rand_buf_df)
      
      # Join with climate data
      sp_env_obj <- cbind(sp_env_obj, terra::extract(x = clim_data, 
                                                     y = sp_env_obj[, c('decimalLongitude', 'decimalLatitude')], 
                                                     cells=TRUE))
      
      # Store results in the structured list
      results[[key]] <- sp_env_obj
    }
  }
  
  return(results)
}

# Usage
pseudo_absence_data <- generate_pseudo_absences(presence_data, buffer_data, australia_clim1km, sample_sizes, pa_ratios)

# Save pseudo_absence_data
saveRDS(pseudo_absence_data, "data/pseudo_absence_data.RDS")
