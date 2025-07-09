

#---------------
# 0. Custom functions 



# a. Presences sampling -------------

# The function sample_presences() uses the function sampleOccurrences and generates 
# presence points for different sample sizes. It returns a list called "presence_data"
# containing occurrence data, coordinates, and a SpatVector for each sample size.

sample_presences <- function(sim_sp, clim_data, sample_sizes) {
  
  results <- list()
  
  for (n in sample_sizes) {
    # Sample occurrences
    sample_occurrences <- sampleOccurrences(sim_sp, n = n, type = "presence only", plot=F)
    
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




# b. Presences sampling -------------

# The function generate_buffers() creates a 200 km buffer around the presence points 
# created in (a), applies a background mask, and excludes presence locations 
# from the buffer, storing the results as SpatVector and SpatRaster objects in 
# the list called "buffer_data".

generate_buffers <- function(presence_data, sample_sizes, bg) {
  
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
    #plot(bg, col='grey90', legend=FALSE)
    #plot(region_buf_obj, add=TRUE, col='grey60', legend=FALSE)
  }
  
  return(results)
}






# c. Background data generation ----------------

# The function generate_pseudo_absences() creates pseudo-absence points at 
# different ratios (10x, 5x, 3x, 1x) relative to the number of presence points. 
# It returns a structured list called pseudo_absence_data, where each entry 
# contains a data frame with decimal coordinates, presence-absence labels, and 
# extracted environmental values. The pseudo-absence points are sampled randomly 
# from the exclusion buffer (region_buf_exclp) to ensure they do not overlap 
# with presence locations.

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
      #plot(clim_data[[1]], col='grey90', legend=FALSE)
      #plot(region_buf, add=TRUE, col='grey60', legend=FALSE)
      #points(bg_rand_obj, pch=19, cex=0.2)
      #points(presences, pch=19, cex=0.5, col='red')
      
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





# d. Thinning with spThin --------------------

# The function thin_spThin() applies the spThin() method to remove closely located 
# occurrences based on a specified minimum distance. It returns a list of 
# spatially thinned datasets for different presence-absence ratios.


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



# e. Thinning with a checkerboard method -----------

# The function thin_checkerboard() applies a checkerboard thinning approach, 
# filtering points based on a rasterized grid pattern. It returns a list of 
# datasets where occurrences are aligned with the checkerboard grid.


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

