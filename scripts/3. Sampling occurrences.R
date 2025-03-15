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

