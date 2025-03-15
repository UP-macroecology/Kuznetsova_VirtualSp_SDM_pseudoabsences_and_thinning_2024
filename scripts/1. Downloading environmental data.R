library(rgbif)         
library(terra)         
library(geodata)       
library(sf)            

## (a). Downloading the environmental data ----

# Downloaded "bio 30s" from https://www.worldclim.org/data/worldclim21.html
clim1km <- worldclim_global(var = 'bio', #choosing global climate variables
                            res = 0.5, #resolution of 30 seconds ~ 1 km
                            download = T, 
                            path = 'data')

# Renaming bioclimatic variables for simplicity
names(clim1km) <- paste0("bio", 1:19)



# (b). Cropping to the area of interest ----

# Download and read the global country boundaries
world <- geodata::world(resolution = 1, path = "data")

# Extract the boundaries for Australia
australia <- world[world$NAME_0 == "Australia", ]

# Define the extent for the mask based on the Australia boundaries
# The coordinates are: xmin, xmax, ymin, ymax
australia_extent <- ext(113, 154, -44, -10)

# Create a blank raster with the same extent as your climate data
mask_template <- rast(extent = australia_extent, resolution = 0.005)

# Rasterize the Australia boundaries to match the climate data resolution
australia_mask <- rasterize(australia, mask_template, field = "NAME_0")

# Convert the rasterized mask to a binary mask (1 for land, NA for ocean)
australia_mask <- ifel(!is.na(australia_mask), 1, NA)

# Convert the raster mask to polygons
australia_polygons <- as.polygons(australia_mask, na.rm = TRUE)

# Convert to a shapefile object
australia_sf <- st_as_sf(australia_polygons)

# Crop the global climate data to Australia
australia_clim1km <- crop(clim1km, australia_sf)

# Mask the cropped climate data to the exact shape of Australia
australia_clim1km <- mask(australia_clim1km, australia_sf)


# Plot the cropped layers 
# We are interested in layers bio10 and bio14 
# (mean temperature of the warmest quarter, precipitation of the driest month)
plot(australia_clim1km[[c("bio10", "bio14")]])

# Save the cropped climate data for Australia as a raster file
writeRaster(australia_clim1km, filename = "data/australia_clim1km.tif",
            overwrite = TRUE)
