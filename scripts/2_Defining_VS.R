library(virtualspecies) 
library(geodata)       
library(terra)    

#---------------------------

## 2. Creating virtual species (VS).

## (a). Defining VS by their response to the environmental variables -----

# These responses are then combined to calculate the environmental suitability 
# of the virtual species. Here we create a species inhabiting the coastal region of
# Australia with the according average values of annual temperature and 
# precipitation. The species is inspired by the Eastern Water Dragon.

set.seed(123)
# The first step is to provide to the helper function formatFunctions which 
# responses we want for which variables
optima_temp = 2 # optimal mean temperature of the coldest month; 
sd_temp = 2.5 # possible deviation from the temperature;
optima_prec = 20 # optimal precipitation seasonality in % (coastal regions are more buffered, less seasonal, than the interior); 
sd_prec = 7 # possible deviation from the precipitation seasonality in %.

sp1_param <- formatFunctions(bio6 = c(fun = 'dnorm', mean = optima_temp, sd = sd_temp),
                             bio15 = c(fun = 'dnorm', mean = optima_prec, sd = sd_prec))

# Generating the virtual species niche and plotting the spatial distribution of 
# the species’ environmental suitability

# upload the raster file of our region
australia_clim1km <- rast("data/australia_clim1km.tif") 

# First upload the raster file of our region and 
sim_sp1 <- generateSpFromFun(raster.stack = australia_clim1km[[c("bio6", "bio15")]],
                             parameters = sp1_param,
                             species.type = "additive",
                             plot = TRUE)
sim_sp1





## (b). Conversion of environmental suitability into probability of occurrence ----

# We use the logistic function for the probabilities to obtain values 
# between 0 and 1. A positive beta indicates the inflexion point of the curve, 
# the higher the beta, the lower the probability of finding the suitable 
# environmental conditions for the species. When environmental suitability is 
# very low (𝑥≈ 0), a negative α (like -0.05) means the species is even less 
# likely to be present. By default, the function performs a logistic conversion
# with random parameters, so it is better to define them ourselves.

# This is the step where we create true presence and absence points.

sim_sp1_pa <- convertToPA(sim_sp1,
                          PA.method = "probability",
                          prob.method = "logistic", beta = 0.8, alpha = -0.05) 

plotSuitabilityToProba(sim_sp1_pa)

summary(as.data.frame(sim_sp1_pa$pa.raster, xy = TRUE))



## (c). Exploring the virtual species ----

# Here are various helpful functions to look at the set-up parameters for the virtual species.

plotResponse(sim_sp1_pa) #plot the species-environment relationship
plotSuitabilityToProba(sim_sp1_pa) #plot the relationship between suitability and probability of occurrence
str(sim_sp1_pa) #extracting raster of the environmental suitability
sim_sp1_pa$suitab.raster
sim_sp1_pa$details$variables
sim_sp1_pa$details$parameters

# Saving the virtual species objects for later use
saveRDS(sim_sp1_pa, file = "data/MyVirtualSpecies.RDS")
#sim_sp1_pa <- readRDS("data/MyVirtualSpecies.RDS")

# Save true presences and absences as a separate dataframe.
sim_sp1_pa.df <- as.data.frame(sim_sp1_pa$pa.raster, xy = TRUE)

write.csv(sim_sp1_pa.df, file = "data/VS.dataframe.csv", row.names = FALSE)



## (d). Combining VS df with climate variables for later

# Rename columns
colnames(sim_sp1_pa.df)[colnames(sim_sp1_pa.df) == "x"] <- "decimalLongitude"
colnames(sim_sp1_pa.df)[colnames(sim_sp1_pa.df) == "y"] <- "decimalLatitude"
colnames(sim_sp1_pa.df)[colnames(sim_sp1_pa.df) == "lyr.1"] <- "occ"

# Join this combined data set with the climate data.
sim_sp1_pa_env.df <- cbind(sim_sp1_pa.df, terra::extract(x = australia_clim1km, 
                                                         y = sim_sp1_pa.df[,c('decimalLongitude',
                                                                              'decimalLatitude')], 
                                                         cells=T) )
summary(sim_sp1_pa_env.df)

write.csv(sim_sp1_pa_env.df, "data/sim_sp1_pa_env_df.csv")
sim_sp1_pa_env.df <- read.csv("data/sim_sp1_pa_env_df.csv")

