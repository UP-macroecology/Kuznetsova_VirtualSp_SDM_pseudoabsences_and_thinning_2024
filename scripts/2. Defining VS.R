## 2. Creating virtual species (VS)

library(virtualspecies) # Generate virtual species distribution data for simulations
library(geodata)       # Download and manage geographic data for analysis
library(terra)         # Manipulate and analyze geographic data 



## (a). Defining VS by their response to the environmental variables -----

# These responses are then combined to calculate the environmental suitability 
# of the virtual species. Here we create a species inhabiting the coastal region of
# Australia with the according average values of annual temperature and 
# precipitation. The species is inspired by the Eastern Water Dragon.

# The first step is to provide to the helper function formatFunctions which 
# responses we want for which variables
optima_temp = 18 # optimal mean temperature of the warmest quarter; 
sd_temp = 4 # possible deviation from the temperature
optima_prec = 75 # optimal precipitation of the driest month; 
sd_prec = 40 # possible deviation from the precipitation level

sp1_param <- formatFunctions(bio10 = c(fun = 'dnorm', mean = optima_temp, sd = sd_temp),
                             bio14 = c(fun = 'dnorm', mean = optima_prec, sd = sd_prec))

# Generating the virtual species niche and plotting the spatial distribution of 
# the species’ environmental suitability

# upload the raster file of our region
australia_clim1km <- rast("data/australia_clim1km.tif") 

# First upload the raster file of our region and 
sim_sp1 <- generateSpFromFun(raster.stack = australia_clim1km[[c("bio10", "bio14")]],
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
                          prob.method = "logistic", beta = 0.55, alpha = -0.05) 

plotSuitabilityToProba(sim_sp1_pa)

summary(as.data.frame(sim_sp1_pa$pa.raster, xy = TRUE))

# Save true presences and absences as a separate dataframe.
sim_sp1_pa.df <- as.data.frame(sim_sp1_pa$pa.raster, xy = TRUE)

write.csv(sim_sp1_pa.df, file = "data/VS.dataframe.csv", row.names = FALSE)


## c. Exploring the virtual species ----

# Here are various functions to look at the set-up parameters for our virtual species.

plotResponse(sim_sp1_pa) #plot the species-environment relationship
plotSuitabilityToProba(sim_sp1_pa) #plot the relationship between suitability and probability of occurrence
str(sim_sp1_pa) #extracting raster of the environmental suitability
sim_sp1_pa$suitab.raster
sim_sp1_pa$details$variables
sim_sp1_pa$details$parameters

# We an save the virtual species objects for later use
saveRDS(sim_sp1_pa, file = "MyVirtualSpecies.RDS") 
sim_sp1_pa <- readRDS("MyVirtualSpecies.RDS")
