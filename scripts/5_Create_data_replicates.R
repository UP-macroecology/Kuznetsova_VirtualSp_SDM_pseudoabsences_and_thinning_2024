## WITH MODEL REPLICATIONS = NUMBER OF PRESENCES -------

# The goal is to ensure robust estimation of species' niche optima by generating 
# multiple replicates for each combination of presence/pseudo-absence ratio and thinning method.

# 1. Generate multiple replicates using bootstrapping:
#    - Each replicate is created by randomly sampling the dataset with replacement.
#    - Some observations may appear multiple times, while others may be omitted.
#    - This reduces dependence on individual data points and captures model variability.

# 2. Fit multiple GLMs and GAMs for each replicate:
#    - GLMs model linear and quadratic relationships between predictors and species presence.
#    - GAMs allow for non-linear relationships through smoothing splines.

# 3. Extract optima from the models:
#    - Optima are identified based on predictor values corresponding to 
#      the highest predicted probabilities of occurrence.

# 4. Compute the mean optima across all replicates:
#    - This helps generalize model predictions and minimize the influence of 
#      outliers in any single replicate.

# 5. Select the top 5% highest predicted probabilities and extract optima:
#    - By focusing on the top 5%, we refine the final estimates to represent 
#      the most probable suitable conditions for the species.

set.seed(123)

thinned_spThin_data <- readRDS("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/thinned_spThin_data.RDS")
thinned_checker_data <- readRDS("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/thinned_checker_data.RDS")
output_repl_data_spThin <- "/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/replicated_data_spThin.RDS"
output_repl_data_checker <- "/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/replicated_data_checker.RDS"

generate_replicates <- function(data, n_replicates) {
  replicates <- list()
  
  for (i in 1:n_replicates) {
    # Resample pseudo-absences (random selection)
    sampled_data <- data[sample(nrow(data), size = nrow(data), replace = TRUE), ]
    replicates[[paste0("rep", i)]] <- sampled_data
  }
  
  return(replicates)
}

# Generate replicates for all spThin datasets
#replicated_data_spThin <- list()
#for (key in names(thinned_spThin_data)) {
#  n_replicates <- nrow(thinned_spThin_data[[key]])  # Use the number of presences
#  replicated_data_spThin[[key]] <- generate_replicates(thinned_spThin_data[[key]], n_replicates)
#}

#saveRDS(replicated_data_spThin, output_repl_data_spThin)

# Generate replicates for all spThin datasets
replicated_data_checker <- list()
for (key in names(thinned_checker_data)) {
  n_replicates <- sum(thinned_checker_data[["20_pa10"]]$occ == 1)  # Use the number of presences
  replicated_data_checker[[key]] <- generate_replicates(thinned_checker_data[[key]], n_replicates)
}


saveRDS(replicated_data_checker, output_repl_data_checker)
     