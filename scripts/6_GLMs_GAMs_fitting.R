# Load necessary libraries
library(mgcv)       # For GAM
library(dplyr)      # For data manipulation
library(doParallel) # For parallel processing
library(foreach)    # For parallel for-loops

# Set seed for reproducibility
set.seed(123)

# Detect the number of cores available and register parallel backend
n_cores <- 60
cl <- makeCluster(n_cores)
registerDoParallel(cl)
cat("Using", n_cores, "cores for parallel processing.\n")

# Load replicated data
replicated_data_checker <- readRDS("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/replicated_data_checker.RDS")
replicated_data_spThin <- readRDS("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/replicated_data_spThin.RDS")

# Define predictors and top 5% threshold
predictors <- c('bio10', 'bio14')
top_percent <- 0.05  # Top 5%

# Function to calculate the optima within the top 5% predicted probabilities
calculate_optima <- function(predictions, data) {
  results <- cbind(data, pred_prob = predictions)
  results <- results[order(-results$pred_prob), ]
  top_5 <- head(results, ceiling(nrow(results) * top_percent))
  mean_bio10 <- mean(top_5$bio10, na.rm = TRUE)
  mean_bio14 <- mean(top_5$bio14, na.rm = TRUE)
  mean_prob <- mean(top_5$pred_prob, na.rm = TRUE)
  return(c(mean_bio10, mean_bio14, mean_prob))
}

# Parallelized function to fit GLM and GAM models, calculate optima, and save models
fit_models_and_save <- function(data_list, method_name, key) {
  cat("Fitting models for:", key, "\n")
  
  for (replicate in names(data_list[[key]])) {
    data <- data_list[[key]][[replicate]]
    
    # Paths for saved models
    glm_model_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/models/glm_", 
                             method_name, "_", key, "_", replicate, ".RDS")
    gam_model_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/models/gam_", 
                             method_name, "_", key, "_", replicate, ".RDS")
    
    # Check if both models already exist
    if (file.exists(glm_model_path) && file.exists(gam_model_path)) {
      cat("Models for", key, "replicate", replicate, "already exist. Skipping.\n")
      next
    }
    
    # Fit GLM
    glm_model <- glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2), 
                     family = 'binomial', data = data)
    saveRDS(glm_model, glm_model_path)
    
    # Fit GAM
    gam_model <- mgcv::gam(occ ~ s(bio10, k = 4) + s(bio14, k = 4), 
                           family = 'binomial', data = data)
    saveRDS(gam_model, gam_model_path)
    
    cat("Models saved for:", key, "replicate", replicate, "\n")
  }
}

# Use foreach to parallelize the function calls for both spThin and checkerboard data
foreach(key = names(replicated_data_spThin), .packages = c("mgcv", "dplyr")) %dopar% {
  fit_models_and_save(replicated_data_spThin, "spThin", key)
}

foreach(key = names(replicated_data_checker), .packages = c("mgcv", "dplyr")) %dopar% {
  fit_models_and_save(replicated_data_checker, "checkerboard", key)
}

# Stop the parallel cluster
stopCluster(cl)
cat("Parallel processing complete.\n")
