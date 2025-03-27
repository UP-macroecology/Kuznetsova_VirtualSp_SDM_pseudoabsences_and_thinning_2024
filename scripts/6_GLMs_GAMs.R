# Load necessary libraries
library(mgcv)       # For GAM
library(dplyr)      # For data manipulation
library(doParallel) # For parallel processing
library(foreach)    # For parallel for-loops

# Set seed for reproducibility
set.seed(123)

# Detect the number of cores available and register parallel backend
n_cores <- 40  # Use up to 8 cores or max available - 1
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
  
  optima_glm <- list()
  optima_gam <- list()
  
  glm_replicate_df <- data.frame(replicate = character(), mean_bio10 = numeric(), mean_bio14 = numeric(), mean_prob = numeric())
  gam_replicate_df <- data.frame(replicate = character(), mean_bio10 = numeric(), mean_bio14 = numeric(), mean_prob = numeric())
  
  for (replicate in names(data_list[[key]])) {
    data <- data_list[[key]][[replicate]]
    
    # Fit GLM
    glm_model <- glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2), 
                     family = 'binomial', data = data)
    glm_predictions <- predict(glm_model, newdata = data, type = "response")
    glm_optima <- calculate_optima(glm_predictions, data)
    
    # Save the GLM model
    glm_model_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/models/glm_", 
                             method_name, "_", key, "_", replicate, ".RDS")
    saveRDS(glm_model, glm_model_path)
    
    # Store GLM optima in a dataframe
    glm_replicate_df <- rbind(glm_replicate_df, 
                              data.frame(replicate = replicate, mean_bio10 = glm_optima[1], mean_bio14 = glm_optima[2], mean_prob = glm_optima[3]))
    
    # Fit GAM
    gam_model <- mgcv::gam(occ ~ s(bio10, k = 4) + s(bio14, k = 4), 
                           family = 'binomial', data = data)
    gam_predictions <- predict(gam_model, newdata = data, type = "response")
    gam_optima <- calculate_optima(gam_predictions, data)
    
    # Save the GAM model
    gam_model_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/models/gam_", 
                             method_name, "_", key, "_", replicate, ".RDS")
    saveRDS(gam_model, gam_model_path)
    
    # Store GAM optima in a dataframe
    gam_replicate_df <- rbind(gam_replicate_df, 
                              data.frame(replicate = replicate, mean_bio10 = gam_optima[1], mean_bio14 = gam_optima[2], mean_prob = gam_optima[3]))
  }
  
  # Calculate the mean optima and mean predicted probability across replicates
  mean_optima_glm <- colMeans(glm_replicate_df[, -1], na.rm = TRUE)
  mean_optima_gam <- colMeans(gam_replicate_df[, -1], na.rm = TRUE)
  
  # Save the replicate optima dataframes
  glm_replicate_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/optima/glm_replicates_", method_name, "_", key, ".csv")
  gam_replicate_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/optima/gam_replicates_", method_name, "_", key, ".csv")
  write.csv(glm_replicate_df, glm_replicate_path, row.names = FALSE)
  write.csv(gam_replicate_df, gam_replicate_path, row.names = FALSE)
  
  # Save the mean optima as a separate CSV
  mean_optima_path <- paste0("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/optima/mean_optima_", method_name, "_", key, ".csv")
  mean_optima_df <- data.frame(model = c("GLM", "GAM"), mean_bio10 = c(mean_optima_glm[1], mean_optima_gam[1]),
                               mean_bio14 = c(mean_optima_glm[2], mean_optima_gam[2]), mean_prob = c(mean_optima_glm[3], mean_optima_gam[3]))
  write.csv(mean_optima_df, mean_optima_path, row.names = FALSE)
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
