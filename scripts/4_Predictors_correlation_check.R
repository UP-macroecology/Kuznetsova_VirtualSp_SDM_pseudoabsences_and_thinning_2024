library(geodata)       
library(terra)         
library(ggplot2)
library(tidyr)

#-------------------
# 4. Checking for correlation between predictors

# Spearman correlations were calculated across 20 datasets representing 
# different combinations of spatial thinning (spThin/checkerboard) and 
# presence-absence ratios. Correlation values ranged from 0.02 to 0.58, 
# all well below the 0.7 threshold, confirming that the two variables
# can be reliably used together in the models.


## Uploading the data
thinned_spThin_data <- readRDS("data/thinned_spThin_data.RDS")
thinned_checker_data <- readRDS("data/thinned_checker_data.RDS")

# Function to calculate Spearman correlation between bio6 and bio15
get_bio6_bio15_corr <- function(df) {
  cor(df$bio6, df$bio15, method = "spearman", use = "complete.obs")
}

# Get the names of the datasets (e.g., "20_pa10", "20_pa5", etc.)
ratios <- names(thinned_spThin_data2)

# Calculate correlations
cor_results_thin <- sapply(thinned_spThin_data2, get_bio6_bio15_corr)
cor_results_check <- sapply(thinned_checker_data2, get_bio6_bio15_corr)

# Combine into one data frame
cor_df <- data.frame(
  Ratio = ratios,
  spThin = cor_results_thin,
  Checkerboard = cor_results_check,
  row.names = NULL
)

# View result
print(cor_df)

# Reshape for easier plotting
cor_df_long <- pivot_longer(cor_df, cols = c(spThin, Checkerboard),
                            names_to = "ThinningMethod", values_to = "SpearmanCorrelation")

# Plot
par(mfrow = c(1, 1), oma = c(0, 0, 4, 0))  # 3x2 layout for plots

ggplot(cor_df_long, aes(x = Ratio, y = SpearmanCorrelation, color = ThinningMethod, group = ThinningMethod)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Spearman correlation between bio6 and bio15 \n per occ/abs ratio and thinning method",
       y = "Spearman Correlation", x = "PA Ratio")

# Save plot
dev.copy(png, filename = "plots/4_corr_check_bio6_bio15.png", width = 1600, height = 1200, res = 200)
dev.off()
