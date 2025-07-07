library(mecofun)

sim_sp1_pa_env.df <- read.csv("/import/ecoc9/data-zurell/kuznetsova/pseudoabsences/data/sim_sp1_pa_env_df.csv")

predictors <- names(sim_sp1_pa_env.df[, 5:23])

# Define equal weights for presences and background data
weights <- ifelse(sim_sp1_pa_env.df$occ == 1, 1, 
                  sum(sim_sp1_pa_env.df$occ == 1) / sum(sim_sp1_pa_env.df$occ == 0))

# Run select07_cv function
var_sel <- select07_cv(X = sim_sp1_pa_env.df[,predictors], 
                       y = sim_sp1_pa_env.df$occ, 
                       threshold = 0.7,
                       weights = weights)


# Extract most important and weakly correlated predictors
my_preds <- var_sel$pred_sel

print(my_preds)
