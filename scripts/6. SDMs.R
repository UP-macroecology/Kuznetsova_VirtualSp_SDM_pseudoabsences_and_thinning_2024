# Downloading the necessary libaries
library(terra)
library(mecofun)

## Uploading the data (absences x10) ----
bg <- terra::rast('data/bg_australia_mask.grd')
load('data/VS_PresAbsx10_thinned.RData')
sp_dat <- sp_thinned

# Check for multicollinearity between our environmental variables
cor_mat <- cor(sp_dat[,-c(1:5)], method='spearman')
var_sel <- select07(X=sp_dat[,-c(1:5)], 
                    y=sp_dat$occ, 
                    threshold=0.7)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio10" "bio19" "bio2"  "bio11" "bio14" "bio16" "bio4"  "bio3"  "bio15" "bio9" 

# Let's only use the two most important predictors for now that we picked initially
my_preds <- c('bio10','bio14')



### 1. Splitting data into training and testing ------

# First, we randomly select 70% of the rows that will be used as training data
train_i <- sample(seq_len(nrow(sp_dat)), size=round(0.7*nrow(sp_dat)))

# Then, we can subset the training and testing data
sp_train <- sp_dat[train_i,]
sp_test <- sp_dat[-train_i,]

# We store the split information for later:
write(train_i, file='data/indices_traindata10.txt')



### 2. GLM (absences x10) ----------------------------
# Calculate same weights for presences and absences for regression based algorithms
weights <- ifelse(species_occ_clim_native$occ==1, 1, 
                  sum(species_occ_clim_native$occ==1) / sum(species_occ_clim_native$occ==0))
# sum of all pseudo abs has the same weight as the sum of presences

# Fit GLM
vs10_glm <- step(glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2),
                   family='binomial', data=sp_train, weights = weights))

# Plot partial response curves using the mecofun package:
library(mecofun)
par(mfrow=c(1,2)) 
partial_response(vs10_glm, predictors = sp_train[,my_preds], 
                 main='GLM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs10_glm <- evalSDM(sp_test$occ, 
                     predict(vs10_glm, sp_test[,my_preds], type='response') ))

print(perf_vs10_glm)


### 3. GAM (absences x10) -------
# Fit GAM with spline smoother
vs10_gam <- mgcv::gam(occ ~ s(bio10,k=4) + s(bio14, k=4),
                    family='binomial', data=sp_train)
# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs10_gam, predictors = sp_train[,my_preds], main='GAM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs10_gam <- evalSDM(sp_test$occ, predict(vs10_gam, sp_test[,my_preds], 
                                          type='response') ))
print(perf_vs10_gam)
