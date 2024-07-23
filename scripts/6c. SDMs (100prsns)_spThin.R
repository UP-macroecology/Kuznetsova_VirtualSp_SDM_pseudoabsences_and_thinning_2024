# Downloading the necessary libaries
library(terra)
library(mecofun)

## Running SDMs for dfs with 100 presences


## Uploading the data 
sim_sp1_pa.df <- read.csv("data/VS.dataframe.csv")
# For the thinned data, run the scripts under number 5.

# We will be testing the model prediction ability on the virtual truth of our species
# First we need to make sure the dataframe has the same column names and is 
# binded with the climate variables
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




## 1. Absences x10 ----

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_10

# Now we take random 0.001% of rows from our virtual truth as our test df
test_i <- sample(seq_len(nrow(sim_sp1_pa_env.df)), size=round(0.001*nrow(sim_sp1_pa_env.df))) 
sp_test <- sim_sp1_pa_env.df[test_i,]

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_10$occ==1, 1, 
                  sum(sp_thinned_100_10$occ==1) / sum(sp_thinned_100_10$occ==0))

# Check for multicollinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_10[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_10[,c(5:23)], 
                    y=sp_thinned_100_10$occ, 
                    threshold=0.7, weights=weights)

# Inspect weakly correlated variables
var_sel$pred_sel

#Output: [1] "bio10" "bio2"  "bio4"  "bio16" "bio14" "bio11" "bio8"  "bio9"  "bio15" "bio3" 

print(cor_mat)

# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.38
my_preds <- c('bio10','bio14')





## GLM 

# Fit GLM
vs100_10_glm <- glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2),
                   family='binomial', data=sp_train, weights = weights)

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_10_glm, predictors = sp_train[,my_preds], 
                 main='GLM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_10_glm <- evalSDM(sp_test$occ, 
                     predict(vs100_10_glm, sp_test[,my_preds], type='response') ))


# For the response surface, we first prepare the 3D-grid with environmental gradient and predictions
xyz <- expand.grid(
  seq(min(sp_train[,my_preds[1]]),max(sp_train[,my_preds[1]]),length=30),
  seq(min(sp_train[,my_preds[2]]),max(sp_train[,my_preds[2]]),length=130))
names(xyz) <- my_preds

# Make predictions to gradients:
xyz$z <- predict(vs100_10_glm, xyz, type='response')

# Define colour palette:
cls <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))(100)

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GLM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- data.frame(
  model_name = "glm_100x10",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
)





## GAM 
# Fit GAM with spline smoother
vs100_10_gam <- mgcv::gam(occ ~ s(bio10,k=4) + s(bio14, k=4),
                    family='binomial', data=sp_train)
# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_10_gam, predictors = sp_train[,my_preds], main='GAM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_10_gam <- evalSDM(sp_test$occ, predict(vs100_10_gam, sp_test[,my_preds], 
                                          type='response') ))
print(perf_vs100_10_gam)

# Make predictions to gradients:
xyz$z <- predict(vs100_10_gam, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GAM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "gam_100x10",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))






## 2. Absences x5 ----

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_5

# We use the same data for testing the predictions
summary(sp_test)

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_5$occ==1, 1, 
                  sum(sp_thinned_100_5$occ==1) / sum(sp_thinned_100_5$occ==0))


# Check for multicollinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_5[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_5[,c(5:23)], 
                    y=sp_thinned_100_5$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio10" "bio11" "bio8"  "bio14" "bio16" "bio3"  "bio15" "bio9" 

print(cor_mat)
# We are picking two variables representing temperature and precipitation
# The initial variables 'bio10 & 'bio14' have correlation of -0.43
my_preds <- c('bio10','bio14')






## GLM 
# Fit GLM
vs100_5_glm <- glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2),
                        family='binomial', data=sp_train, weights = weights)

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_5_glm, predictors = sp_train[,my_preds], 
                 main='GLM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_5_glm <- evalSDM(sp_test$occ, 
                             predict(vs100_5_glm, sp_test[,my_preds], type='response') ))

print(perf_vs100_5_glm)


# Make predictions to gradients:
xyz$z <- predict(vs100_5_glm, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GLM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "glm_100x5",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))






## GAM 
# Fit GAM with spline smoother
vs100_5_gam <- mgcv::gam(occ ~ s(bio10,k=4) + s(bio14, k=4),
                         family='binomial', data=sp_train)
# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_5_gam, predictors = sp_train[,my_preds], main='GAM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_5_gam <- evalSDM(sp_test$occ, predict(vs100_5_gam, sp_test[,my_preds], 
                                                  type='response') ))
print(perf_vs100_5_gam)


# Make predictions to gradients:
xyz$z <- predict(vs100_5_gam, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GAM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "gam_100x5",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))






## 3. Absences x3 ----

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_3

# We use the same data for testing the predictions
summary(sp_test)

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_3$occ==1, 1, 
                  sum(sp_thinned_100_3$occ==1) / sum(sp_thinned_100_3$occ==0))


# Check for multicollinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_3[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_3[,c(5:23)], 
                    y=sp_thinned_100_3$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel

#Output: [1] "bio10" "bio4"  "bio11" "bio12" "bio9"  "bio3"  "bio15"

print(cor_mat)

# We are picking two variables representing temperature and precipitation
# The initial variables 'bio10 & 'bio14' have correlation of -0.46 
my_preds <- c('bio10','bio14')







## GLM 

# Fit GLM
vs100_3_glm <- glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2),
                       family='binomial', data=sp_train, weights = weights)

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_3_glm, predictors = sp_train[,my_preds], 
                 main='GLM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_3_glm <- evalSDM(sp_test$occ, 
                            predict(vs100_3_glm, sp_test[,my_preds], type='response') ))

print(perf_vs100_3_glm)


# Make predictions to gradients:
xyz$z <- predict(vs100_3_glm, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GLM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "glm_100x3",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))





## GAM 
# Fit GAM with spline smoother
vs100_3_gam <- mgcv::gam(occ ~ s(bio10,k=4) + s(bio14, k=4),
                        family='binomial', data=sp_train)
# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_3_gam, predictors = sp_train[,my_preds], main='GAM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_3_gam <- evalSDM(sp_test$occ, predict(vs100_3_gam, sp_test[,my_preds], 
                                                 type='response') ))
print(perf_vs100_3_gam)


# Make predictions to gradients:
xyz$z <- predict(vs100_3_gam, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GAM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "gam_100x3",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))







## 4. Absences x1 ----

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_1

# We use the same data for testing the predictions
summary(sp_test)

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_1$occ==1, 1, 
                  sum(sp_thinned_100_1$occ==1) / sum(sp_thinned_100_1$occ==0))




# Check for multicollinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_1[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_1[,c(5:23)], 
                    y=sp_thinned_100_1$occ, 
                    threshold=0.7, weights=weights)

# Inspect weakly correlated variables
var_sel$pred_sel
cor_mat
#Output: [1] "bio5"  "bio4"  "bio14" "bio8"  "bio11" "bio16" "bio9"  "bio15" "bio3" 

# We are picking two variables representing temperature and precipitation
# The initial variables 'bio10 & 'bio14' have correlation of -0.53
my_preds <- c('bio10','bio14')






## GLM 

# Fit GLM
vs100_1_glm <- glm(occ ~ bio10 + I(bio10^2) + bio14 + I(bio14^2),
                       family='binomial', data=sp_train, weights = weights)

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_1_glm, predictors = sp_train[,my_preds], 
                 main='GLM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_1_glm <- evalSDM(sp_test$occ, 
                            predict(vs100_1_glm, sp_test[,my_preds], type='response') ))



# Make predictions to gradients:
xyz$z <- predict(vs100_1_glm, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GLM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "glm_100x1",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))






## GAM 
# Fit GAM with spline smoother
vs100_1_gam <- mgcv::gam(occ ~ s(bio10,k=4) + s(bio14, k=4),
                        family='binomial', data=sp_train)
# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(vs100_1_gam, predictors = sp_train[,my_preds], main='GAM', 
                 ylab='Occurrence probability')

# Performance measures
(perf_vs100_1_gam <- evalSDM(sp_test$occ, predict(vs100_1_gam, sp_test[,my_preds], 
                                                 type='response') ))
print(perf_vs100_1_gam)


# Make predictions to gradients:
xyz$z <- predict(vs100_1_gam, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE),
          zlim = c(0, 1), main='GAM', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "gam_100x1",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))







## 5. Comparing all algorithms

(comp_perf <- rbind(glm_100x10 = perf_vs100_10_glm, gam_100x10 = perf_vs100_10_gam,
                    glm_100x5 = perf_vs100_5_glm, gam_100x5 = perf_vs100_5_gam,
                    glm_100x3 = perf_vs100_3_glm, gam_100x3 = perf_vs100_3_gam,
                    glm_100x1 = perf_vs100_1_glm, gam_100x1 = perf_vs100_1_gam))

#Adding the optimas data

comp_perf$opt_bio10 <- optimas$bio10
comp_perf$opt_bio14 <- optimas$bio14
comp_perf$opt_occ_prob <- optimas$occ_prob

# We add a column containing the names of the algorithm
comp_perf <- data.frame(model_name=row.names(comp_perf),comp_perf)

# Adapt the file path to your folder structure
write.table(comp_perf, file='data/SDM_alg_perf_100x10x5x3x1.txt', row.names=F)
