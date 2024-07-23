
#Downloading the libraries
library(randomForest)


## Uploading the data if not uploaded earlier
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

## a. Based on the data thinned with spThin

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_10

# Now we take random 0.001% of rows from our virtual truth as our test df
test_i <- sample(seq_len(nrow(sim_sp1_pa_env.df)), size=round(0.001*nrow(sim_sp1_pa_env.df))) 
sp_test <- sim_sp1_pa_env.df[test_i,]

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_10$occ==1, 1, 
                  sum(sp_thinned_100_10$occ==1) / sum(sp_thinned_100_10$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_10[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_10[,c(5:23)], 
                    y=sp_thinned_100_10$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio10" "bio2"  "bio4"  "bio16" "bio14" "bio11" "bio8"  "bio9"  "bio15" "bio3" 

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.38
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_10 <- randomForest( x=sp_train[,my_preds], y=sp_train$occ, 
                      ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_10,type=1)
varImpPlot(m_rf_100_10)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the decision trees.

# Look at single trees:
head(getTree(m_rf_100_10,1,T))


# For the response surface, we first prepare the 3D-grid with environmental gradient and predictions
xyz <- expand.grid(
  seq(min(sp_train[,my_preds[1]]),max(sp_train[,my_preds[1]]),length=30),
  seq(min(sp_train[,my_preds[2]]),max(sp_train[,my_preds[2]]),length=130))
names(xyz) <- my_preds

# Make predictions to gradients:
xyz$z <- predict(m_rf_100_10, xyz, type='response')

# Define colour palette:
cls <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))(100)

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_10, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- data.frame(
  model_name = "rf_100x10",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
)


# Performance measures of RF
(perf_rf_100_10 <- evalSDM(sp_test$occ, predict(m_rf_100_10, 
                                         sp_test[,my_preds],  
                                         type='response') ))




## b. Based on the data thinned with the checkerboard method

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thin_checker_100_10

# We are using the same test data as earlier

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thin_checker_100_10$occ==1, 1, 
                  sum(sp_thin_checker_100_10$occ==1) / sum(sp_thin_checker_100_10$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thin_checker_100_10[,(7:25)], method='spearman')
var_sel <- select07(X=sp_thin_checker_100_10[,c(7:25)], 
                    y=sp_thin_checker_100_10$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio10" "bio2"  "bio4"  "bio13" "bio11" "bio8"  "bio9"  "bio15" "bio3" 

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.44
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_10_chb <- randomForest( x=sp_train[,my_preds], y=sp_train$occ, 
                            ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_10_chb,type=1)
varImpPlot(m_rf_100_10_chb)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the decision trees.

# Look at single trees:
head(getTree(m_rf_100_10_chb,1,T))



# Make predictions to gradients:
xyz$z <- predict(m_rf_100_10_chb, xyz, type='response')

# Now, we plot the response surface:
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_10_chb, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x10_chb",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))

# Performance measures of RF
(perf_rf_100_10_chb <- evalSDM(sp_test$occ, predict(m_rf_100_10_chb, 
                                               sp_test[,my_preds],  
                                               type='response') ))







## 2. Absences x5 ----

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_5

#We use the same data for the testing

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_5$occ==1, 1, 
                  sum(sp_thinned_100_5$occ==1) / sum(sp_thinned_100_5$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_5[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_5[,c(5:23)], 
                    y=sp_thinned_100_5$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel

#Output: [1] "bio10" "bio11" "bio8"  "bio14" "bio16" "bio3"  "bio15" "bio9" 

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.43
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_5 <- randomForest( x=sp_train[,my_preds], y=sp_train$occ, 
                            ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_5,type=1)
varImpPlot(m_rf_100_5)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the decision trees.


# Now, we plot the response surface:
xyz$z <- predict(m_rf_100_5, xyz, type='response')

wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_5, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x5",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))

# Performance measures of RF
(perf_rf_100_5 <- evalSDM(sp_test$occ, predict(m_rf_100_5, 
                                               sp_test[,my_preds],  
                                               type='response') ))





## b. Based on the data thinned with the checkerboard method

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thin_checker_100_5

# We are using the same test data as earlier

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thin_checker_100_5$occ==1, 1, 
                  sum(sp_thin_checker_100_5$occ==1) / sum(sp_thin_checker_100_5$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thin_checker_100_5[,(7:25)], method='spearman')
var_sel <- select07(X=sp_thin_checker_100_5[,c(7:25)], 
                    y=sp_thin_checker_100_5$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio10" "bio11" "bio4"  "bio12" "bio8"  "bio9"  "bio15" "bio3" 

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.48
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_5_chb <- randomForest(x=sp_train[,my_preds], y=sp_train$occ, 
                                ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_5_chb,type=1)
varImpPlot(m_rf_100_5_chb)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the 
# decision trees.

# Look at single trees:
head(getTree(m_rf_100_5_chb,1,T))



# Make predictions to gradients:
xyz$z <- predict(m_rf_100_5_chb, xyz, type='response')

# Now, we plot the response surface:
xyz$z <- predict(m_rf_100_5_chb, xyz, type='response')
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_5_chb, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x5_chb",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))

# Performance measures of RF
(perf_rf_100_5_chb <- evalSDM(sp_test$occ, predict(m_rf_100_5_chb, 
                                                   sp_test[,my_preds],  
                                                   type='response') ))






## 2. Absences x3 ----

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_3

#We use the same data for the testing

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_3$occ==1, 1, 
                  sum(sp_thinned_100_3$occ==1) / sum(sp_thinned_100_3$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_3[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_3[,c(5:23)], 
                    y=sp_thinned_100_3$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel

#Output: [1] "bio10" "bio4"  "bio11" "bio12" "bio9"  "bio3"  "bio15"

cor_mat 

# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.56
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_3 <- randomForest( x=sp_train[,my_preds], y=sp_train$occ, 
                           ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_3,type=1)
varImpPlot(m_rf_100_3)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the decision trees.


# Now, we plot the response surface:
xyz$z <- predict(m_rf_100_3, xyz, type='response')

wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_3, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x3",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))

# Performance measures of RF
(perf_rf_100_3 <- evalSDM(sp_test$occ, predict(m_rf_100_3, 
                                              sp_test[,my_preds],  
                                              type='response') ))




## b. Based on the data thinned with the checkerboard method

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thin_checker_100_3

# We are using the same test data as earlier

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thin_checker_100_3$occ==1, 1, 
                  sum(sp_thin_checker_100_3$occ==1) / sum(sp_thin_checker_100_3$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thin_checker_100_3[,(7:25)], method='spearman')
var_sel <- select07(X=sp_thin_checker_100_3[,c(7:25)], 
                    y=sp_thin_checker_100_3$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio5"  "bio4"  "bio14" "bio8"  "bio11" "bio9"  "bio15" "bio3" 

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.52
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_3_chb <- randomForest(x=sp_train[,my_preds], y=sp_train$occ, 
                              ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_3_chb,type=1)
varImpPlot(m_rf_100_3_chb)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the 
# decision trees.

# Look at single trees:
head(getTree(m_rf_100_3_chb,1,T))



# Make predictions to gradients:
xyz$z <- predict(m_rf_100_3_chb, xyz, type='response')

# Now, we plot the response surface:
xyz$z <- predict(m_rf_100_3_chb, xyz, type='response')
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_3_chb, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x3_chb",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))

# Performance measures of RF
(perf_rf_100_3_chb <- evalSDM(sp_test$occ, predict(m_rf_100_3_chb, 
                                                  sp_test[,my_preds],  
                                                  type='response') ))






## 2. Absences x1 ----

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thinned_100_1

#We use the same data for the testing

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thinned_100_1$occ==1, 1, 
                  sum(sp_thinned_100_1$occ==1) / sum(sp_thinned_100_1$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thinned_100_1[,(5:23)], method='spearman')
var_sel <- select07(X=sp_thinned_100_1[,c(5:23)], 
                    y=sp_thinned_100_1$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel

#Output: [1] "bio5"  "bio4"  "bio14" "bio8"  "bio11" "bio16" "bio9"  "bio15" "bio3" 

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.53
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_1 <- randomForest( x=sp_train[,my_preds], y=sp_train$occ, 
                           ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_1,type=1)
varImpPlot(m_rf_100_1)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the decision trees.


# Now, we plot the response surface:
xyz$z <- predict(m_rf_100_1, xyz, type='response')

wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_1, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x1",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))

# Performance measures of RF
(perf_rf_100_1 <- evalSDM(sp_test$occ, predict(m_rf_100_1, 
                                              sp_test[,my_preds],  
                                              type='response') ))




## b. Based on the data thinned with the checkerboard method

## Splitting data into training and testing 

# Our training data is the dataframe with the samples itself
sp_train <- sp_thin_checker_100_1

# We are using the same test data as earlier

# Calculate same weights for presences and absences for regression based algorithms
# sum of all pseudo abs has the same weight as the sum of presences
weights <- ifelse(sp_thin_checker_100_1$occ==1, 1, 
                  sum(sp_thin_checker_100_1$occ==1) / sum(sp_thin_checker_100_1$occ==0))

# Check for multi-collinearity between our environmental variables
cor_mat <- cor(sp_thin_checker_100_1[,(7:25)], method='spearman')
var_sel <- select07(X=sp_thin_checker_100_1[,c(7:25)], 
                    y=sp_thin_checker_100_1$occ, 
                    threshold=0.7, weights = weights)

# Inspect weakly correlated variables
var_sel$pred_sel
#Output: [1] "bio5"  "bio4"  "bio14" "bio8"  "bio6"  "bio13" "bio9"  "bio3"  "bio15"

cor_mat 
# We are picking two variables representing temperature and precipitation
# initial variables 'bio10 & 'bio14' have correlation of -0.56
my_preds <- c('bio10','bio14')




## Fit RF 
m_rf_100_1_chb <- randomForest(x=sp_train[,my_preds], y=sp_train$occ, 
                              ntree=1000, importance =T)

# Variable importance:
importance(m_rf_100_1_chb,type=1)
varImpPlot(m_rf_100_1_chb)
# Interpretation: bio10 is crucial for the model. While bio14 might not drastically 
# impact the overall accuracy of the model when excluded, it still plays a 
# significant role in improving node purity and making effective splits in the 
# decision trees.



# Make predictions to gradients:
xyz$z <- predict(m_rf_100_1_chb, xyz, type='response')

# Now, we plot the response surface:
xyz$z <- predict(m_rf_100_1_chb, xyz, type='response')
wireframe(z ~ bio10 + bio14, data = xyz, zlab = list("Occurrence prob.", rot=90), 
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
          zlim = c(0, 1), main='Random Forest', xlab='bio10', ylab='bio14', 
          screen=list(z = -120, x = -70, y = 3))

# Plot partial response curves:
par(mfrow=c(1,2)) 
partial_response(m_rf_100_1_chb, predictors = sp_train[,my_preds], 
                 main='Random Forest', 
                 ylab='Occurrence probability')



# Identify the optimal parameters where the predicted probability is highest
# Create a dataframe for optimas
optimas <- rbind(optimas, list(
  model_name = "rf_100x1_chb",
  bio10 = xyz[which.max(xyz$z), my_preds[1]],
  bio14 = xyz[which.max(xyz$z), my_preds[2]],
  occ_prob = max(xyz$z)
))


# Performance measures of RF
(perf_rf_100_1_chb <- evalSDM(sp_test$occ, predict(m_rf_100_1_chb, 
                                                  sp_test[,my_preds],  
                                                  type='response') ))







## 5. Comparing all algorithms

(comp_perf <- rbind(rf_100x10 = perf_rf_100_10, rf_100x10_chb = perf_rf_100_10_chb,
                    rf_100x5 = perf_rf_100_5, rf_100x5_chb = perf_rf_100_5_chb,
                    rf_100x3 = perf_rf_100_3, rf_100x3_chb = perf_rf_100_3_chb,
                    rf_100x1 = perf_rf_100_1, rf_100x1_chb = perf_rf_100_1_chb))

#Adding the optimas data

comp_perf$opt_bio10 <- optimas$bio10
comp_perf$opt_bio14 <- optimas$bio14
comp_perf$opt_occ_prob <- optimas$occ_prob

# We add a column containing the names of the algorithm
comp_perf <- data.frame(model_name=row.names(comp_perf),comp_perf)

# Adapt the file path to your folder structure
write.table(comp_perf, file='data/RF_perf_100x10x5x3x1_spThin_chb.txt', row.names=F)