
## Putting together all the glms and gams results -----

# First uploading all the separate data frames

GLMs_GAMs_eval_20 <- read.table('data/SDM_alg_perf_20x10x5x3x1.txt',
                 header=TRUE, sep=" ")
GLMs_GAMs_eval_50 <- read.table('data/SDM_alg_perf_50x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_100 <- read.table('data/SDM_alg_perf_100x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_500 <- read.table('data/SDM_alg_perf_500x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_1000 <- read.table('data/SDM_alg_perf_1000x10x5x3x1.txt',
                                header=TRUE, sep=" ")

# Binding the dataframes together

GLMs_GAMs_evals <- rbind(GLMs_GAMs_eval_20, GLMs_GAMs_eval_50,
                         GLMs_GAMs_eval_100, GLMs_GAMs_eval_500, 
                         GLMs_GAMs_eval_1000)

# Adding a TRUE-FALSE marker for the predictions made within/outside of the 
# natural environmental range

# Check if bio10 values are within the range 14-22
GLMs_GAMs_evals$bio10_in_range <- GLMs_GAMs_evals$opt_bio10 >= 14 & GLMs_GAMs_evals$opt_bio10 <= 22

# Check if bio14 values are within the range 35-115
GLMs_GAMs_evals$bio14_in_range <- GLMs_GAMs_evals$opt_bio14 >= 35 & GLMs_GAMs_evals$opt_bio14 <= 115

# Display the first few rows of the dataframe to verify the changes
head(GLMs_GAMs_evals)

# Saving the final dataframe
write.csv(GLMs_GAMs_evals, "data/GLMs_GAMs_evals(spThin30,nostep).csv", row.names = FALSE)

