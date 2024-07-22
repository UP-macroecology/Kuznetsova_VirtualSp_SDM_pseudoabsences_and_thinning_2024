
## Putting together all the glms and gams results -----

# First uploading all the separate data frames

GLMs_GAMs_eval_20_spThin <- read.table('data/SDM_alg_perf_20x10x5x3x1.txt',
                 header=TRUE, sep=" ")
GLMs_GAMs_eval_50_spThin <- read.table('data/SDM_alg_perf_50x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_100_spThin <- read.table('data/SDM_alg_perf_100x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_500_spThin <- read.table('data/SDM_alg_perf_500x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_1000_spThin <- read.table('data/SDM_alg_perf_1000x10x5x3x1.txt',
                                header=TRUE, sep=" ")
GLMs_GAMs_eval_20_chb <- read.table('data/SDM_alg_perf_20x10x5x3x1_chb.txt',
                                       header=TRUE, sep=" ")
GLMs_GAMs_eval_50_chb <- read.table('data/SDM_alg_perf_50x10x5x3x1_chb.txt',
                                       header=TRUE, sep=" ")
GLMs_GAMs_eval_100_chb <- read.table('data/SDM_alg_perf_100x10x5x3x1_chb.txt',
                                        header=TRUE, sep=" ")
GLMs_GAMs_eval_500_chb <- read.table('data/SDM_alg_perf_500x10x5x3x1_chb.txt',
                                        header=TRUE, sep=" ")
GLMs_GAMs_eval_1000_chb <- read.table('data/SDM_alg_perf_1000x10x5x3x1_chb.txt',
                                         header=TRUE, sep=" ")
# Binding the dataframes together

GLMs_GAMs_evals <- rbind(GLMs_GAMs_eval_20_spThin, GLMs_GAMs_eval_50_spThin,
                         GLMs_GAMs_eval_100_spThin, GLMs_GAMs_eval_500_spThin, 
                         GLMs_GAMs_eval_1000_spThin, GLMs_GAMs_eval_20_chb, 
                         GLMs_GAMs_eval_50_chb, GLMs_GAMs_eval_100_chb, 
                         GLMs_GAMs_eval_500_chb, GLMs_GAMs_eval_1000_chb)

# Adding a TRUE-FALSE marker for the predictions made within/outside of the 
# natural environmental range

# Check if bio10 values are within the range 14-22
GLMs_GAMs_evals$bio10_in_range <- GLMs_GAMs_evals$opt_bio10 >= 14 & GLMs_GAMs_evals$opt_bio10 <= 22

# Check if bio14 values are within the range 35-115
GLMs_GAMs_evals$bio14_in_range <- GLMs_GAMs_evals$opt_bio14 >= 35 & GLMs_GAMs_evals$opt_bio14 <= 115

# Display the first few rows of the dataframe to verify the changes
head(GLMs_GAMs_evals)

# Saving the final dataframe
write.csv(GLMs_GAMs_evals, "data/GLMs_GAMs_evals(spThin30,checkb,nostep).csv", row.names = FALSE)

