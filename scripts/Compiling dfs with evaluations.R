
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

RF_eval_20_spThin_chb <- read.table('data/RF_perf_20x10x5x3x1_spThin_chb.txt',
                                      header=TRUE, sep=" ")

RF_eval_50_spThin_chb <- read.table('data/RF_perf_50x10x5x3x1_spThin_chb.txt',
                                    header=TRUE, sep=" ")

RF_eval_100_spThin_chb <- read.table('data/RF_perf_100x10x5x3x1_spThin_chb.txt',
                                    header=TRUE, sep=" ")








# Binding the dataframes together

SDMs_evals <- rbind(GLMs_GAMs_eval_20_spThin, GLMs_GAMs_eval_50_spThin,
                         GLMs_GAMs_eval_100_spThin, GLMs_GAMs_eval_500_spThin, 
                         GLMs_GAMs_eval_1000_spThin, GLMs_GAMs_eval_20_chb, 
                         GLMs_GAMs_eval_50_chb, GLMs_GAMs_eval_100_chb, 
                         GLMs_GAMs_eval_500_chb, GLMs_GAMs_eval_1000_chb, 
                         RF_eval_20_spThin_chb, RF_eval_50_spThin_chb,
                    RF_eval_100_spThin_chb)

# Adding a TRUE-FALSE marker for the predictions made within/outside of the 
# natural environmental range

# Check if bio10 values are within the range 14-22
SDMs_evals$bio10_in_range <- SDMs_evals$opt_bio10 >= 14 & SDMs_evals$opt_bio10 <= 22

# Check if bio14 values are within the range 35-115
SDMs_evals$bio14_in_range <- SDMs_evals$opt_bio14 >= 35 & SDMs_evals$opt_bio14 <= 115

# Display the first few rows of the dataframe to verify the changes
head(SDMs_evals)

# Saving the final dataframe
write.csv(SDMs_evals, "data/SDMs_evals(spThin30,checkb,nostep).csv", row.names = FALSE)

