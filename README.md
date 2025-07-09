# Testing the optimal proportion of pseudo-absences / presences in combination with spatial thinning methods

## Rationale 

Species distribution models (SDMs) rely on the availability of both presence and absence data to accurately predict species’ environmental niches. In the absence of reliable true absences, pseudo-absences are commonly used. Barbet-Massin et al. (2012) provided foundational guidelines for pseudo-absence generation, showing that model performance depends critically on how, where, and how many pseudo-absences are selected. Their results recommend using a large number of pseudo-absences (e.g., 10,000) with equal weighting relative to presences for regression techniques, and a 1:1 presence–pseudo-absence ratio for classification methods, alongside random selection strategies for pseudo-absences.

While these recommendations significantly advanced SDM methodology, they were based on specific modeling approaches and virtual species configurations. A key knowledge gap remains regarding the generalizability of these findings across broader pseudo-absence strategies. Notably, while Barbet-Massin et al. recommend a large number of pseudo-absences for regression techniques and a 1:1 ratio for classification models, the paper does not explicitly address how different presence–absence ratios interact with varying numbers of initial presence points. To address this, we test multiple initial presence sample sizes (20, 50, 100, 500, 1000) and apply varying pseudo-absence ratios (x1, x3, x5, x10) to systematically evaluate their impact across different modeling approaches.

Although Barbet-Massin et al. emphasize that bias in presence data can degrade model accuracy, we do not explicitly test for sampling bias in presences and instead assume our presence data is unbiased. Nonetheless, we aim to assess how different spatial thinning methods applied to pseudo-absences influence model outcomes. To do this, we first generate pseudo-absences randomly within a fixed buffer of 200 km around the presence points to simulate climatic limits of the virtual species. We then apply two thinning strategies—checkerboard and spThin() with a fixed minimum distance of a diagonal of one raster cell or 30 km accordingly—to both the pseudo-absences and the presences. This allows us to explore the potential effects of spatial filtering on model robustness and predictive performance.

We test a range of modeling algorithms (GAMs, GLMs, Random Forests, Maxent), run multiple times and averaged, then validated using several performance metrics: AUC, TSS, Kappa, Sensitivity, Specificity, Pearson Correlation Coefficient (PCC), Deviance explained (D2), and threshold-dependent statistics. Additionally, we calculate and compare the mean niche optima  derived from the models against the initial environmental parameters used to define the virtual species.


## General workflow

### 1. Downloading and Preparing Environmental Data
To begin, I downloaded high-resolution (~1 km) bioclimatic variables from the WorldClim v2.1 database using the `worldclim_global` function. I focused on Australia as the area of interest and defined its extent manually, excluding Tasmania to simplify the modeling domain. A raster mask was created based on country boundaries and used to crop the climate data, spatially constraining it to terrestrial areas only. (Script [1_Downloading_env_data](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/scripts/1_Downloading_env_data.R).)

### 2. Creating a Virtual Species
Next, a virtual species was created using the `virtualspecies` package. I defined the species’ niche through Gaussian response curves for the mean temperature of the coldest month (bio6) and precipitation seasonality (bio15), representing its ecological preferences and tolerances. These functions were passed to `generateSpFromFun`, which produced a continuous environmental suitability map. The suitability values were then converted into probabilities of occurrence using a logistic transformation. Finally, a presence–absence raster was generated and saved, along with a dataframe linking geographic coordinates, occurrence values, and environmental predictors. (Script [2_Defining_VS.R](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/scripts/2_Defining_VS.R).)

### 3. Sampling Occurrences and Background Data
To simulate sampling as it might happen in ecological surveys, presence-only points were randomly drawn at different sample sizes (20, 50, 100, 500, and 1000 records) using the `sampleOccurrences()` function. Around each set of sampled presence points, a 200 km buffer / background area was created. Within the resulting exclusion buffer, pseudo-absences were randomly sampled at different ratios relative to the number of presences (1×, 3×, 5×, 10×). Each combined dataset of presence and pseudo-absence points was then enriched with the corresponding environmental values extracted from the climate layers. These structured datasets were saved for use in later modeling and spatial thinning steps. (Script [3_Sampling_occ_and_pseudo_absences.R](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/scripts/3_Sampling_occ_and_pseudo_absences.R).)

### 4. Spatial Thinning
To reduce spatial autocorrelation in the presence–absence datasets, I applied two alternative thinning strategies. The first approach used the `spThin` package to enforce a minimum distance of 30 km between points, thinning both presence and pseudo-absence locations. The second approach used a checkerboard method, a grid-based thinning approach that overlays a checkerboard-style raster on the buffered region and keeps only one point per raster cell.

These two methods were applied independently across all combinations of sample sizes and presence–absence ratios, resulting in two distinct collections of thinned datasets: one produced with spThin(), and another with the checkerboard filter. Both collections were saved separately for downstream use. In the final step, I visualized and compared the spatial distribution of points under both thinning strategies to assess how each method altered the spatial structure of the data (see [4_thinned_data_comparison.png](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/plots/4_thinned_data_comparison.png)). (Script [4_Spatial_thinning.R](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/scripts/4_Spatial_thinning.R).)
    
## 5. Checking Correlation Between Predictors
To ensure that the chosen environmental predictors (bio6 and bio15) were not collinear, I calculated Spearman correlation coefficients across all 20 datasets resulting from the thinning step (combinations of spThin vs. checkerboard and all presence–absence ratios). 
The correlation values ranged from 0.02 to 0.58, consistently below the 0.7 threshold ([see 5_corr_check_bio6_bio15.png](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/plots/5_corr_check_bio6_bio15.png)). This confirmed that both predictors could be used together in downstream species distribution models. (Script [5_Predictors_correlation_check.R](https://github.com/UP-macroecology/Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024/blob/main/scripts/5_Predictors_correlation_check.R).)

6. **Running SDMs for different DFs and presence/absence proportions:**
    - Load species occurrence data and corresponding thinned data for different presence/absence proportions (x20, x5, x3, x1).
    - For each dataset:
        - Split data into training and testing sets.
        - Calculate weights for GLM models.
        - Fit GLM and GAM models.
        - Evaluate model performance metrics.
          - AUC (Area Under the Curve)
          - TSS (True Skill Statistic)
          - Kappa
          - Sensitivity (Sens)
          - Specificity (Spec)
          - Pearson Correlation Coefficient (PCC)
          - Deviance explained (D2)
          - Threshold-dependent statistics (thresh).
    - Compare algorithms using performance measures across all datasets and proportions.
