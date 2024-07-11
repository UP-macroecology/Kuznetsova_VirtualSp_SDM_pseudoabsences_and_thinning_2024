# Testing the optimal proportion of pseudo-absences / presences in combination with spatial thinning methods

## General workflow

1. **Download environmental data & Crop it to the area of interest:**
    - Use `worldclim_global` to download global climate variables with a resolution of 30 seconds (~1 km).
    - Define the extent for the wished area using coordinates.
    - Crop global climate data to the wished area.
2. **Create virtual species (VS):**
    - Define species' response to environmental variables using `formatFunctions`.
    - Generate virtual species niche and plot spatial distribution of species’ environmental suitability using `generateSpFromFun`.
    - Use `convertToPA` to create presence and absence points by converting suitability to probability using a logistic function.
    - Plot relationship between suitability and probability of occurrence with `plotSuitabilityToProba`.
    - Save virtual species objects for later use with `saveRDS` and read back with `readRDS`.
3. **Sample occurrences and place a buffer for the background data:**
    - Use `sampleOccurrences` to sample either “presence-absence” or “presence only” occurrence data, with options for introducing sampling biases.
    - Place a buffer of 200 km around the virtual species records and sample background points within this buffer, excluding presence locations.
    - Create a background mask with target resolution and extent from climate layers, setting all raster cells outside the buffer to NA.
    - Exclude cells corresponding to presence locations from the background mask.
    - Randomly select background data within the buffer, excluding presence locations.
4. **Creating pseudo-absences:**
    - Generate pseudo-absences by randomly selecting background data within the buffer, excluding presence locations for different ratios (x10, x5, x3, x1).
    - Visualize selected background points and presence points on a plot.
    - Combine presence and pseudo-absence data into a single dataset.
    - Join combined data with climate data for further analysis.
5. **Spatial thinning for DFs of 20, 50, 100, 500 and 1000 presences:** 
    - Perform spatial thinning to reduce adjacent cells of presence/background data.
    - Plot maps for each pseudo-absence level (x10, x5, x3, x1) and save the combined plot.
6. **Running SDMs for different DFs and presence/absence proportions:**
    - Load species occurrence data and corresponding thinned data for different presence/absence proportions (x20, x5, x3, x1).
    - Rename columns and join with climate variables.
    - For each dataset:
        - Check multicollinearity among environmental variables.
        - Select predictor variables based on correlation analysis.
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
