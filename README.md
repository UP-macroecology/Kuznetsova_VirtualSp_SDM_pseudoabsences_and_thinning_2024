# Kuznetsova_VirtualSp_SDM_pseudoabsences_and_thinning_2024

1. **Download environmental data & Crop it to the area of interest:**
    - Use `worldclim_global` to download global climate variables with a resolution of 30 seconds (~1 km).
    - Define the extent for the wished area using coordinates.
    - Crop global climate data to the wished area.
2. **Create virtual species (VS):**
    - Define species' response to environmental variables using `formatFunctions`.
    - Generate virtual species niche and plot spatial distribution of species’ environmental suitability using `generateSpFromFun`.
    - Use `convertToPA` to create presence and absence points by converting suitability to probability using a logistic function.
    - Plot relationship between suitability and probability of occurrence with `plotSuitabilityToProba`.
3. **Explore virtual species:**
    - Plot species-environment relationship using `plotResponse`.
    - Extract raster of environmental suitability and other details using `str` and direct access to object properties.
    - Save virtual species objects for later use with `saveRDS` and read back with `readRDS`.
4. **Sample occurrences and use the as true observations later:**
    - Use `sampleOccurrences` to sample either “presence-absence” or “presence only” occurrence data, with options for introducing sampling biases.
5. **Sampling background points:**
    - Place a buffer of 200 km around the virtual species records and sample background points within this buffer, excluding presence locations.
    - Create a background mask with target resolution and extent from climate layers, setting all raster cells outside the buffer to NA.
    - Exclude cells corresponding to presence locations from the background mask.
    - Randomly select background data within the buffer, excluding presence locations. Sample 10 times as many background points as there are presence points.
