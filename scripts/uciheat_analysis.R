##################################
## STEP 1. SET UP
##################################

rm(list = ls())

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(jsonlite)
library(stringr)

# Check where R is currently working
getwd()


##################################
## STEP 2. LOCATE UC IRVINE FILES
##################################

campus <- "uci"
campus_name <- "UC Irvine"

# Look for the UC Irvine Landsat folder
# in several likely locations
landsat_candidates <- c(
  file.path(
    "landsat",
    "uci_landsat"
  ),
  file.path(
    "landsat",
    "ucidata_landsat"
  ),
  "uci_landsat",
  "ucidata_landsat"
)

# Select the first folder containing
# at least one temperature raster
candidate_has_data <- vapply(
  landsat_candidates,
  function(folder) {
    dir.exists(folder) &&
      length(
        list.files(
          path = folder,
          pattern = "ST_B10\\.TIF$",
          recursive = TRUE,
          ignore.case = TRUE
        )
      ) > 0
  },
  logical(1)
)

if (!any(candidate_has_data)) {
  cat(
    "\nFolders currently inside the project:\n"
  )
  
  print(
    list.files()
  )
  
  if (dir.exists("landsat")) {
    cat(
      "\nFolders currently inside landsat:\n"
    )
    
    print(
      list.files("landsat")
    )
  }
  
  stop(
    paste0(
      "Could not find the UC Irvine Landsat files.\n",
      "Expected one of these folders:\n",
      paste(
        landsat_candidates,
        collapse = "\n"
      )
    )
  )
}

landsat_folder <- landsat_candidates[
  which(candidate_has_data)[1]
]

# Find the exact UC Irvine boundary file
boundary_candidates <- c(
  file.path(
    "boundary",
    "uci.kml"
  ),
  file.path(
    "boundary",
    "UCI.kml"
  ),
  file.path(
    "boundary",
    "UC Irvine.kml"
  )
)

boundary_exists <- file.exists(
  boundary_candidates
)

if (!any(boundary_exists)) {
  cat(
    "\nFiles currently inside boundary:\n"
  )
  
  print(
    list.files("boundary")
  )
  
  stop(
    paste0(
      "Could not find the UC Irvine boundary file.\n",
      "Expected one of these files:\n",
      paste(
        boundary_candidates,
        collapse = "\n"
      )
    )
  )
}

boundary_file <- boundary_candidates[
  which(boundary_exists)[1]
]

cat(
  "\nUsing Landsat folder:",
  landsat_folder,
  "\n"
)

cat(
  "Using boundary file:",
  boundary_file,
  "\n"
)


##################################
## STEP 3. FIND LANDSAT FILES
##################################

temperature_files <- list.files(
  path = landsat_folder,
  pattern = "ST_B10\\.TIF$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

qa_files <- list.files(
  path = landsat_folder,
  pattern = "QA_PIXEL\\.TIF$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

red_files <- list.files(
  path = landsat_folder,
  pattern = "SR_B4\\.TIF$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

nir_files <- list.files(
  path = landsat_folder,
  pattern = "SR_B5\\.TIF$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

cat(
  "\nTemperature files:",
  length(temperature_files),
  "\n"
)

cat(
  "QA files:",
  length(qa_files),
  "\n"
)

cat(
  "Red files:",
  length(red_files),
  "\n"
)

cat(
  "NIR files:",
  length(nir_files),
  "\n"
)

if (length(temperature_files) == 0) {
  stop(
    paste(
      "No ST_B10 temperature files were found in:",
      landsat_folder
    )
  )
}

if (length(qa_files) == 0) {
  stop(
    paste(
      "No QA_PIXEL files were found in:",
      landsat_folder
    )
  )
}

if (length(red_files) == 0) {
  stop(
    paste(
      "No SR_B4 red-band files were found in:",
      landsat_folder
    )
  )
}

if (length(nir_files) == 0) {
  stop(
    paste(
      "No SR_B5 near-infrared files were found in:",
      landsat_folder
    )
  )
}


##################################
## STEP 4. MATCH FILES BY SCENE
##################################

# Extract satellite, path/row and acquisition date
get_scene_id <- function(file_path) {
  str_extract(
    basename(file_path),
    "LC0[89]_L2SP_[0-9]{6}_[0-9]{8}"
  )
}

make_scene_file_table <- function(
    files,
    file_column
) {
  output <- data.frame(
    scene_id = get_scene_id(files),
    file_path = files,
    stringsAsFactors = FALSE
  )
  
  names(output)[2] <- file_column
  
  output
}

temperature_table <- make_scene_file_table(
  temperature_files,
  "temperature_file"
)

qa_table <- make_scene_file_table(
  qa_files,
  "qa_file"
)

red_table <- make_scene_file_table(
  red_files,
  "red_file"
)

nir_table <- make_scene_file_table(
  nir_files,
  "nir_file"
)

if (any(is.na(temperature_table$scene_id))) {
  stop(
    paste0(
      "At least one temperature filename did not ",
      "contain a recognizable Landsat scene ID."
    )
  )
}

if (any(is.na(qa_table$scene_id))) {
  stop(
    paste0(
      "At least one QA filename did not ",
      "contain a recognizable Landsat scene ID."
    )
  )
}

if (any(is.na(red_table$scene_id))) {
  stop(
    paste0(
      "At least one red-band filename did not ",
      "contain a recognizable Landsat scene ID."
    )
  )
}

if (any(is.na(nir_table$scene_id))) {
  stop(
    paste0(
      "At least one NIR filename did not ",
      "contain a recognizable Landsat scene ID."
    )
  )
}

scene_table <- temperature_table |>
  full_join(
    qa_table,
    by = "scene_id"
  ) |>
  full_join(
    red_table,
    by = "scene_id"
  ) |>
  full_join(
    nir_table,
    by = "scene_id"
  ) |>
  arrange(scene_id)

print(scene_table)

incomplete_scenes <- scene_table |>
  filter(
    is.na(temperature_file) |
      is.na(qa_file) |
      is.na(red_file) |
      is.na(nir_file)
  )

if (nrow(incomplete_scenes) > 0) {
  print(incomplete_scenes)
  
  stop(
    paste0(
      "At least one Landsat scene is missing ",
      "a required temperature, QA, red or NIR file."
    )
  )
}

if (nrow(scene_table) == 0) {
  stop(
    "No complete UC Irvine Landsat scenes were matched."
  )
}

cat(
  "\nAll",
  nrow(scene_table),
  "Landsat scenes matched successfully!\n"
)


##################################
## STEP 5. IMPORT UC IRVINE BOUNDARY
##################################

uci_boundary <- st_read(
  boundary_file,
  quiet = TRUE
)

if (is.na(st_crs(uci_boundary))) {
  stop(
    paste0(
      "The UC Irvine boundary file does not ",
      "have a valid coordinate reference system."
    )
  )
}

reference_temperature <- rast(
  scene_table$temperature_file[1]
)

uci_boundary_projected <- st_transform(
  uci_boundary,
  crs(reference_temperature)
)

uci_boundary_vect <- vect(
  uci_boundary_projected
)

cat(
  "\nBoundary CRS:",
  st_crs(
    uci_boundary_projected
  )$input,
  "\n"
)

cat(
  "Reference raster CRS:",
  crs(reference_temperature),
  "\n"
)

# Optional inspection plot
plot(
  reference_temperature,
  main = paste0(
    "Reference Landsat Scene and ",
    "UC Irvine Boundary"
  )
)

plot(
  uci_boundary_vect,
  add = TRUE,
  border = "red",
  lwd = 2
)


##################################
## STEP 6. CREATE STORAGE LISTS
##################################

temperature_rasters <- vector(
  mode = "list",
  length = nrow(scene_table)
)

ndvi_rasters <- vector(
  mode = "list",
  length = nrow(scene_table)
)

valid_pixel_rasters <- vector(
  mode = "list",
  length = nrow(scene_table)
)


##################################
## STEP 7. PROCESS ALL SCENES
##################################

for (i in seq_len(nrow(scene_table))) {
  
  cat(
    "\nProcessing scene",
    i,
    "of",
    nrow(scene_table),
    "\n"
  )
  
  cat(
    scene_table$scene_id[i],
    "\n"
  )
  
  flush.console()
  
  
  ##################################
  ## READ THIS SCENE
  ##################################
  
  temperature_raw <- rast(
    scene_table$temperature_file[i]
  )
  
  qa_pixel <- rast(
    scene_table$qa_file[i]
  )
  
  red_raw <- rast(
    scene_table$red_file[i]
  )
  
  nir_raw <- rast(
    scene_table$nir_file[i]
  )
  
  
  ##################################
  ## CROP TO UC IRVINE
  ##################################
  
  temperature_crop <- mask(
    crop(
      temperature_raw,
      uci_boundary_vect
    ),
    uci_boundary_vect
  )
  
  qa_crop <- mask(
    crop(
      qa_pixel,
      uci_boundary_vect
    ),
    uci_boundary_vect
  )
  
  red_crop <- mask(
    crop(
      red_raw,
      uci_boundary_vect
    ),
    uci_boundary_vect
  )
  
  nir_crop <- mask(
    crop(
      nir_raw,
      uci_boundary_vect
    ),
    uci_boundary_vect
  )
  
  
  ##################################
  ## CREATE CLOUD MASK
  ##################################
  
  # QA_PIXEL:
  # Bit 0 = fill
  # Bit 1 = dilated cloud
  # Bit 2 = cirrus
  # Bit 3 = cloud
  # Bit 4 = cloud shadow
  # Bit 5 = snow
  
  clear_pixels <- app(
    qa_crop,
    fun = function(x) {
      
      x <- as.integer(x)
      
      contaminated <- (
        bitwAnd(x, 1L) != 0L |
          bitwAnd(x, 2L) != 0L |
          bitwAnd(x, 4L) != 0L |
          bitwAnd(x, 8L) != 0L |
          bitwAnd(x, 16L) != 0L |
          bitwAnd(x, 32L) != 0L
      )
      
      ifelse(
        !is.na(x) & !contaminated,
        1,
        NA_real_
      )
    }
  )
  
  
  ##################################
  ## CALCULATE SURFACE TEMPERATURE
  ##################################
  
  temperature_kelvin <- (
    temperature_crop * 0.00341802
  ) + 149.0
  
  temperature_celsius <- (
    temperature_kelvin - 273.15
  )
  
  temperature_fahrenheit_scene <- (
    temperature_celsius * 9 / 5
  ) + 32
  
  temperature_fahrenheit_scene <- mask(
    temperature_fahrenheit_scene,
    clear_pixels
  )
  
  names(
    temperature_fahrenheit_scene
  ) <- paste0(
    "temp_",
    scene_table$scene_id[i]
  )
  
  
  ##################################
  ## CALCULATE NDVI
  ##################################
  
  red_reflectance <- (
    red_crop * 0.0000275
  ) - 0.2
  
  nir_reflectance <- (
    nir_crop * 0.0000275
  ) - 0.2
  
  ndvi_scene <- (
    nir_reflectance - red_reflectance
  ) / (
    nir_reflectance + red_reflectance
  )
  
  ndvi_scene <- mask(
    ndvi_scene,
    clear_pixels
  )
  
  ndvi_scene[
    ndvi_scene < -1 |
      ndvi_scene > 1
  ] <- NA
  
  names(ndvi_scene) <- paste0(
    "ndvi_",
    scene_table$scene_id[i]
  )
  
  
  ##################################
  ## STORE PROCESSED RASTERS
  ##################################
  
  temperature_rasters[[i]] <-
    temperature_fahrenheit_scene
  
  ndvi_rasters[[i]] <-
    ndvi_scene
  
  valid_pixel_rasters[[i]] <- ifel(
    !is.na(
      temperature_fahrenheit_scene
    ),
    1,
    NA
  )
  
  cat(
    "Scene processed successfully!\n"
  )
  
  flush.console()
}

cat(
  "\nAll",
  nrow(scene_table),
  "scenes were processed successfully!\n"
)


##################################
## STEP 8. BUILD COMPOSITES
##################################

temperature_stack <- rast(
  temperature_rasters
)

ndvi_stack <- rast(
  ndvi_rasters
)

valid_pixel_stack <- rast(
  valid_pixel_rasters
)

temperature_fahrenheit <- app(
  temperature_stack,
  median,
  na.rm = TRUE
)

ndvi <- app(
  ndvi_stack,
  median,
  na.rm = TRUE
)

valid_observations <- app(
  valid_pixel_stack,
  sum,
  na.rm = TRUE
)

valid_observations[
  valid_observations == 0
] <- NA

names(
  temperature_fahrenheit
) <- "surface_temp_f"

names(ndvi) <- "ndvi"

names(
  valid_observations
) <- "valid_observations"

cat(
  "Summer composite rasters created successfully!\n"
)


##################################
## STEP 9. VALIDATE COMPOSITES
##################################

cat(
  "\nSurface-temperature summary:\n"
)

print(
  global(
    temperature_fahrenheit,
    c(
      "min",
      "mean",
      "max",
      "sd"
    ),
    na.rm = TRUE
  )
)

print(
  global(
    temperature_fahrenheit,
    median,
    na.rm = TRUE
  )
)

cat(
  "\nNDVI summary:\n"
)

print(
  global(
    ndvi,
    c(
      "min",
      "mean",
      "max",
      "sd"
    ),
    na.rm = TRUE
  )
)

print(
  global(
    ndvi,
    median,
    na.rm = TRUE
  )
)

cat(
  "\nValid-observation summary:\n"
)

print(
  global(
    valid_observations,
    c(
      "min",
      "mean",
      "max"
    ),
    na.rm = TRUE
  )
)


##################################
## STEP 10. CREATE STUDY DATES
##################################

scene_dates <- as.Date(
  str_extract(
    scene_table$scene_id,
    "[0-9]{8}$"
  ),
  format = "%Y%m%d"
)

study_start <- min(
  scene_dates,
  na.rm = TRUE
)

study_end <- max(
  scene_dates,
  na.rm = TRUE
)

cat(
  "\nStudy period:",
  as.character(study_start),
  "through",
  as.character(study_end),
  "\n"
)


##################################
## STEP 11. PLOT INITIAL RESULTS
##################################

plot(
  temperature_fahrenheit,
  main = paste0(
    "Median UC Irvine Surface Temperature\n",
    study_start,
    " to ",
    study_end
  )
)

plot(
  uci_boundary_vect,
  add = TRUE,
  border = "red",
  lwd = 2
)

plot(
  ndvi,
  main = paste0(
    "Median UC Irvine NDVI\n",
    study_start,
    " to ",
    study_end
  )
)

plot(
  uci_boundary_vect,
  add = TRUE,
  border = "red",
  lwd = 2
)

plot(
  valid_observations,
  main = "Valid Landsat Observations per Pixel"
)

plot(
  uci_boundary_vect,
  add = TRUE,
  border = "red",
  lwd = 2
)


##################################
## STEP 12. SAVE COMPOSITE RASTERS
##################################

dir.create(
  "outputs/data",
  recursive = TRUE,
  showWarnings = FALSE
)

writeRaster(
  temperature_fahrenheit,
  paste0(
    "outputs/data/",
    campus,
    "_surface_temperature_median_2025.tif"
  ),
  overwrite = TRUE
)

writeRaster(
  ndvi,
  paste0(
    "outputs/data/",
    campus,
    "_ndvi_median_2025.tif"
  ),
  overwrite = TRUE
)

writeRaster(
  valid_observations,
  paste0(
    "outputs/data/",
    campus,
    "_valid_observations_2025.tif"
  ),
  overwrite = TRUE
)

cat(
  "Composite GeoTIFF files saved successfully!\n"
)


##################################
## STEP 13. PREPARE BOUNDARY FOR MAPS
##################################

uci_boundary_plot <- st_as_sf(
  uci_boundary_vect
)


##################################
## STEP 14. POLISHED HEAT MAP
##################################

heat_map <- ggplot() +
  geom_spatraster(
    data = temperature_fahrenheit
  ) +
  geom_sf(
    data = uci_boundary_plot,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  scale_fill_gradientn(
    colours = c(
      "#313695",
      "#4575b4",
      "#74add1",
      "#fdae61",
      "#f46d43",
      "#a50026"
    ),
    name = paste0(
      "Median surface\n",
      "temperature (°F)"
    ),
    na.value = "transparent"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  labs(
    title = paste0(
      "Land Surface Temperature Across ",
      campus_name
    ),
    subtitle = paste0(
      "Median of ",
      nrow(scene_table),
      " cloud-masked Landsat observations, ",
      study_start,
      " to ",
      study_end
    ),
    caption = paste0(
      "Source: USGS Landsat Collection 2 Level-2. ",
      "Values represent land-surface temperature, ",
      "not air temperature."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray40",
      hjust = 0
    ),
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    legend.text = element_text(
      size = 9
    ),
    legend.position = "right",
    plot.margin = margin(
      15,
      15,
      15,
      15
    )
  )

print(heat_map)


##################################
## STEP 15. POLISHED NDVI MAP
##################################

ndvi_map <- ggplot() +
  geom_spatraster(
    data = ndvi
  ) +
  geom_sf(
    data = uci_boundary_plot,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  scale_fill_gradientn(
    colours = c(
      "#d9d9d9",
      "#c2b280",
      "#a1d76a",
      "#4d9221",
      "#1b5e20"
    ),
    name = paste0(
      "Median vegetation\n",
      "index (NDVI)"
    ),
    limits = c(
      0,
      0.8
    ),
    oob = scales::squish,
    na.value = "transparent"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  labs(
    title = paste0(
      "Vegetation Across ",
      campus_name
    ),
    subtitle = paste0(
      "Median of ",
      nrow(scene_table),
      " cloud-masked Landsat observations; ",
      "higher NDVI indicates denser vegetation"
    ),
    caption = paste0(
      "Source: USGS Landsat Collection 2 Level-2."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray40",
      hjust = 0
    ),
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    legend.text = element_text(
      size = 9
    ),
    legend.position = "right",
    plot.margin = margin(
      15,
      15,
      15,
      15
    )
  )

print(ndvi_map)


##################################
## STEP 16. VALID-OBSERVATION MAP
##################################

observation_map <- ggplot() +
  geom_spatraster(
    data = valid_observations
  ) +
  geom_sf(
    data = uci_boundary_plot,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  scale_fill_viridis_c(
    name = "Valid\nobservations",
    breaks = seq_len(
      nrow(scene_table)
    ),
    limits = c(
      1,
      nrow(scene_table)
    ),
    na.value = "transparent"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  labs(
    title = paste0(
      "Valid Satellite Observations Across ",
      campus_name
    ),
    subtitle = paste0(
      "Maximum possible observations: ",
      nrow(scene_table)
    ),
    caption = paste0(
      "Clouds, cloud shadows, cirrus, snow and invalid pixels ",
      "were removed before compositing."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray40",
      hjust = 0
    ),
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    legend.text = element_text(
      size = 9
    ),
    legend.position = "right",
    plot.margin = margin(
      15,
      15,
      15,
      15
    )
  )

print(observation_map)


##################################
## STEP 17. SAVE POLISHED MAPS
##################################

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = paste0(
    "outputs/figures/",
    campus,
    "_surface_temperature_composite.png"
  ),
  plot = heat_map,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = paste0(
    "outputs/figures/",
    campus,
    "_ndvi_composite.png"
  ),
  plot = ndvi_map,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = paste0(
    "outputs/figures/",
    campus,
    "_valid_observations.png"
  ),
  plot = observation_map,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat(
  "Polished UC Irvine map images saved successfully!\n"
)


##################################
## STEP 18. PREPARE WEB MAP LAYERS
##################################

dir.create(
  "web/data",
  recursive = TRUE,
  showWarnings = FALSE
)

heat_web <- project(
  temperature_fahrenheit,
  "EPSG:4326",
  method = "bilinear"
)

ndvi_web <- project(
  ndvi,
  "EPSG:4326",
  method = "bilinear"
)

observations_web <- project(
  valid_observations,
  "EPSG:4326",
  method = "near"
)

# Put all layers on the same grid
ndvi_web <- resample(
  ndvi_web,
  heat_web,
  method = "bilinear"
)

observations_web <- resample(
  observations_web,
  heat_web,
  method = "near"
)

names(heat_web) <- "temperature"
names(ndvi_web) <- "ndvi"

names(
  observations_web
) <- "valid_observations"


##################################
## STEP 19. CALCULATE WEB RANGES
##################################

heat_limits <- global(
  heat_web,
  quantile,
  probs = c(
    0.02,
    0.98
  ),
  na.rm = TRUE
)

ndvi_limits <- global(
  ndvi_web,
  quantile,
  probs = c(
    0.02,
    0.98
  ),
  na.rm = TRUE
)

heat_min <- as.numeric(
  heat_limits[1, 1]
)

heat_max <- as.numeric(
  heat_limits[1, 2]
)

ndvi_min <- as.numeric(
  ndvi_limits[1, 1]
)

ndvi_max <- as.numeric(
  ndvi_limits[1, 2]
)

cat(
  "\nWeb heat range:",
  heat_min,
  "to",
  heat_max,
  "\n"
)

cat(
  "Web NDVI range:",
  ndvi_min,
  "to",
  ndvi_max,
  "\n"
)


##################################
## STEP 20. EXPORT WEB PNGS
##################################

png(
  filename = paste0(
    "web/data/",
    campus,
    "_heat.png"
  ),
  width = 1600,
  height = 1600,
  bg = "transparent"
)

par(
  mar = c(
    0,
    0,
    0,
    0
  )
)

plot(
  heat_web,
  col = hcl.colors(
    100,
    "Inferno"
  ),
  range = c(
    heat_min,
    heat_max
  ),
  axes = FALSE,
  legend = FALSE,
  box = FALSE
)

dev.off()

png(
  filename = paste0(
    "web/data/",
    campus,
    "_ndvi.png"
  ),
  width = 1600,
  height = 1600,
  bg = "transparent"
)

par(
  mar = c(
    0,
    0,
    0,
    0
  )
)

plot(
  ndvi_web,
  col = hcl.colors(
    100,
    "Greens 3"
  ),
  range = c(
    ndvi_min,
    ndvi_max
  ),
  axes = FALSE,
  legend = FALSE,
  box = FALSE
)

dev.off()

cat(
  "Transparent UC Irvine web PNGs exported successfully!\n"
)


##################################
## STEP 21. CREATE WEB DATAFRAME
##################################

analysis_stack <- c(
  heat_web,
  ndvi_web,
  observations_web
)

names(analysis_stack) <- c(
  "temperature",
  "ndvi",
  "valid_observations"
)

analysis_df <- as.data.frame(
  analysis_stack,
  xy = TRUE,
  na.rm = FALSE
)

names(analysis_df) <- c(
  "longitude",
  "latitude",
  "temperature",
  "ndvi",
  "valid_observations"
)

analysis_df <- analysis_df |>
  filter(
    !is.na(temperature) |
      !is.na(ndvi)
  ) |>
  mutate(
    longitude = round(
      longitude,
      6
    ),
    latitude = round(
      latitude,
      6
    ),
    temperature = round(
      temperature,
      2
    ),
    ndvi = round(
      ndvi,
      3
    ),
    valid_observations = as.integer(
      round(valid_observations)
    )
  )

head(analysis_df)
summary(analysis_df)

write.csv(
  analysis_df,
  paste0(
    "web/data/",
    campus,
    "_temperature_ndvi.csv"
  ),
  row.names = FALSE
)

cat(
  "UC Irvine web data CSV exported successfully!\n"
)


##################################
## STEP 22. EXPORT CAMPUS SUMMARY
##################################

campus_summary <- data.frame(
  campus = campus_name,
  statistic = "median",
  scene_count = nrow(scene_table),
  start_date = study_start,
  end_date = study_end,
  
  median_surface_temp_f = global(
    temperature_fahrenheit,
    median,
    na.rm = TRUE
  )[1, 1],
  
  mean_surface_temp_f = global(
    temperature_fahrenheit,
    "mean",
    na.rm = TRUE
  )[1, 1],
  
  median_ndvi = global(
    ndvi,
    median,
    na.rm = TRUE
  )[1, 1],
  
  mean_ndvi = global(
    ndvi,
    "mean",
    na.rm = TRUE
  )[1, 1],
  
  mean_valid_observations = global(
    valid_observations,
    "mean",
    na.rm = TRUE
  )[1, 1]
)

write.csv(
  campus_summary,
  paste0(
    "web/data/",
    campus,
    "_summary.csv"
  ),
  row.names = FALSE
)

print(campus_summary)


##################################
## STEP 23. EXPORT UC IRVINE METADATA
##################################

# Use the exact raster extent so the
# exported PNG aligns with Leaflet
heat_extent <- ext(
  heat_web
)

uci_map_metadata <- list(
  campus = campus,
  campus_name = campus_name,
  statistic = "median",
  scene_count = nrow(scene_table),
  
  start_date = as.character(
    study_start
  ),
  
  end_date = as.character(
    study_end
  ),
  
  bounds = list(
    south = ymin(
      heat_extent
    ),
    west = xmin(
      heat_extent
    ),
    north = ymax(
      heat_extent
    ),
    east = xmax(
      heat_extent
    )
  ),
  
  heat = list(
    min = unname(
      heat_min
    ),
    max = unname(
      heat_max
    ),
    units = "°F",
    variable = "land surface temperature"
  ),
  
  ndvi = list(
    min = unname(
      ndvi_min
    ),
    max = unname(
      ndvi_max
    )
  ),
  
  valid_observations = list(
    minimum = 1,
    maximum = nrow(
      scene_table
    )
  ),
  
  files = list(
    heat_image = paste0(
      campus,
      "_heat.png"
    ),
    ndvi_image = paste0(
      campus,
      "_ndvi.png"
    ),
    data = paste0(
      campus,
      "_temperature_ndvi.csv"
    ),
    summary = paste0(
      campus,
      "_summary.csv"
    )
  )
)

write_json(
  uci_map_metadata,
  paste0(
    "web/data/",
    campus,
    "_map_metadata.json"
  ),
  pretty = TRUE,
  auto_unbox = TRUE
)

print(
  uci_map_metadata
)

cat(
  "\nSaved web/data/uci_map_metadata.json\n"
)


##################################
## STEP 24. CONFIRM OUTPUTS
##################################

cat(
  "\nAll UC Irvine outputs exported successfully!\n"
)

cat(
  "Web data rows:",
  nrow(analysis_df),
  "\n"
)

cat(
  "Scenes included:",
  nrow(scene_table),
  "\n"
)

cat(
  "Study period:",
  as.character(study_start),
  "through",
  as.character(study_end),
  "\n"
)

cat(
  "\nUC Irvine files in outputs/data:\n"
)

print(
  list.files(
    "outputs/data",
    pattern = campus,
    ignore.case = TRUE
  )
)

cat(
  "\nUC Irvine files in outputs/figures:\n"
)

print(
  list.files(
    "outputs/figures",
    pattern = campus,
    ignore.case = TRUE
  )
)

cat(
  "\nUC Irvine files in web/data:\n"
)

print(
  list.files(
    "web/data",
    pattern = campus,
    ignore.case = TRUE
  )
)

cat(
  "\nUC Irvine analysis complete!\n"
)

