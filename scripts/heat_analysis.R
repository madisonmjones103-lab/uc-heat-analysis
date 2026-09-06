
## STEP 1. SET UP

rm(list = ls())

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(jsonlite)

# Check where R is currently working
getwd()

## STEP 2. LOCATE FILES

# Folder containing all extracted Landsat files
landsat_folder <- "ucladata_landstat"

# View available files
list.files(landsat_folder)
list.files("boundary")
list.files("canopy")

# Find every required Landsat file
temperature_files <- list.files(
  landsat_folder,
  pattern = "ST_B10\\.TIF$",
  full.names = TRUE,
  recursive = TRUE
)

qa_files <- list.files(
  landsat_folder,
  pattern = "QA_PIXEL\\.TIF$",
  full.names = TRUE,
  recursive = TRUE
)

red_files <- list.files(
  landsat_folder,
  pattern = "SR_B4\\.TIF$",
  full.names = TRUE,
  recursive = TRUE
)

nir_files <- list.files(
  landsat_folder,
  pattern = "SR_B5\\.TIF$",
  full.names = TRUE,
  recursive = TRUE
)

boundary_file <- list.files(
  "boundary",
  pattern = "\\.kml$",
  full.names = TRUE
)

# Check how many files were found
cat("Temperature files:", length(temperature_files), "\n")
cat("QA files:", length(qa_files), "\n")
cat("Red files:", length(red_files), "\n")
cat("NIR files:", length(nir_files), "\n")
cat("Boundary files:", length(boundary_file), "\n")

# Stop if the number of Landsat files does not match
stopifnot(length(temperature_files) > 0)
stopifnot(length(temperature_files) == length(qa_files))
stopifnot(length(temperature_files) == length(red_files))
stopifnot(length(temperature_files) == length(nir_files))
stopifnot(length(boundary_file) == 1)

# Sort files so scenes line up by filename/date
temperature_files <- sort(temperature_files)
qa_files <- sort(qa_files)
red_files <- sort(red_files)
nir_files <- sort(nir_files)

# Print filenames for inspection
temperature_files
qa_files
red_files
nir_files

library(stringr)

## STEP 3. MATCH FILES BY SCENE

# Sort all file paths
temperature_files <- sort(temperature_files)
qa_files <- sort(qa_files)
red_files <- sort(red_files)
nir_files <- sort(nir_files)

# Extract the scene identifier through the acquisition date
get_scene_id <- function(file_path) {
  stringr::str_extract(
    basename(file_path),
    "LC0[89]_L2SP_041036_[0-9]{8}"
  )
}

scene_table <- data.frame(
  scene_id_temp = get_scene_id(temperature_files),
  scene_id_qa = get_scene_id(qa_files),
  scene_id_red = get_scene_id(red_files),
  scene_id_nir = get_scene_id(nir_files),
  temperature_file = temperature_files,
  qa_file = qa_files,
  red_file = red_files,
  nir_file = nir_files
)

print(
  scene_table[
    c(
      "scene_id_temp",
      "scene_id_qa",
      "scene_id_red",
      "scene_id_nir"
    )
  ]
)

# Confirm that all four files in each row belong to the same scene
stopifnot(
  scene_table$scene_id_temp == scene_table$scene_id_qa,
  scene_table$scene_id_temp == scene_table$scene_id_red,
  scene_table$scene_id_temp == scene_table$scene_id_nir
)

cat("All Landsat scene files matched successfully!\n")


## STEP 4. IMPORT UCLA BOUNDARY

ucla_boundary <- st_read(
  boundary_file,
  quiet = TRUE
)

# Use the first temperature raster to identify the Landsat CRS
reference_temperature <- rast(
  scene_table$temperature_file[1]
)

# Transform the UCLA boundary into the Landsat CRS
ucla_boundary_projected <- st_transform(
  ucla_boundary,
  crs(reference_temperature)
)

# Convert the sf boundary to a terra vector
ucla_boundary_vect <- vect(
  ucla_boundary_projected
)

# Inspect the boundary and reference raster
ucla_boundary
ucla_boundary_vect
reference_temperature

# Check that the boundary overlaps the Landsat scene
plot(
  reference_temperature,
  main = "Reference Landsat Scene and UCLA Boundary"
)

plot(
  ucla_boundary_vect,
  add = TRUE,
  border = "red",
  lwd = 2
)

## STEP 5. CREATE STORAGE LISTS

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

## STEP 6. PROCESS ALL SCENES

for (i in seq_len(nrow(scene_table))) {
  
  cat(
    "\nProcessing scene",
    i,
    "of",
    nrow(scene_table),
    "\n"
  )
  
  cat(
    scene_table$scene_id_temp[i],
    "\n"
  )
  
  flush.console()
  
  # Read this scene
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
  
  # Crop and mask to UCLA
  temperature_crop <- mask(
    crop(
      temperature_raw,
      ucla_boundary_vect
    ),
    ucla_boundary_vect
  )
  
  qa_crop <- mask(
    crop(
      qa_pixel,
      ucla_boundary_vect
    ),
    ucla_boundary_vect
  )
  
  red_crop <- mask(
    crop(
      red_raw,
      ucla_boundary_vect
    ),
    ucla_boundary_vect
  )
  
  nir_crop <- mask(
    crop(
      nir_raw,
      ucla_boundary_vect
    ),
    ucla_boundary_vect
  )
  
  cat("Scene cropped successfully!\n")
  flush.console()
}

cat("\nAll four scenes were read and cropped successfully!\n")

