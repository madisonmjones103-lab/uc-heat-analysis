##################################
## STEP 1. SET UP
##################################

rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(janitor)
library(scales)

# Check the working directory
getwd()


##################################
## STEP 2. LOCATE CAMPUS OUTPUTS
##################################

# Folder containing all individual campus summary CSV files
data_folder <- "web/data"

# Find files such as ucla_summary.csv, ucd_summary.csv, etc.
summary_files <- list.files(
  path = data_folder,
  pattern = "_summary\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)

# Exclude any previously combined summary file
summary_files <- summary_files[
  !grepl(
    paste0(
      "combined_uc_campus_summary|",
      "uc_campus_heat_vegetation_summary"
    ),
    basename(summary_files)
  )
]

# Check which individual campus files were found
print(summary_files)

if (length(summary_files) == 0) {
  stop(
    paste0(
      "No individual campus summary files were found in ",
      data_folder
    )
  )
}


##################################
## STEP 3. COMBINE CAMPUS SUMMARIES
##################################

# Read every individual campus summary file
# and stack them into one table
campus_summary <- summary_files |>
  map_dfr(
    read_csv,
    show_col_types = FALSE
  ) |>
  clean_names()

# View the combined data
print(campus_summary)
glimpse(campus_summary)


##################################
## STEP 4. CHECK REQUIRED COLUMNS
##################################

required_columns <- c(
  "campus",
  "mean_surface_temp_f",
  "median_surface_temp_f",
  "mean_ndvi",
  "median_ndvi"
)

missing_columns <- setdiff(
  required_columns,
  names(campus_summary)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "The following required columns are missing:",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


##################################
## STEP 5. STANDARDIZE CAMPUS NAMES
##################################

campus_summary <- campus_summary |>
  mutate(
    campus = case_when(
      campus %in% c(
        "Berkeley",
        "UCB",
        "UC Berkeley"
      ) ~ "UC Berkeley",
      
      campus %in% c(
        "UCD",
        "Ucd",
        "UC Davis"
      ) ~ "UC Davis",
      
      campus %in% c(
        "UCI",
        "Uci",
        "UC Irvine"
      ) ~ "UC Irvine",
      
      campus %in% c(
        "UCLA",
        "UC Los Angeles"
      ) ~ "UCLA",
      
      campus %in% c(
        "UCM",
        "Ucm",
        "UC Merced"
      ) ~ "UC Merced",
      
      campus %in% c(
        "UCR",
        "Ucr",
        "UC Riverside"
      ) ~ "UC Riverside",
      
      campus %in% c(
        "UCSB",
        "Ucsb",
        "UC Santa Barbara"
      ) ~ "UC Santa Barbara",
      
      campus %in% c(
        "UCSC",
        "Ucsc",
        "UC Santa Cruz"
      ) ~ "UC Santa Cruz",
      
      campus %in% c(
        "UCSD",
        "Ucsd",
        "UC San Diego"
      ) ~ "UC San Diego",
      
      TRUE ~ campus
    )
  )


##################################
## STEP 6. CLEAN NUMERIC COLUMNS
##################################

campus_summary <- campus_summary |>
  mutate(
    mean_surface_temp_f = as.numeric(
      mean_surface_temp_f
    ),
    
    median_surface_temp_f = as.numeric(
      median_surface_temp_f
    ),
    
    mean_ndvi = as.numeric(
      mean_ndvi
    ),
    
    median_ndvi = as.numeric(
      median_ndvi
    )
  )

# Convert optional columns when present
optional_numeric_columns <- c(
  "scene_count",
  "mean_valid_observations",
  "min_surface_temp_f",
  "max_surface_temp_f",
  "sd_surface_temp_f",
  "min_ndvi",
  "max_ndvi",
  "sd_ndvi",
  "pixel_count"
)

existing_optional_numeric_columns <- intersect(
  optional_numeric_columns,
  names(campus_summary)
)

campus_summary <- campus_summary |>
  mutate(
    across(
      all_of(
        existing_optional_numeric_columns
      ),
      as.numeric
    )
  )


##################################
## STEP 7. REMOVE INCOMPLETE ROWS
##################################

campus_summary <- campus_summary |>
  filter(
    !is.na(campus),
    !is.na(mean_surface_temp_f),
    !is.na(median_surface_temp_f),
    !is.na(mean_ndvi),
    !is.na(median_ndvi)
  )


##################################
## STEP 8. REMOVE DUPLICATE CAMPUSES
##################################

# Prefer rows that contain scene-count information
# when duplicate campus summaries exist
if ("scene_count" %in% names(campus_summary)) {
  campus_summary <- campus_summary |>
    arrange(
      campus,
      desc(
        !is.na(scene_count)
      ),
      desc(scene_count)
    ) |>
    distinct(
      campus,
      .keep_all = TRUE
    )
} else {
  campus_summary <- campus_summary |>
    distinct(
      campus,
      .keep_all = TRUE
    )
}

# Check that nine campuses remain
print(campus_summary)

cat(
  "\nNumber of unique campuses:",
  nrow(campus_summary),
  "\n"
)

if (nrow(campus_summary) != 9) {
  warning(
    paste0(
      "Expected 9 unique UC campuses, but found ",
      nrow(campus_summary),
      ". Check the individual summary files."
    )
  )
}


##################################
## STEP 9. ADD CAMPUS FILE IDS
##################################

campus_summary <- campus_summary |>
  mutate(
    campus_id = case_when(
      campus == "UC Berkeley" ~ "berkeley",
      campus == "UC Davis" ~ "ucd",
      campus == "UC Irvine" ~ "uci",
      campus == "UCLA" ~ "ucla",
      campus == "UC Merced" ~ "ucm",
      campus == "UC Riverside" ~ "ucr",
      campus == "UC San Diego" ~ "ucsd",
      campus == "UC Santa Barbara" ~ "ucsb",
      campus == "UC Santa Cruz" ~ "ucsc",
      TRUE ~ NA_character_
    )
  )

if (any(is.na(campus_summary$campus_id))) {
  stop(
    paste(
      "A campus could not be matched to a campus_id:",
      paste(
        campus_summary$campus[
          is.na(campus_summary$campus_id)
        ],
        collapse = ", "
      )
    )
  )
}


##################################
## STEP 10. ADD WEBSITE FILENAMES
##################################

campus_summary <- campus_summary |>
  mutate(
    heat_image = paste0(
      campus_id,
      "_heat.png"
    ),
    
    ndvi_image = paste0(
      campus_id,
      "_ndvi.png"
    ),
    
    metadata_file = paste0(
      campus_id,
      "_map_metadata.json"
    ),
    
    data_file = paste0(
      campus_id,
      "_temperature_ndvi.csv"
    ),
    
    summary_file = paste0(
      campus_id,
      "_summary.csv"
    )
  )


##################################
## STEP 11. CHECK WEBSITE FILES
##################################

campus_summary <- campus_summary |>
  mutate(
    heat_image_exists = file.exists(
      file.path(
        data_folder,
        heat_image
      )
    ),
    
    ndvi_image_exists = file.exists(
      file.path(
        data_folder,
        ndvi_image
      )
    ),
    
    metadata_exists = file.exists(
      file.path(
        data_folder,
        metadata_file
      )
    ),
    
    data_file_exists = file.exists(
      file.path(
        data_folder,
        data_file
      )
    ),
    
    summary_file_exists = file.exists(
      file.path(
        data_folder,
        summary_file
      )
    )
  )

# Show any missing website files
missing_web_files <- campus_summary |>
  filter(
    !heat_image_exists |
      !ndvi_image_exists |
      !metadata_exists |
      !data_file_exists |
      !summary_file_exists
  ) |>
  select(
    campus,
    heat_image_exists,
    ndvi_image_exists,
    metadata_exists,
    data_file_exists,
    summary_file_exists
  )

if (nrow(missing_web_files) > 0) {
  cat(
    "\nSome campus website files are missing:\n"
  )
  
  print(missing_web_files)
} else {
  cat(
    "\nAll expected campus website files were found.\n"
  )
}


##################################
## STEP 12. RECALCULATE RANKINGS
##################################

campus_summary <- campus_summary |>
  select(
    -any_of(
      c(
        "heat_rank",
        "vegetation_rank"
      )
    )
  ) |>
  mutate(
    heat_rank = rank(
      -mean_surface_temp_f,
      ties.method = "min"
    ),
    
    vegetation_rank = rank(
      -mean_ndvi,
      ties.method = "min"
    )
  ) |>
  arrange(
    heat_rank
  )

print(
  campus_summary |>
    select(
      campus,
      mean_surface_temp_f,
      median_surface_temp_f,
      mean_ndvi,
      median_ndvi,
      heat_rank,
      vegetation_rank
    )
)


##################################
## STEP 13. SURFACE TEMPERATURE PLOT
##################################

temperature_plot <- ggplot(
  campus_summary,
  aes(
    x = reorder(
      campus,
      mean_surface_temp_f
    ),
    y = mean_surface_temp_f
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = round(
        mean_surface_temp_f,
        1
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_y_continuous(
    labels = label_number(
      suffix = "°F"
    ),
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  labs(
    title = paste0(
      "Average land surface temperature ",
      "across UC campuses"
    ),
    subtitle = paste0(
      "Higher values indicate hotter ",
      "campus surfaces"
    ),
    x = NULL,
    y = "Average land surface temperature"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(
      face = "bold"
    )
  )

print(temperature_plot)


##################################
## STEP 14. VEGETATION PLOT
##################################

vegetation_plot <- ggplot(
  campus_summary,
  aes(
    x = reorder(
      campus,
      mean_ndvi
    ),
    y = mean_ndvi
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = round(
        mean_ndvi,
        2
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      NA
    ),
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  labs(
    title = paste0(
      "Average vegetation levels ",
      "across UC campuses"
    ),
    subtitle = paste0(
      "Higher NDVI values generally indicate ",
      "denser or healthier vegetation"
    ),
    x = NULL,
    y = "Average NDVI"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(
      face = "bold"
    )
  )

print(vegetation_plot)


##################################
## STEP 15. TEMPERATURE VS. VEGETATION
##################################

vegetation_temperature_plot <- ggplot(
  campus_summary,
  aes(
    x = mean_ndvi,
    y = mean_surface_temp_f
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = campus
    ),
    nudge_y = 0.5,
    check_overlap = TRUE,
    size = 3.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = paste0(
      "Vegetation and surface temperature ",
      "across UC campuses"
    ),
    subtitle = "Each point represents one UC campus",
    x = "Average NDVI",
    y = "Average land surface temperature (°F)"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

print(vegetation_temperature_plot)


##################################
## STEP 16. CALCULATE RELATIONSHIP
##################################

campus_correlation <- cor(
  campus_summary$mean_ndvi,
  campus_summary$mean_surface_temp_f,
  use = "complete.obs",
  method = "pearson"
)

print(
  paste(
    paste0(
      "Correlation between average NDVI ",
      "and average surface temperature:"
    ),
    round(
      campus_correlation,
      3
    )
  )
)

campus_model <- lm(
  mean_surface_temp_f ~ mean_ndvi,
  data = campus_summary
)

print(
  summary(campus_model)
)


##################################
## STEP 17. CREATE OUTPUT FOLDERS
##################################

combined_output_folder <- file.path(
  "output",
  "combined"
)

dir.create(
  combined_output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  data_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


##################################
## STEP 18. EXPORT COMBINED DATA
##################################

# Full analytical dataset
write_csv(
  campus_summary,
  file.path(
    combined_output_folder,
    "uc_campus_heat_vegetation_summary.csv"
  )
)

# Website lookup table placed inside web/data
write_csv(
  campus_summary,
  file.path(
    data_folder,
    "uc_campus_heat_vegetation_summary.csv"
  )
)


##################################
## STEP 19. EXPORT WEBSITE LOOKUP TABLE
##################################

website_lookup <- campus_summary |>
  select(
    campus,
    campus_id,
    heat_image,
    ndvi_image,
    metadata_file,
    data_file,
    summary_file,
    mean_surface_temp_f,
    median_surface_temp_f,
    mean_ndvi,
    median_ndvi,
    heat_rank,
    vegetation_rank
  )

write_csv(
  website_lookup,
  file.path(
    data_folder,
    "uc_campus_file_lookup.csv"
  )
)

print(website_lookup)


##################################
## STEP 20. EXPORT PLOTS
##################################

ggsave(
  filename = file.path(
    combined_output_folder,
    "uc_surface_temperature_comparison.png"
  ),
  plot = temperature_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    combined_output_folder,
    "uc_vegetation_comparison.png"
  ),
  plot = vegetation_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    combined_output_folder,
    "uc_vegetation_vs_temperature.png"
  ),
  plot = vegetation_temperature_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)


##################################
## STEP 21. FINAL CHECK
##################################

final_check <- campus_summary |>
  select(
    campus,
    campus_id,
    mean_surface_temp_f,
    median_surface_temp_f,
    mean_ndvi,
    median_ndvi,
    heat_rank,
    vegetation_rank,
    heat_image_exists,
    ndvi_image_exists,
    metadata_exists,
    data_file_exists,
    summary_file_exists
  )

print(final_check)

cat(
  "\nCombined campus summary saved to:\n",
  file.path(
    combined_output_folder,
    "uc_campus_heat_vegetation_summary.csv"
  ),
  "\n"
)

cat(
  "\nWebsite campus summary saved to:\n",
  file.path(
    data_folder,
    "uc_campus_heat_vegetation_summary.csv"
  ),
  "\n"
)

cat(
  "\nWebsite file lookup saved to:\n",
  file.path(
    data_folder,
    "uc_campus_file_lookup.csv"
  ),
  "\n"
)

print(
  "UC campus comparison analysis complete."
)