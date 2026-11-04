
# ============================================================
# VICTORIAN CRASH DATA PROJECT
# DATA CLEANING AND DATA INTEGRATION
# ============================================================


# STEP 1: LOAD PACKAGES

library(tidyverse)
library(janitor)


# STEP 2: CREATE OUTPUT FOLDER

# Create the processed folder before saving any reports

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)


# STEP 3: IMPORT ALL NINE DATASETS

files <- list.files(
  "data",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Check that all nine files are present

if (length(files) != 9) {
  stop("Expected 9 CSV files. Check the data folder.")
}

# Import CSV files

data_list <- lapply(
  files,
  function(file) read_csv(file, show_col_types = FALSE)
)

# Assign dataset names

names(data_list) <- tools::file_path_sans_ext(
  basename(files)
)

# Clean column names

data_list <- lapply(data_list, clean_names)

# Display dataset names

print(names(data_list))


# STEP 4: CLEAN BLANK VALUES

# Convert blank character values into NA

data_list <- lapply(data_list, function(df) {
  
  df <- df %>%
    mutate(
      across(
        where(is.character),
        ~ na_if(trimws(.x), "")
      )
    )
  
  # Remove columns where all values are missing
  
  df <- df %>%
    select(where(~ !all(is.na(.x))))
  
  return(df)
})


# STEP 5: CHECK DATASET DIMENSIONS

dimension_summary <- data.frame(
  Dataset = names(data_list),
  Rows = sapply(data_list, nrow),
  Columns = sapply(data_list, ncol)
)

print(dimension_summary)


# STEP 6: CHECK MISSING VALUES

missing_summary <- bind_rows(
  lapply(names(data_list), function(name) {
    
    df <- data_list[[name]]
    
    data.frame(
      Dataset = name,
      Column = names(df),
      Missing_Values = colSums(is.na(df)),
      Missing_Percentage = round(
        colMeans(is.na(df)) * 100, 2
      )
    )
  })
)

# Display columns containing missing values

missing_results <- missing_summary %>%
  filter(Missing_Values > 0)

print(missing_results)

# Save missing value report

write_csv(
  missing_summary,
  "data/processed/missing_value_summary.csv"
)


# STEP 7: CHECK DUPLICATE ROWS

duplicate_summary <- data.frame(
  Dataset = names(data_list),
  Duplicate_Rows = sapply(
    data_list,
    function(df) sum(duplicated(df))
  )
)

print(duplicate_summary)

# Save duplicate report

write_csv(
  duplicate_summary,
  "data/processed/duplicate_summary.csv"
)


# STEP 8: CHECK UNIQUE ACCIDENT NUMBERS

key_summary <- data.frame(
  Dataset = names(data_list),
  Total_Rows = sapply(data_list, nrow),
  Unique_Accidents = sapply(
    data_list,
    function(df) n_distinct(df$accident_no)
  )
)

print(key_summary)


# STEP 9: SELECT MAIN ACCIDENT DATASET

# The accident table contains one row per accident

accident <- data_list$accident

# Check that accident numbers are unique

if (anyDuplicated(accident$accident_no) > 0) {
  stop("Repeated accident numbers found in main dataset.")
}


# STEP 10: DEFINE MODE FUNCTION

# This function finds the most frequent category
# It will be used when summarising multiple records
# belonging to the same accident.

mode_value <- function(x) {
  
  x <- as.character(x)
  
  x <- x[!is.na(x) & nzchar(trimws(x))]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  counts <- sort(
    table(x),
    decreasing = TRUE
  )
  
  return(names(counts)[1])
}


# STEP 11: DEFINE SUMMARY FUNCTION

# This function converts multiple records per accident
# into one summary row per accident.

prepare_summary <- function(df, selected_columns, prefix) {
  
  result <- df %>%
    
    group_by(accident_no) %>%
    
    summarise(
      
      record_count = n(),
      
      across(
        all_of(selected_columns),
        ~ mode_value(.x)
      ),
      
      .groups = "drop"
      
    ) %>%
    
    rename_with(
      ~ paste0(prefix, "_", .x),
      -accident_no
    )
  
  return(result)
}


# STEP 12: SUMMARISE ACCIDENT EVENT DATA

event_summary <- prepare_summary(
  data_list$accident_event,
  c("event_type_desc"),
  "event"
)


# STEP 13: SUMMARISE ATMOSPHERIC CONDITIONS

atmosphere_summary <- prepare_summary(
  data_list$atmospheric_cond,
  c("atmosph_cond_desc"),
  "atmosphere"
)


# STEP 14: SUMMARISE ROAD SURFACE CONDITIONS

surface_summary <- prepare_summary(
  data_list$road_surface_cond,
  c("surface_cond_desc"),
  "surface"
)


# STEP 15: SUMMARISE PERSON DATA

# Keep selected demographic and road-user information.
# Injury-related columns are excluded from model predictors.

person_summary <- prepare_summary(
  data_list$person,
  c(
    "age_group",
    "sex",
    "road_user_type_desc"
  ),
  "person"
)


# STEP 16: SUMMARISE VEHICLE DATA

vehicle_summary <- prepare_summary(
  data_list$vehicle,
  c(
    "vehicle_type_desc",
    "vehicle_body_style",
    "vehicle_movement"
  ),
  "vehicle"
)


# STEP 17: SUMMARISE SUB-DCA DATA

sub_dca_summary <- prepare_summary(
  data_list$sub_dca,
  c("sub_dca_code_desc"),
  "sub_dca"
)


# STEP 18: PREPARE LOCATION DATA

# Location has one row per accident according to
# the earlier duplicate accident number check.

location <- data_list$accident_location %>%
  select(-node_id)

# Confirm unique accident numbers

if (anyDuplicated(location$accident_no) > 0) {
  stop("Location table contains repeated accident numbers.")
}


# STEP 19: PREPARE NODE DATA

# Keep geographical information and the matching keys

node_selected <- data_list$node %>%
  select(
    accident_no,
    node_id,
    lga_name,
    deg_urban_name,
    latitude,
    longitude,
    postcode_crash
  )

# Function to select the first non-missing value

first_non_missing <- function(x) {
  
  available <- which(!is.na(x))
  
  if (length(available) == 0) {
    return(x[NA_integer_])
  }
  
  x[available[1]]
}

# Group by accident number and node ID.
# This produces one row for each accident/node combination.

node_summary <- node_selected %>%
  
  group_by(accident_no, node_id) %>%
  
  summarise(
    
    across(
      c(
        lga_name,
        deg_urban_name,
        latitude,
        longitude,
        postcode_crash
      ),
      first_non_missing
    ),
    
    .groups = "drop"
  )


# STEP 20: INTEGRATE ALL DATASETS

# Start with the main accident dataset.
# Left joins retain all records from the accident table.

integrated_data <- accident %>%
  
  left_join(
    location,
    by = "accident_no"
  ) %>%
  
  left_join(
    node_summary,
    by = c("accident_no", "node_id")
  ) %>%
  
  left_join(
    event_summary,
    by = "accident_no"
  ) %>%
  
  left_join(
    atmosphere_summary,
    by = "accident_no"
  ) %>%
  
  left_join(
    surface_summary,
    by = "accident_no"
  ) %>%
  
  left_join(
    person_summary,
    by = "accident_no"
  ) %>%
  
  left_join(
    vehicle_summary,
    by = "accident_no"
  ) %>%
  
  left_join(
    sub_dca_summary,
    by = "accident_no"
  )


# STEP 21: VERIFY INTEGRATION

# Check final number of rows and columns

cat("\nFinal integrated dataset dimensions:\n")

print(dim(integrated_data))

# Compare original and integrated row counts

cat("\nOriginal accident rows:", nrow(accident), "\n")

cat("Integrated dataset rows:", nrow(integrated_data), "\n")

# Check repeated accident numbers

cat(
  "Repeated accident numbers:",
  sum(duplicated(integrated_data$accident_no)),
  "\n"
)

# Stop if the integration increased the number of rows

if (
  nrow(integrated_data) != nrow(accident) ||
  anyDuplicated(integrated_data$accident_no) > 0
) {
  stop("Integration check failed. Check the join keys.")
}


# STEP 22: CHECK FINAL MISSING VALUES

final_missing_summary <- data.frame(
  
  Column = names(integrated_data),
  
  Missing_Values = colSums(is.na(integrated_data)),
  
  Missing_Percentage = round(
    colMeans(is.na(integrated_data)) * 100,
    2
  )
)

# Display columns with missing values

print(
  final_missing_summary %>%
    filter(Missing_Values > 0)
)

# Save final missing value report

write_csv(
  final_missing_summary,
  "data/processed/final_missing_summary.csv"
)


# STEP 23: SAVE INTEGRATED DATASET

write_csv(
  integrated_data,
  "data/processed/integrated_accident_data.csv"
)


# STEP 24: FINAL CONFIRMATION

cat("\n====================================\n")
cat("DATA CLEANING AND INTEGRATION COMPLETED\n")
cat("====================================\n")

cat("Total accident records:", nrow(integrated_data), "\n")

cat("Total columns:", ncol(integrated_data), "\n")

cat(
  "Unique accident numbers:",
  n_distinct(integrated_data$accident_no),
  "\n"
)

cat("\nIntegrated dataset saved successfully.\n")

# Open the integrated dataset in RStudio
View(integrated_data)
