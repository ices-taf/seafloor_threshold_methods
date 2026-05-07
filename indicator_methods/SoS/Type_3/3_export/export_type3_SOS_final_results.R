# =======================================================================
# Export SoS and GAM significance results to Type 3 Excel files
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script reads habitat-specific Data_Ready CSV files generated from
# the Type 3 SoS workflow, extracts SoS values and GAM significance
# information, and writes them into the "Station information" sheet of
# the corresponding original Type 3 Excel file.
#
# For each habitat-specific Data_Ready file, the script extracts:
#   - station identifier
#   - PerTS_ByWeight, exported as SoS_2026
#   - gam_significant
#
# It also creates a descriptive GAM relationship column:
#   - "significant"     if gam_significant = 1
#   - "not_significant" if gam_significant = 0
#
# One output Excel file is created per dataset.
# The original source file is not overwritten.
#
# -----------------------------------------------------------------------
# NOTE ON STATION IDENTIFIERS
# -----------------------------------------------------------------------
# Most datasets use the column "station" as the haul identifier.
# However, some datasets (e.g. NS_BE and NS_NL) contain both "station"
# and "station_2", and in those cases "station_2" should be used as the
# correct identifier for export.
#
# To handle this, manually set `use_station_2 <- TRUE` for those
# datasets. Otherwise, "station" is used by default.
#
# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(openxlsx)

# -----------------------------------------------------------------------
# User settings
# -----------------------------------------------------------------------

# Dataset name should be provided WITHOUT file extension
dataset_name <- "BoBIC_IberianChabitats"

# Set to TRUE only for datasets where station_2 must be used instead of
# station (e.g. NS_BE, NS_NL)
use_station_2 <- FALSE

# Folder containing original Type 3 Excel files
input_folder <- "../../../data/Type3"

# Folder containing habitat-specific Data_Ready CSV files
data_ready_folder <- file.path(
  "../SoS/Type3/sos_output/EPIFAUNA",
  dataset_name
)

# Output folder for exported Excel files
output_folder <- "../../SoS/Type3/final_output"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Input Excel file
input_xlsx <- file.path(input_folder, paste0(dataset_name, ".xlsx"))

# Output Excel file
output_xlsx <- file.path(output_folder, paste0(dataset_name, "_SoS_export.xlsx"))

# -----------------------------------------------------------------------
# Check input paths
# -----------------------------------------------------------------------

if (!file.exists(input_xlsx)) {
  stop(paste("Input Excel file not found:", input_xlsx))
}

if (!dir.exists(data_ready_folder)) {
  stop(paste("Data_Ready folder not found:", data_ready_folder))
}

# -----------------------------------------------------------------------
# Read habitat-specific Data_Ready CSV files
# -----------------------------------------------------------------------

csv_files <- list.files(
  path = data_ready_folder,
  pattern = "^Data_Ready_.*\\.csv$",
  full.names = TRUE
)

if (length(csv_files) == 0) {
  stop(paste("No Data_Ready CSV files found in:", data_ready_folder))
}

data_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE)

# -----------------------------------------------------------------------
# Select correct station identifier when needed
# -----------------------------------------------------------------------

if (use_station_2) {
  data_list <- lapply(data_list, function(df) {
    if (!"station_2" %in% names(df)) {
      stop("Column 'station_2' not found, but use_station_2 = TRUE.")
    }
    df$station <- df$station_2
    return(df)
  })
}

# -----------------------------------------------------------------------
# Combine all habitat-specific outputs
# -----------------------------------------------------------------------

all_data <- do.call(rbind, data_list)

# Keep only required columns
required_cols <- c("station", "PerTS_ByWeight", "gam_significant")
missing_cols <- setdiff(required_cols, names(all_data))

if (length(missing_cols) > 0) {
  stop(
    paste(
      "The following required columns are missing from the Data_Ready files:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

all_data <- all_data[, required_cols]

# Rename SoS column
names(all_data)[names(all_data) == "PerTS_ByWeight"] <- "SoS_2026"

# Create descriptive GAM relationship column
all_data$gam_relationship <- ifelse(
  all_data$gam_significant == 1,
  "significant",
  "not_significant"
)

# Remove duplicated rows if present
all_data <- unique(all_data)

# -----------------------------------------------------------------------
# Read Station information sheet from original Excel file
# -----------------------------------------------------------------------
# # Check the exact name of the sheet in the Excel file before reading it.
# It may be "Station information", "station_information", or another variant
# depending on the dataset. Use excel_sheets(input_xlsx) to verify. 
station_info <- read_excel(input_xlsx, sheet = "Station information")

# Remove previous export columns if already present
cols_to_replace <- c("SoS_2026", "gam_significant", "gam_relationship")
station_info <- station_info[, !(names(station_info) %in% cols_to_replace)]

# -----------------------------------------------------------------------
# Merge SoS information into Station information
# -----------------------------------------------------------------------

station_info_updated <- merge(
  station_info,
  all_data,
  by = "station",
  all.x = TRUE
)

# -----------------------------------------------------------------------
# Write updated Station information into a new Excel file
# -----------------------------------------------------------------------

wb <- loadWorkbook(input_xlsx)

writeData(
  wb,
  sheet = "Station information",
  x = station_info_updated
)

saveWorkbook(wb, output_xlsx, overwrite = TRUE)

# -----------------------------------------------------------------------
# Final message
# -----------------------------------------------------------------------

cat("------------------------------------------------------------\n")
cat("Type 3 export completed successfully.\n")
cat("Dataset:", dataset_name, "\n")
cat("Input Excel file:\n", input_xlsx, "\n")
cat("Data_Ready folder:\n", data_ready_folder, "\n")
cat("Output Excel file:\n", output_xlsx, "\n")
cat("Number of Data_Ready files processed:", length(csv_files), "\n")
cat("Done.\n")
