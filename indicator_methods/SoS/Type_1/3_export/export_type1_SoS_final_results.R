# =======================================================================
# Export SoS results to Type 1 Excel files
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script reads Data_Ready CSV files generated from Run_type1_sos.R,
# extracts SoS values (PerTS_ByWeight), and writes them into the
# "Time series information" sheet of the corresponding original Type 1
# Excel files.
#
# One output Excel file is created per dataset.
# The original source files are not overwritten.
# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx
# - dplyr
# - stringr

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

# -----------------------------------------------------------------------
# Clear workspace
# -----------------------------------------------------------------------

rm(list = ls())
gc()

# -----------------------------------------------------------------------
# Select dataset type
# -----------------------------------------------------------------------
# Choose one option:
data_type <- "EPIFAUNA"
# data_type <- "INFAUNA"

# -----------------------------------------------------------------------
# Input and output folders
# -----------------------------------------------------------------------

# Folder containing the Data_Ready CSV outputs from the SoS workflow
input_base_folder <- file.path("../sos_output", data_type)

# Folder containing the original Type 1 Excel files from the repository
source_xlsx_folder <- "../../../data/Type1/species by biomass"

# Folder where updated Excel outputs will be saved
output_base_folder <- file.path("../final_output", data_type)

if(!dir.exists(output_base_folder)){
  dir.create(output_base_folder, recursive = TRUE)
}

# -----------------------------------------------------------------------
# Find dataset folders
# -----------------------------------------------------------------------

dataset_folders <- list.dirs(input_base_folder, full.names = TRUE, recursive = FALSE)

if(length(dataset_folders) == 0){
  stop(paste("No dataset folders found in:", input_base_folder))
}

# -----------------------------------------------------------------------
# Loop through dataset folders
# -----------------------------------------------------------------------

for(dataset_folder in dataset_folders){
  
  file_name <- basename(dataset_folder)
  
  cat("\n----------------------------------------\n")
  cat("Processing dataset:", file_name, "\n")
  
  # ---------------------------------------------------------------------
  # Locate Data_Ready CSV
  # ---------------------------------------------------------------------
  
  data_ready_file <- file.path(
    dataset_folder,
    paste0("Data_Ready_", file_name, ".csv")
  )
  
  if(!file.exists(data_ready_file)){
    warning(paste("Data_Ready file not found for:", file_name))
    next
  }
  
  sos_data <- read.csv(data_ready_file, stringsAsFactors = FALSE)
  
  if(!all(c("station_2", "PerTS_ByWeight") %in% names(sos_data))){
    warning(paste("Required columns not found in:", data_ready_file))
    next
  }
  
  sos_data <- sos_data[, c("station_2", "PerTS_ByWeight")]
  sos_data <- unique(sos_data)
  names(sos_data)[names(sos_data) == "PerTS_ByWeight"] <- "SoS_2026"
  sos_data$station_2 <- str_to_upper(trimws(as.character(sos_data$station_2)))
  
  # ---------------------------------------------------------------------
  # Locate corresponding source Excel file
  # ---------------------------------------------------------------------
  
  source_xlsx_file <- file.path(source_xlsx_folder, paste0(file_name, ".xlsx"))
  
  if(!file.exists(source_xlsx_file)){
    warning(paste("Source Excel file not found for:", file_name))
    next
  }
  
  # ---------------------------------------------------------------------
  # Read "Time series information" sheet
  # ---------------------------------------------------------------------
  
  sheet_names <- excel_sheets(source_xlsx_file)
  
  if(!"Time series information" %in% sheet_names){
    warning(paste("'Time series information' sheet not found in:", source_xlsx_file))
    next
  }
  
  station_info <- read_excel(source_xlsx_file, sheet = "Time series information")
  station_info <- as.data.frame(station_info)
  
  if(!all(c("station", "year") %in% names(station_info))){
    warning(paste("Columns 'station' and/or 'year' not found in:", source_xlsx_file))
    next
  }
  
  station_info <- station_info %>%
    mutate(
      station_2 = paste0(station, "_", year),
      station_2 = str_to_upper(trimws(station_2))
    )
  
  # ---------------------------------------------------------------------
  # Merge SoS values into station information
  # ---------------------------------------------------------------------
  
  station_info_updated <- merge(
    station_info,
    sos_data,
    by = "station_2",
    all.x = TRUE
  )
  
  # ---------------------------------------------------------------------
  # Write updated Excel file
  # ---------------------------------------------------------------------
  
  output_xlsx_file <- file.path(output_base_folder, paste0(file_name, ".xlsx"))
  
  file.copy(source_xlsx_file, output_xlsx_file, overwrite = TRUE)
  
  wb <- loadWorkbook(output_xlsx_file)
  writeData(wb, sheet = "Time series information", x = station_info_updated)
  saveWorkbook(wb, output_xlsx_file, overwrite = TRUE)
  
  cat("Saved:", output_xlsx_file, "\n")
}

