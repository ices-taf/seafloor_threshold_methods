# =======================================================================
# Prepare Type 2 INFAUNA datasets with sand extraction and trawling for SoS calculation
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script prepares a subset of Type 2 infauna datasets affected by
# two cumulative pressures: sand extraction and bottom trawling.
#
# It is designed for the following datasets only:
# - NS_Hinderbanken_sandextr_and_btrawling.xlsx
# - NS_Oostdyck_sandextr_and_btrawling.xlsx
# - NS_Thornton_sandextr_and_btrawling.xlsx
#
# Biological and station-level information are merged using:
#   station_2 = station + year
#
# Replicate information is retained as descriptive information when
# available, but it is not used in the merge key.
#
# Two pressure variables are extracted from the station sheet:
# - EXTR_sand = pressure_value1 (sand extraction pressure)
# - SAR1      = pressure_value2 (bottom trawling pressure)

# -----------------------------------------------------------------------
# Main steps
# -----------------------------------------------------------------------
# - Read the selected Type 2 raw datasets from the repository data folder
# - Detect biological, station, and metadata sheets
# - Standardise station, year, taxonomic, and biological variables
# - Build station_2 = station + year as the join key
# - Harmonise taxonomy using WoRMS (AphiaID and species names)
# - Merge station-level information into biological records
# - Extract and standardise two pressure metrics:
#     * EXTR_sand from pressure_value1
#     * SAR1 from pressure_value2
# - Merge BESITO sensitivity scores
# - Export one prepared "_data_ready.xlsx" file per input dataset

# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
# - Raw Type 2 Excel datasets stored in:
#   ../../../data/Type2/
# - External BESITO lookup table

# -----------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------
# - One prepared "_data_ready.xlsx" file per input dataset saved in:
#   ../prepared_data/INFAUNA_DATA/

# -----------------------------------------------------------------------
# Notes
# -----------------------------------------------------------------------
# - This script is intended for a specific subset of Type 2 INFAUNA
#   datasets only
# - WoRMS is used to complete missing AphiaID or species names when possible
# - BESITO is treated as an external dependency
# - station_2 is defined as station + year
# - Replicate is retained as information only and is not part of the join key
# - pressure_value1 and pressure_value2 are interpreted as distinct
#   cumulative pressures and exported separately

# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx
# - worms

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(worms)

# -----------------------------------------------------------------------
# Clear workspace
# -----------------------------------------------------------------------

rm(list = ls())
gc()

# -----------------------------------------------------------------------
# Input and output folders
# -----------------------------------------------------------------------

input_folder  <- "../../../data/Type2"
output_folder <- "../prepared_data/INFAUNA_DATA"

if(!dir.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)
}


# -----------------------------------------------------------------------
# Keep only the target files
# -----------------------------------------------------------------------

target_files <- c(
  "NS_Hinderbanken_sandextr_and_btrawling.xlsx",
  "NS_Oostdyck_sandextr_and_btrawling.xlsx",
  "NS_Thornton_sandextr_and_btrawling.xlsx"
)

all_files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)
all_files <- all_files[!grepl("^~\\$", basename(all_files))]
all_files <- all_files[basename(all_files) %in% target_files]

cat("Files to process:\n")
print(basename(all_files))

# ----------------------------
# Helpers
# ----------------------------
assign_column <- function(target_col, possible_names, df_raw, df_prepared, convert_fun = identity){
  col_name <- names(df_raw)[tolower(trimws(names(df_raw))) %in% tolower(trimws(possible_names))]
  if(length(col_name) == 1){
    df_prepared[[target_col]] <- convert_fun(df_raw[[col_name]])
  }
  df_prepared
}

to_num_safe <- function(x){
  # si ya viene como número, devuélvelo tal cual
  if(is.numeric(x)) return(as.numeric(x))
  
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "TRUE", "FALSE")] <- NA
  
  # coma decimal
  x <- gsub(",", ".", x, fixed = TRUE)

  
  suppressWarnings(as.numeric(x))
}

get_year <- function(x){
  if(inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")){
    return(as.integer(format(x, "%Y")))
  }
  x_chr <- trimws(as.character(x))
  y <- suppressWarnings(as.integer(substr(x_chr, 1, 4)))
  y2 <- suppressWarnings(as.integer(x_chr))
  y[is.na(y)] <- y2[is.na(y)]
  y
}

normalize_station <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  toupper(x)
}

build_station2 <- function(station, year){
  paste0(normalize_station(station), "_", trimws(as.character(year)))
}

# -----------------------------------------------------------------------
# Possible station sheet names
# -----------------------------------------------------------------------

possible_station_sheets <- c(
  "Station information",
  "station information",
  "Station info",
  "station info",
  "Station_information",
  "station_information",
  "Station_Information"
)

# -----------------------------------------------------------------------
# Load BESITO
# -----------------------------------------------------------------------

besito_file <- "C:/Users/User/Dropbox/0_IEO/Ciencia/ICES/WKBENTH4/1_INPUT_DATA/BESITO/FINALES/Epifauna&Infauna_BESITO_2026.csv"

besito_raw <- read.csv(besito_file, stringsAsFactors = FALSE)
names(besito_raw) <- trimws(tolower(names(besito_raw)))

if(!all(c("aphiaid","besito") %in% names(besito_raw))){
  stop("BESITO CSV must contain columns 'aphiaid' and 'besito'.")
}

besito_raw$aphiaid <- to_num_safe(besito_raw$aphiaid)
besito <- besito_raw[!duplicated(besito_raw$aphiaid), c("aphiaid","besito")]

# -----------------------------------------------------------------------
# Loop through files
# -----------------------------------------------------------------------

for(file_path in all_files){
  
  file_name <- basename(file_path)
  cat("\nProcessing file:", file_name, "\n")
  
  # ---------------------------------------------------------------------
  # Identify biological sheet
  # ---------------------------------------------------------------------
  sheets <- excel_sheets(file_path)
  bio_sheet <- sheets[sheets %in% c(
    "Biological information",
    "Biological info",
    "Biological_information",
    "Biological_Information"
  )]
  
  if(length(bio_sheet) == 0){
    stop("No biological information sheet found in file.")
  }
  
  # ---------------------------------------------------------------------
  # Load biological information
  # ---------------------------------------------------------------------
  bio_raw <- read_excel(file_path, sheet = bio_sheet[1])
  bio_raw <- as.data.frame(bio_raw)
  
  # ---------------------------------------------------------------------
  # Create standard table
  # ---------------------------------------------------------------------
  prepared_df <- data.frame(
    station = NA_character_,
    station_2 = NA_character_,
    year = NA_integer_,
    month = NA_integer_,
    replicate = NA_character_,
    lon = NA_real_,
    lat = NA_real_,
    AphiaID = NA_real_,
    TaxCode = NA_character_,
    Species = NA_character_,
    Biomass = NA_real_,
    Biomass_units = NA_character_,
    Abundance = NA_real_,
    Abundance_units = NA_character_,
    Total_biomass = NA_real_,
    Total_abundance = NA_real_,
    MSFD_broad_Ch = NA_character_,
    SAR1 = NA_real_,
    info_SAR1 = NA_character_,
    EXTR_sand = NA_real_,
    info_EXTR_sand = NA_character_,
    BESITO = NA_character_,
    depth = NA_real_,
    gear = NA_character_,
    sediment = NA_character_,
    stringsAsFactors = FALSE
  )
  
  prepared_df <- prepared_df[rep(1, nrow(bio_raw)), , drop = FALSE]
  
  # ---------------------------------------------------------------------
  # Fill columns from BIO
  # ---------------------------------------------------------------------
  prepared_df <- assign_column("station", c("station","Station","station_name"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("year", c("year","Year","survey_year","surveyyear","yr"), bio_raw, prepared_df, get_year)
  prepared_df <- assign_column("month", c("month","Month"), bio_raw, prepared_df, as.integer)
  prepared_df <- assign_column("replicate", c("replicate","Replicate","replicates","Replicates","rep","repl"), bio_raw, prepared_df, as.character)
  
  prepared_df <- assign_column("lon", c("lon","Longitude","long","longitude_shooting"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("lat", c("lat","Latitude","latitude_hauling"), bio_raw, prepared_df, to_num_safe)
  
  prepared_df <- assign_column("AphiaID", c("AphiaID","aphiaid"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("TaxCode", c("TaxCode","taxcode"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Species", c("Species","species"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Biomass", c("biomass","Biomass"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("Abundance", c("abundance","Abundance"), bio_raw, prepared_df, to_num_safe)
  
  prepared_df <- assign_column("MSFD_broad_Ch", c("MSFD_broad_Ch","MSFD_BBHT","Habitat_type_MSFD","habitat_type_msfd"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("depth", c("depth","Depth"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("gear", c("gear","Gear"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("sediment", c("sediment","Sediment"), bio_raw, prepared_df, as.character)
  
  # ---------------------------------------------------------------------
  # Build station_2 = station + year
  # ---------------------------------------------------------------------
  prepared_df$station <- normalize_station(prepared_df$station)
  prepared_df$station_2 <- toupper(build_station2(prepared_df$station, prepared_df$year))
  
  # ---------------------------------------------------------------------
  # Fill missing AphiaID from TaxCode using WoRMS
  # ---------------------------------------------------------------------
  missing_idx <- which(is.na(prepared_df$AphiaID) & !is.na(prepared_df$TaxCode) & trimws(prepared_df$TaxCode) != "")
  taxnames_missing <- unique(prepared_df$TaxCode[missing_idx])
  
  if(length(taxnames_missing) > 0){
    worms_results <- wormsbymatchnames(taxnames_missing)
    if(!is.null(worms_results) && nrow(worms_results) > 0){
      aphia_map <- worms_results[, c("scientificname", "AphiaID")]
      colnames(aphia_map) <- c("TaxCode", "AphiaID_new")
      prepared_df <- merge(prepared_df, aphia_map, by = "TaxCode", all.x = TRUE, sort = FALSE)
      prepared_df$AphiaID <- ifelse(is.na(prepared_df$AphiaID), prepared_df$AphiaID_new, prepared_df$AphiaID)
      prepared_df$AphiaID_new <- NULL
    }
  }
  
  # ---------------------------------------------------------------------
  # Fill Species using WoRMS AphiaID
  # ---------------------------------------------------------------------
  missing_species_idx <- which(!is.na(prepared_df$AphiaID) & (is.na(prepared_df$Species) | trimws(prepared_df$Species) == ""))
  
  if(length(missing_species_idx) > 0){
    aphia_ids_missing <- unique(prepared_df$AphiaID[missing_species_idx])
    worms_info <- wormsbyid(aphia_ids_missing)
    
    if(!is.null(worms_info) && nrow(worms_info) > 0){
      aphia_map <- worms_info[, c("AphiaID", "scientificname")]
      colnames(aphia_map) <- c("AphiaID", "Species_new")
      prepared_df <- merge(prepared_df, aphia_map, by = "AphiaID", all.x = TRUE, sort = FALSE)
      prepared_df$Species <- ifelse(
        is.na(prepared_df$Species) | trimws(prepared_df$Species) == "",
        prepared_df$Species_new,
        prepared_df$Species
      )
      prepared_df$Species_new <- NULL
    }
  }
  
  # ---------------------------------------------------------------------
  # Load station information and merge by station_2
  # ---------------------------------------------------------------------
  station_sheet <- sheets[sheets %in% possible_station_sheets]
  
  if(length(station_sheet) == 0){
    warning("No Station Information sheet found. Station-related columns will remain NA.")
  } else {
   
    station_raw <- read_excel(file_path, sheet = station_sheet[1], col_types = "text")
    station_raw <- as.data.frame(station_raw)
    names(station_raw) <- tolower(gsub(" ", "_", names(station_raw)))
    
    if(!("station" %in% names(station_raw))){
      warning("Station column not found in station sheet; cannot merge pressures or coordinates.")
    } else if(!("year" %in% names(station_raw))){
      warning("Year column not found in station sheet; cannot build station_2 for matching.")
    } else {
      
      station_raw$station <- normalize_station(station_raw$station)
      station_raw$year <- get_year(station_raw$year)
      station_raw$station_2 <- toupper(build_station2(station_raw$station, station_raw$year))
      
      
    
      
      
      
      
      idx <- match(prepared_df$station_2, station_raw$station_2)
      cat("Match success (%):", round(mean(!is.na(idx)) * 100, 2), "\n")
      
      # Sand extraction pressure
      if("pressure_value1" %in% names(station_raw)){
        prepared_df$EXTR_sand <- to_num_safe(station_raw$pressure_value1[idx])
        prepared_df$info_EXTR_sand <- "Sand extraction volume (pressure_value1)"
      } else {
        warning("pressure_value1 not found in station sheet (expected for EXTR_sand).")
      }
      
      # Bottom trawling pressure
      if("pressure_value2" %in% names(station_raw)){
        prepared_df$SAR1 <- to_num_safe(station_raw$pressure_value2[idx])
        prepared_df$info_SAR1 <- "Average over the last 1 year prior to the sample (pressure_value2)"
      } else {
        warning("pressure_value2 not found in station sheet (expected for SAR1).")
      }
      
      # Other station-level fields
      longitude_cols <- c("longitude","lon","long","longitude_shooting")
      latitude_cols  <- c("latitude","lat","latitude_hauling")
      
      lon_col <- longitude_cols[longitude_cols %in% names(station_raw)]
      lat_col <- latitude_cols[latitude_cols %in% names(station_raw)]
      
      if(length(lon_col) > 0) prepared_df$lon <- to_num_safe(station_raw[[lon_col[1]]][idx])
      if(length(lat_col) > 0) prepared_df$lat <- to_num_safe(station_raw[[lat_col[1]]][idx])
      
      if("gear" %in% names(station_raw)) prepared_df$gear <- station_raw$gear[idx]
      if("month" %in% names(station_raw)) prepared_df$month <- suppressWarnings(as.integer(station_raw$month[idx]))
      if("depth" %in% names(station_raw)) prepared_df$depth <- to_num_safe(station_raw$depth[idx])
      
      if("total_biomass" %in% names(station_raw)) prepared_df$Total_biomass <- to_num_safe(station_raw$total_biomass[idx])
      if("total_abundance" %in% names(station_raw)) prepared_df$Total_abundance <- to_num_safe(station_raw$total_abundance[idx])
      
      msfd_cols <- c("habitat_type","msfd_broad_ch","msfd_bbht","habitat_type_msfd")
      msfd_col <- msfd_cols[msfd_cols %in% names(station_raw)]
      if(length(msfd_col) > 0) prepared_df$MSFD_broad_Ch <- station_raw[[msfd_col[1]]][idx]
      
      if("sediment" %in% names(station_raw)) prepared_df$sediment <- station_raw$sediment[idx]
    }
  }
  
  # ---------------------------------------------------------------------
  # Read Biomass / Abundance units from Metadata
  # ---------------------------------------------------------------------
  possible_meta_sheets <- c(
    "Metadata and protocols",
    "Metadata_and_protocols",
    "Metadata_and_Protocols"
  )
  
  meta_sheet <- sheets[sheets %in% possible_meta_sheets]
  biomass_units <- NA_character_
  abundance_units <- NA_character_
  
  if(length(meta_sheet) > 0){
    meta <- read_excel(file_path, sheet = meta_sheet[1], col_names = FALSE)
    meta <- as.data.frame(meta)
    colnames(meta) <- paste0("col", seq_len(ncol(meta)))
    meta$col1_low <- tolower(trimws(as.character(meta$col1)))
    
    idx_biomass <- which(
      grepl("^biomass$", meta$col1_low) |
        (grepl("biomass", meta$col1_low) & !grepl("total", meta$col1_low))
    )
    if(length(idx_biomass) > 0){
      biomass_units <- as.character(meta$col2[idx_biomass[1]])
      if(toupper(biomass_units) %in% c("NA","")) biomass_units <- NA
    }
    
    idx_abundance <- which(
      grepl("^abundance$", meta$col1_low) |
        (grepl("abund", meta$col1_low) & !grepl("total", meta$col1_low))
    )
    if(length(idx_abundance) > 0){
      abundance_units <- as.character(meta$col2[idx_abundance[1]])
      if(toupper(abundance_units) %in% c("NA","")) abundance_units <- NA
    }
  }
  
  prepared_df$Biomass_units <- biomass_units
  prepared_df$Abundance_units <- abundance_units
  
  # ---------------------------------------------------------------------
  # Merge BESITO by AphiaID
  # ---------------------------------------------------------------------
  prepared_df$BESITO <- NULL
  prepared_df <- merge(prepared_df, besito, by.x = "AphiaID", by.y = "aphiaid", all.x = TRUE, sort = FALSE)
  names(prepared_df)[names(prepared_df) == "besito"] <- "BESITO"
  
  prepared_df$BESITO <- ifelse(
    is.na(prepared_df$BESITO) | trimws(as.character(prepared_df$BESITO)) == "",
    "1",
    as.character(prepared_df$BESITO)
  )
  
  # ---------------------------------------------------------------------
  # Force final column order
  # ---------------------------------------------------------------------
  final_col_order <- c(
    "station",
    "station_2",
    "year",
    "month",
    "depth",
    "gear",
    "replicate",
    "lon",
    "lat",
    "AphiaID",
    "Species",
    "BESITO",
    "TaxCode",
    "Biomass",
    "Biomass_units",
    "Abundance",
    "Abundance_units",
    "Total_biomass",
    "Total_abundance",
    "MSFD_broad_Ch",
    "sediment",
    "EXTR_sand",
    "info_EXTR_sand",
    "SAR1",
    "info_SAR1"
  )
  
  prepared_df <- prepared_df[, final_col_order[final_col_order %in% names(prepared_df)], drop = FALSE]
  
  prepared_df_export <- prepared_df
  if("TaxCode" %in% names(prepared_df_export)) prepared_df_export$TaxCode <- NULL
  
  # ---------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------
  output_file <- file.path(
    output_folder,
    paste0(tools::file_path_sans_ext(file_name), "_data_ready.xlsx")
  )
  
  write.xlsx(prepared_df_export, output_file, rowNames = FALSE)
  
  cat("Finished:", file_name, "\n")
  cat("Saved:", output_file, "\n")
}