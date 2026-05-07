# =======================================================================
# Prepare Type 2 INFAUNA dataset NS_BfN_grab_btrawling for SoS calculation
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script prepares the Type 2 infauna dataset
# NS_BfN_grab_btrawling.xlsx for subsequent sentinel species selection,
# SoS calculation, and pressure-state analysis.
#
# This dataset requires a dedicated preparation workflow because
# biological records are replicate-level, whereas station-level
# information is reported at station-year level.
#
# Therefore:
# - station-level fields are merged using a station-year key
#   (station_join = station + year)
# - a unique replicate-level identifier is created as:
#   station_2 = station + year + replicate
#
# This ensures that replicate-level biological observations are retained
# while station-level environmental and pressure information is correctly
# linked.

# -----------------------------------------------------------------------
# Main steps
# -----------------------------------------------------------------------
# - Read the Type 2 raw dataset from the repository data folder
# - Load biological, station, and metadata sheets
# - Standardise station identifiers, year, replicate, and biological fields
# - Create station_join (station + year) for merging station-level data
# - Create station_2 (station + year + replicate) as unique replicate ID
# - Harmonise taxonomy using WoRMS (species names from AphiaID)
# - Extract and standardise SAR1 pressure values
# - Merge BESITO sensitivity scores
# - Export one prepared "_data_ready.xlsx" output file

# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
# - Raw Type 2 Excel dataset stored in:
#   ../../../data/Type2/NS_BfN_grab_btrawling.xlsx
# - External BESITO lookup table

# -----------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------
# - One prepared "_data_ready.xlsx" file saved in:
#   ../prepared_data/INFAUNA_DATA/

# -----------------------------------------------------------------------
# Notes
# -----------------------------------------------------------------------
# - This script is intended for a single special-case INFAUNA dataset only
# - WoRMS is used to complete species names from AphiaID when possible
# - BESITO is treated as an external dependency
# - Station identifiers are normalised to ensure BIO/STATION matching
# - Station-level information is merged by station-year, not by replicate

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
# Input and output paths
# -----------------------------------------------------------------------

input_file <- "../../../data/Type2/NS_BfN_grab_btrawling.xlsx"
output_folder <- "../prepared_data/INFAUNA_DATA"
input_file <- file.path(input_folder, "NS_BfN_grab_btrawling.xlsx")

if(!dir.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)
}

# -----------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------

to_num_safe <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "TRUE", "FALSE")] <- NA
  x <- gsub(",", ".", x)
  x <- gsub("[^0-9\\.\\-]", "", x)
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

normalize_rep_id <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  xn <- suppressWarnings(as.integer(x))
  x[!is.na(xn)] <- as.character(abs(xn))
  x
}

# Normalize station codes to match BIO and STATION
# Example: NEP-1, Nep-01 -> NEP-01
normalize_station <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  x <- toupper(x)
  
  out <- x
  m <- regexec("^([A-Z]+)[\\-_]?([0-9]+)$", x)
  reg <- regmatches(x, m)
  ok <- lengths(reg) == 3
  
  if(any(ok, na.rm = TRUE)){
    pref <- sapply(reg[ok], `[`, 2)
    num  <- sapply(reg[ok], `[`, 3)
    num2 <- sprintf("%02d", suppressWarnings(as.integer(num)))
    out[ok] <- paste0(pref, "-", num2)
  }
  
  out
}

build_station_join <- function(station, year){
  paste0(station, "_", year)
}

build_station2_rep <- function(station, year, rep){
  paste0(station, "_", year, "_", rep)
}

# -----------------------------------------------------------------------
# SAR mapping for this dataset
# -----------------------------------------------------------------------

sar1_mapping <- list(
  "NS_BfN_grab_btrawling.xlsx" = "pressure_value"
)

sar1_info <- "Average over the last 1 year prior to the sample"

fill_sar <- function(file_name, mapping, station_tbl, idx){
  out <- rep(NA_real_, length(idx))
  if(file_name %in% names(mapping)){
    col <- mapping[[file_name]]
    col <- tolower(gsub(" ", "_", trimws(col)))
    if(col %in% names(station_tbl)){
      out <- to_num_safe(station_tbl[[col]][idx])
    } else {
      warning(paste("Column", col, "not found in Station information for", file_name))
    }
  }
  out
}

# -----------------------------------------------------------------------
# Load BESITO
# -----------------------------------------------------------------------

besito_file <- "SET_PATH_TO_EXTERNAL_BESITO_INFAUNA_FILE"

besito_raw <- read.csv(besito_file, stringsAsFactors = FALSE)
names(besito_raw) <- trimws(tolower(names(besito_raw)))

if(!all(c("aphiaid","besito") %in% names(besito_raw))){
  stop("BESITO CSV does not contain columns 'aphiaid' and 'besito'.")
}

besito_raw$aphiaid <- to_num_safe(besito_raw$aphiaid)
besito <- besito_raw[!duplicated(besito_raw$aphiaid), c("aphiaid","besito")]

# -----------------------------------------------------------------------
# Identify sheets
# -----------------------------------------------------------------------

file_name <- basename(input_file)
sheets <- excel_sheets(input_file)

bio_sheet <- "Biological information"
station_sheet <- "Station information"
meta_sheet <- "Metadata and protocols"

if(!(bio_sheet %in% sheets)) stop("BIO sheet not found: Biological information")
if(!(station_sheet %in% sheets)) stop("STATION sheet not found: Station information")

cat("Processing file:", file_name, "\n")
cat("BIO sheet:", bio_sheet, "\n")
cat("STATION sheet:", station_sheet, "\n")

# -----------------------------------------------------------------------
# Load BIO
# -----------------------------------------------------------------------

bio_raw <- as.data.frame(read_excel(input_file, sheet = bio_sheet))

# -----------------------------------------------------------------------
# Create standard output table
# -----------------------------------------------------------------------

prepared_df <- data.frame(
  station = NA_character_,
  station_join = NA_character_,
  station_2 = NA_character_,
  year = NA_integer_,
  month = NA_integer_,
  replicate = NA_character_,
  lon = NA_real_,
  lat = NA_real_,
  AphiaID = NA_real_,
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
  BESITO = NA_character_,
  depth = NA_real_,
  gear = NA_character_,
  sediment = NA_character_,
  coverage = NA_real_,
  stringsAsFactors = FALSE
)

prepared_df <- prepared_df[rep(1, nrow(bio_raw)), , drop = FALSE]

# -----------------------------------------------------------------------
# Fill fields from BIO
# -----------------------------------------------------------------------
# Expected BIO columns include:
# station_name, year, replicate, AphiaID, biomass, abundance, coverage

prepared_df$station   <- normalize_station(bio_raw$station_name)
prepared_df$year      <- get_year(bio_raw$year)
prepared_df$replicate <- normalize_rep_id(bio_raw$replicate)

prepared_df$AphiaID   <- to_num_safe(bio_raw$AphiaID)
prepared_df$Biomass   <- to_num_safe(bio_raw$biomass)
prepared_df$Abundance <- to_num_safe(bio_raw$abundance)

if("coverage" %in% names(bio_raw)){
  prepared_df$coverage <- to_num_safe(bio_raw$coverage)
}

# station_join for BIO-STATION merge
prepared_df$station_join <- toupper(build_station_join(prepared_df$station, prepared_df$year))

# unique replicate-level identifier
prepared_df$station_2 <- toupper(build_station2_rep(prepared_df$station, prepared_df$year, prepared_df$replicate))

# -----------------------------------------------------------------------
# Fill Species using WoRMS AphiaID
# -----------------------------------------------------------------------

missing_species_idx <- which(
  !is.na(prepared_df$AphiaID) &
    (is.na(prepared_df$Species) | trimws(as.character(prepared_df$Species)) == "")
)

if(length(missing_species_idx) > 0){
  aphia_ids_missing <- unique(prepared_df$AphiaID[missing_species_idx])
  worms_info <- wormsbyid(aphia_ids_missing)
  
  if(!is.null(worms_info) && nrow(worms_info) > 0){
    aphia_map <- worms_info[, c("AphiaID", "scientificname")]
    colnames(aphia_map) <- c("AphiaID", "Species_new")
    
    prepared_df <- merge(prepared_df, aphia_map, by = "AphiaID", all.x = TRUE, sort = FALSE)
    
    prepared_df$Species <- ifelse(
      is.na(prepared_df$Species) | trimws(as.character(prepared_df$Species)) == "",
      prepared_df$Species_new,
      prepared_df$Species
    )
    
    prepared_df$Species_new <- NULL
  }
}

# -----------------------------------------------------------------------
# Load STATION and merge using station_join (station + year)
# -----------------------------------------------------------------------

station_raw <- as.data.frame(read_excel(input_file, sheet = station_sheet, col_types = "text"))
names(station_raw) <- tolower(gsub(" ", "_", trimws(names(station_raw))))

# Station sheet uses station and year, but station formatting may differ
station_raw$station_clean <- normalize_station(station_raw$station)
station_raw$year_clean <- get_year(station_raw$year)
station_raw$station_join <- toupper(build_station_join(station_raw$station_clean, station_raw$year_clean))

# Convert numeric station-level fields
if("longitude_" %in% names(station_raw)) station_raw$longitude_ <- to_num_safe(station_raw$longitude_)
if("longitude"  %in% names(station_raw)) station_raw$longitude  <- to_num_safe(station_raw$longitude)
if("latitude"   %in% names(station_raw)) station_raw$latitude   <- to_num_safe(station_raw$latitude)
if("depth"      %in% names(station_raw)) station_raw$depth      <- to_num_safe(station_raw$depth)
if("pressure_value" %in% names(station_raw)) station_raw$pressure_value <- to_num_safe(station_raw$pressure_value)
if("total_biomass" %in% names(station_raw)) station_raw$total_biomass <- to_num_safe(station_raw$total_biomass)
if("total_abundance" %in% names(station_raw)) station_raw$total_abundance <- to_num_safe(station_raw$total_abundance)

# Match BIO rows (replicate-level) to STATION rows (station-year level)
idx <- match(prepared_df$station_join, station_raw$station_join)

cat("Match success by station_join (%):", round(mean(!is.na(idx)) * 100, 2), "\n")

# -----------------------------------------------------------------------
# Fill station-level fields
# -----------------------------------------------------------------------

if("longitude_" %in% names(station_raw)) prepared_df$lon <- station_raw$longitude_[idx]
if("longitude"  %in% names(station_raw) && all(is.na(prepared_df$lon))) prepared_df$lon <- station_raw$longitude[idx]
if("latitude"   %in% names(station_raw)) prepared_df$lat <- station_raw$latitude[idx]

if("month" %in% names(station_raw)) prepared_df$month <- suppressWarnings(as.integer(station_raw$month[idx]))
if("depth" %in% names(station_raw)) prepared_df$depth <- station_raw$depth[idx]
if("gear"  %in% names(station_raw)) prepared_df$gear  <- station_raw$gear[idx]

if("total_biomass" %in% names(station_raw)) prepared_df$Total_biomass <- station_raw$total_biomass[idx]
if("total_abundance" %in% names(station_raw)) prepared_df$Total_abundance <- station_raw$total_abundance[idx]
if("habitat_type" %in% names(station_raw)) prepared_df$MSFD_broad_Ch <- station_raw$habitat_type[idx]

prepared_df$SAR1 <- fill_sar(file_name, sar1_mapping, station_raw, idx)
prepared_df$info_SAR1 <- sar1_info

# -----------------------------------------------------------------------
# Read Biomass / Abundance units from Metadata
# -----------------------------------------------------------------------

biomass_units <- NA_character_
abundance_units <- NA_character_

if(meta_sheet %in% sheets){
  meta <- as.data.frame(read_excel(input_file, sheet = meta_sheet, col_names = FALSE))
  colnames(meta) <- paste0("col", seq_len(ncol(meta)))
  meta$col1_low <- tolower(trimws(as.character(meta$col1)))
  
  idx_biomass <- which(
    grepl("^biomass$", meta$col1_low) |
      (grepl("biomass", meta$col1_low) & !grepl("total", meta$col1_low))
  )
  if(length(idx_biomass) > 0){
    biomass_units <- as.character(meta$col2[idx_biomass[1]])
    if(toupper(biomass_units) %in% c("NA", "")) biomass_units <- NA
  }
  
  idx_abundance <- which(
    grepl("^abundance$", meta$col1_low) |
      (grepl("abund", meta$col1_low) & !grepl("total", meta$col1_low))
  )
  if(length(idx_abundance) > 0){
    abundance_units <- as.character(meta$col2[idx_abundance[1]])
    if(toupper(abundance_units) %in% c("NA", "")) abundance_units <- NA
  }
}

prepared_df$Biomass_units <- biomass_units
prepared_df$Abundance_units <- abundance_units

# -----------------------------------------------------------------------
# Merge BESITO
# -----------------------------------------------------------------------

prepared_df <- merge(
  prepared_df,
  besito,
  by.x = "AphiaID",
  by.y = "aphiaid",
  all.x = TRUE,
  sort = FALSE
)

names(prepared_df)[names(prepared_df) == "besito"] <- "BESITO"

prepared_df$BESITO <- ifelse(
  is.na(prepared_df$BESITO) | trimws(as.character(prepared_df$BESITO)) == "",
  "1",
  as.character(prepared_df$BESITO)
)

# -----------------------------------------------------------------------
# Force final column order
# -----------------------------------------------------------------------

final_col_order <- c(
  "station",
  "station_2",
  "station_join",
  "year",
  "month",
  "replicate",
  "lon",
  "lat",
  "depth",
  "gear",
  "AphiaID",
  "Species",
  "BESITO",
  "Biomass",
  "Biomass_units",
  "Abundance",
  "Abundance_units",
  "Total_biomass",
  "Total_abundance",
  "MSFD_broad_Ch",
  "SAR1",
  "info_SAR1",
  "coverage"
)

prepared_df <- prepared_df[, final_col_order[final_col_order %in% names(prepared_df)], drop = FALSE]

# -----------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------

output_file <- file.path(output_folder, "NS_BfN_grab_btrawling_data_ready.xlsx")
write.xlsx(prepared_df, output_file, rowNames = FALSE)

cat("Saved:", output_file, "\n")
cat("Finished:", file_name, "\n")
