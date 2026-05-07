# =======================================================================
# Prepare Type 2 INFAUNA dataset BS_southernbaltic_oxygendepletion
# for SoS calculation
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script prepares the Type 2 infauna dataset
# BS_southernbaltic_oxygendepletion.xlsx for subsequent sentinel species
# selection, SoS calculation, and pressure-state analysis.
#
# This dataset requires a dedicated preparation workflow because:
# - the pressure variable is oxygen depletion, not bottom trawling SAR;
# - sensitivity scores are not taken directly from BESITO;
# - AMBI ecological groups are first converted into BESITO-like scores.
#
# AMBI to BESITO-like conversion used here:
# - AMBI groups IV-V   -> ambi_besito = 1
# - AMBI groups II-III -> ambi_besito = 2
# - AMBI group I       -> ambi_besito = 3
# - missing AMBI match -> ambi_besito = 1
#
# The internal sensitivity column is named ambi_besito for transparency.
# At export, a BESITO column is also created from ambi_besito to keep
# compatibility with downstream SoS scripts.

# -----------------------------------------------------------------------
# Main steps
# -----------------------------------------------------------------------
# - Read the Type 2 raw dataset from the repository data folder
# - Extract unique AphiaIDs from the biological sheet
# - Use WoRMS to retrieve accepted/scientific species names from AphiaID
# - Match species names against the AMBI-BESITO external lookup table
# - Convert AMBI groups into BESITO-like sensitivity scores
# - Load biological and station sheets
# - Standardise station identifiers, year, replicate, and biological fields
# - Create station_2 as station + year
# - Merge station-level information using station_2
# - Extract and standardise oxygen depletion pressure values into SAR1
# - Merge ambi_besito sensitivity scores by AphiaID
# - Export one prepared "_data_ready.xlsx" output file
# - Export one AMBI_BESITO coverage summary file

# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
# - Raw Type 2 Excel dataset stored in:
#   ../../../data/Type2/BS_southernbaltic_oxygendepletion.xlsx
# - External AMBI lookup table \(not included in the repository\).\n
#   Users must provide the local path to this file in the `ambi_file` object.


# -----------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------
# - One prepared "_data_ready.xlsx" file saved in:
#   ../prepared_data/INFAUNA_DATA/
# 
# - One intermediate ambi_sensi_besito.csv file 

# -----------------------------------------------------------------------
# Notes
# -----------------------------------------------------------------------
# - This script is intended for a single special-case INFAUNA dataset only
# - Oxygen depletion is stored in SAR1 for compatibility with downstream scripts
# - info_SAR1 explicitly identifies the pressure as oxygen depletion
# - Station-level information is merged by station-year
# - Missing AMBI matches are conservatively assigned ambi_besito = 1
# - BESITO in the final output is derived from ambi_besito

# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - readr
# - openxlsx
# - worms

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(readr)
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

input_file <- "../../../data/Type2/BS_southernbaltic_oxygendepletion.xlsx"
ambi_file  <- "SET_PATH_TO_EXTERNAL_AMBI_FILE"

output_folder <- "../prepared_data/INFAUNA_DATA"

if(!dir.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)
}

ambi_besito_file <- file.path(output_folder, "ambi_sensi_besito.csv")
missing_match_file <- file.path(output_folder, "needed_sin_match.csv")

# -----------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------

find_col <- function(df, candidates){
  nms <- names(df)
  nms_low <- tolower(trimws(nms))
  cand_low <- tolower(trimws(candidates))
  hit <- match(cand_low, nms_low)
  hit <- hit[!is.na(hit)]
  if(length(hit) == 0) return(NA_character_)
  nms[hit[1]]
}

assign_column <- function(target_col, possible_names, df_raw, df_prepared, convert_fun = identity){
  col_name <- names(df_raw)[tolower(names(df_raw)) %in% tolower(possible_names)]
  if(length(col_name) == 1){
    df_prepared[[target_col]] <- convert_fun(df_raw[[col_name]])
  }
  df_prepared
}

to_num_safe <- function(x){
  if(is.numeric(x)) return(as.numeric(x))
  
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "TRUE", "FALSE")] <- NA
  if(all(is.na(x))) return(as.numeric(x))
  
  x <- gsub("[^0-9eE\\+\\-\\,\\.]", "", x)
  
  out <- vapply(x, function(s){
    if(is.na(s) || s == "") return(NA_real_)
    
    if(grepl("[eE]", s)){
      return(suppressWarnings(as.numeric(s)))
    }
    
    has_comma <- grepl(",", s, fixed = TRUE)
    has_dot   <- grepl("\\.", s)
    
    if(has_comma && has_dot){
      last_comma <- max(gregexpr(",", s, fixed = TRUE)[[1]])
      last_dot   <- max(gregexpr("\\.", s)[[1]])
      
      if(last_comma > last_dot){
        s <- gsub("\\.", "", s)
        s <- sub(",", ".", s, fixed = TRUE)
      } else {
        s <- gsub(",", "", s, fixed = TRUE)
      }
      
    } else if(has_comma && !has_dot){
      s <- sub(",", ".", s, fixed = TRUE)
    }
    
    suppressWarnings(as.numeric(s))
  }, numeric(1))
  
  out
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
  x <- toupper(x)
  x
}

normalize_rep_id <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  toupper(x)
}

build_station2 <- function(station, year){
  st <- trimws(as.character(station))
  yr <- trimws(as.character(year))
  paste(st, yr, sep = "_")
}

norm_key <- function(x){
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- trimws(gsub("\\s+", " ", x))
  x <- tolower(x)
  x <- gsub("\\bsp\\.?\\b", "", x)
  x <- gsub("\\bspp\\.?\\b", "", x)
  x <- gsub("\\.", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

# -----------------------------------------------------------------------
# Sheet detection
# -----------------------------------------------------------------------

detect_bio_sheet <- function(sheets){
  bio <- sheets[sheets %in% c("BS_southernbaltic_oxygendepleti")]
  if(length(bio) == 0) return(NA_character_)
  bio[1]
}

detect_station_sheet <- function(sheets, bio_sheet){
  possible_station_sheets <- c(
    "Station information", "station information",
    "Station info", "station info",
    "Station_information", "station_information", "Station_Information",
    "Station raw", "Station_raw", "Station", "Stations"
  )
  st <- sheets[sheets %in% possible_station_sheets]
  if(length(st) > 0) return(st[1])
  rest <- setdiff(sheets, bio_sheet)
  if(length(rest) == 0) return(NA_character_)
  rest[1]
}

# -----------------------------------------------------------------------
# SAR / pressure mapping for this dataset
# -----------------------------------------------------------------------

sar1_mapping <- list(
  "BS_southernbaltic_oxygendepletion.xlsx" = "pressure_value"
)

sar1_info <- "oxygen depletion"

fill_sar <- function(file_name, mapping, station_tbl, idx){
  out <- rep(NA_real_, length(idx))
  
  if(file_name %in% names(mapping)){
    col <- mapping[[file_name]]
    col <- tolower(gsub(" ", "_", trimws(col)))
    
    if(col %in% names(station_tbl)){
      out <- to_num_safe(station_tbl[[col]][idx])
    } else {
      warning(paste("Column", col, "not found in STATION sheet for", file_name))
    }
  }
  
  out
}

# =======================================================================
# PART 1. Create AMBI -> BESITO-like lookup for this dataset
# =======================================================================

file_name <- basename(input_file)
sheets <- excel_sheets(input_file)

bio_sheet <- detect_bio_sheet(sheets)
if(is.na(bio_sheet)) stop("No biological information sheet found in file.")

station_sheet <- detect_station_sheet(sheets, bio_sheet)
if(is.na(station_sheet)) warning("No station sheet found. Station fields will remain NA.")

cat("Processing file:", file_name, "\n")
cat("BIO sheet:", bio_sheet, "\n")
cat("STATION sheet:", station_sheet, "\n")

bio_raw <- as.data.frame(read_excel(input_file, sheet = bio_sheet))
names(bio_raw) <- trimws(names(bio_raw))

aphia_col <- find_col(bio_raw, c("AphiaID", "aphiaid"))
if(is.na(aphia_col)) stop("AphiaID column not found in BIO sheet.")

aphia_ids <- unique(to_num_safe(bio_raw[[aphia_col]]))
aphia_ids <- aphia_ids[!is.na(aphia_ids)]

cat("Unique AphiaIDs found:", length(aphia_ids), "\n")

worms_info <- wormsbyid(aphia_ids)

if(is.null(worms_info) || nrow(worms_info) == 0){
  stop("WoRMS did not return species information for the AphiaIDs.")
}

needed <- data.frame(
  aphiaid = to_num_safe(worms_info$AphiaID),
  Especies_needed = NA_character_,
  stringsAsFactors = FALSE
)

if("scientificname" %in% names(worms_info)){
  needed$Especies_needed <- as.character(worms_info$scientificname)
}

if("valid_name" %in% names(worms_info)){
  idx_missing_name <- is.na(needed$Especies_needed) | trimws(needed$Especies_needed) == ""
  needed$Especies_needed[idx_missing_name] <- as.character(worms_info$valid_name[idx_missing_name])
}

needed$Especies_needed <- trimws(gsub("\\s+", " ", needed$Especies_needed))
needed <- needed[!is.na(needed$aphiaid) & !is.na(needed$Especies_needed) & needed$Especies_needed != "", ]
needed <- needed[!duplicated(needed$aphiaid), ]

ambi <- read_csv(ambi_file, show_col_types = FALSE)
ambi <- as.data.frame(ambi)
names(ambi) <- trimws(names(ambi))

species_col <- find_col(ambi, c("Especies", "Species", "species", "scientificname"))
ambi_group_col <- find_col(ambi, c("AMBI_Group", "AMBI group", "AMBI", "ambi_group"))

if(is.na(species_col)) stop("Species column not found in AMBI file.")
if(is.na(ambi_group_col)) stop("AMBI_Group column not found in AMBI file.")

ambi$Especies <- trimws(gsub("\\s+", " ", as.character(ambi[[species_col]])))
ambi$AMBI_Group <- suppressWarnings(as.integer(ambi[[ambi_group_col]]))

needed$key <- norm_key(needed$Especies_needed)
ambi$key <- norm_key(ambi$Especies)

ambi_sub <- merge(
  needed[, c("aphiaid", "Especies_needed", "key")],
  ambi[, c("key", "Especies", "AMBI_Group")],
  by = "key",
  all.x = TRUE,
  sort = FALSE
)

# Manual correction preserved from the original workflow
idx_mytilus <- norm_key(ambi_sub$Especies_needed) == norm_key("Mytilus trossulus") & is.na(ambi_sub$AMBI_Group)
ambi_sub$AMBI_Group[idx_mytilus] <- 3L

ambi_sub$ambi_besito <- ifelse(
  is.na(ambi_sub$AMBI_Group), 1L,
  ifelse(
    ambi_sub$AMBI_Group %in% c(4L, 5L), 1L,
    ifelse(
      ambi_sub$AMBI_Group %in% c(2L, 3L), 2L,
      ifelse(ambi_sub$AMBI_Group == 1L, 3L, 1L)
    )
  )
)

ambi_besito <- ambi_sub[, c("aphiaid", "Especies_needed", "ambi_besito")]
names(ambi_besito) <- c("aphiaid", "species", "ambi_besito")
ambi_besito <- ambi_besito[!duplicated(ambi_besito$aphiaid), ]

readr::write_csv(ambi_besito, ambi_besito_file, na = "")
cat("Saved AMBI-BESITO lookup:", ambi_besito_file, "\n")

missing_matches <- ambi_sub[is.na(ambi_sub$AMBI_Group), c("aphiaid", "Especies_needed")]
if(nrow(missing_matches) > 0){
  readr::write_csv(missing_matches, missing_match_file, na = "")
  cat("Saved missing AMBI matches:", missing_match_file, "\n")
}

# =======================================================================
# PART 2. Prepare data-ready file
# =======================================================================

# -----------------------------------------------------------------------
# Create standard output table
# -----------------------------------------------------------------------

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
  ambi_besito = NA_character_,
  BESITO = NA_character_,
  Biomass = NA_real_,
  Biomass_units = NA_character_,
  Abundance = NA_real_,
  Abundance_units = NA_character_,
  Total_biomass = NA_real_,
  Total_abundance = NA_real_,
  MSFD_broad_Ch = NA_character_,
  SAR1 = NA_real_,
  info_SAR1 = NA_character_,
  depth = NA_real_,
  gear = NA_character_,
  sediment = NA_character_,
  stringsAsFactors = FALSE
)

prepared_df <- prepared_df[rep(1, nrow(bio_raw)), , drop = FALSE]

# -----------------------------------------------------------------------
# Fill fields from BIO
# -----------------------------------------------------------------------

prepared_df <- assign_column("station", c("station", "Station", "station_name", "station_id", "stationcode", "stn"), bio_raw, prepared_df, as.character)
prepared_df <- assign_column("year", c("year", "Year", "survey_year", "surveyyear", "yr"), bio_raw, prepared_df, get_year)
prepared_df <- assign_column("month", c("month", "Month"), bio_raw, prepared_df, as.integer)
prepared_df <- assign_column("replicate", c("replicate", "Replicate", "replicates", "Replicates", "rep", "repl"), bio_raw, prepared_df, as.character)

prepared_df <- assign_column("lon", c("lon", "Longitude", "long", "longitude_shooting"), bio_raw, prepared_df, to_num_safe)
prepared_df <- assign_column("lat", c("lat", "Latitude", "latitude_hauling"), bio_raw, prepared_df, to_num_safe)

prepared_df <- assign_column("AphiaID", c("AphiaID", "aphiaid"), bio_raw, prepared_df, to_num_safe)
prepared_df <- assign_column("TaxCode", c("TaxCode", "taxcode"), bio_raw, prepared_df, as.character)
prepared_df <- assign_column("Species", c("Species", "species", "scientificname"), bio_raw, prepared_df, as.character)
prepared_df <- assign_column("Biomass", c("biomass", "Biomass"), bio_raw, prepared_df, to_num_safe)
prepared_df <- assign_column("Abundance", c("abundance", "Abundance"), bio_raw, prepared_df, to_num_safe)

prepared_df <- assign_column("MSFD_broad_Ch", c("MSFD_broad_Ch", "MSFD_BBHT", "Habitat_type_MSFD", "habitat_type_msfd"), bio_raw, prepared_df, as.character)
prepared_df <- assign_column("depth", c("depth", "Depth"), bio_raw, prepared_df, to_num_safe)
prepared_df <- assign_column("gear", c("gear", "Gear"), bio_raw, prepared_df, as.character)
prepared_df <- assign_column("sediment", c("sediment", "Sediment"), bio_raw, prepared_df, as.character)

prepared_df$station <- normalize_station(prepared_df$station)
prepared_df$replicate <- normalize_rep_id(prepared_df$replicate)

# station_2 is station-year for this dataset
prepared_df$station_2 <- build_station2(prepared_df$station, prepared_df$year)
prepared_df$station_2 <- toupper(trimws(as.character(prepared_df$station_2)))

# -----------------------------------------------------------------------
# WoRMS: Fill missing AphiaID from TaxCode when needed
# -----------------------------------------------------------------------

missing_idx <- which(
  is.na(prepared_df$AphiaID) &
    !is.na(prepared_df$TaxCode) &
    trimws(prepared_df$TaxCode) != ""
)

taxnames_missing <- unique(prepared_df$TaxCode[missing_idx])

if(length(taxnames_missing) > 0){
  worms_results <- wormsbymatchnames(taxnames_missing)
  
  if(!is.null(worms_results)){
    if(is.list(worms_results) && !is.data.frame(worms_results)){
      worms_results <- do.call(rbind, worms_results)
    }
    
    worms_results <- as.data.frame(worms_results)
    
    if(nrow(worms_results) > 0 && all(c("scientificname", "AphiaID") %in% names(worms_results))){
      aphia_map <- worms_results[, c("scientificname", "AphiaID")]
      colnames(aphia_map) <- c("TaxCode", "AphiaID_new")
      prepared_df <- merge(prepared_df, aphia_map, by = "TaxCode", all.x = TRUE, sort = FALSE)
      prepared_df$AphiaID <- ifelse(is.na(prepared_df$AphiaID), prepared_df$AphiaID_new, prepared_df$AphiaID)
      prepared_df$AphiaID_new <- NULL
    }
  }
}

# -----------------------------------------------------------------------
# WoRMS: Fill Species using AphiaID
# -----------------------------------------------------------------------

missing_species_idx <- which(
  !is.na(prepared_df$AphiaID) &
    (is.na(prepared_df$Species) | trimws(as.character(prepared_df$Species)) == "")
)

if(length(missing_species_idx) > 0){
  aphia_ids_missing <- unique(prepared_df$AphiaID[missing_species_idx])
  worms_info_species <- wormsbyid(aphia_ids_missing)
  
  if(!is.null(worms_info_species) && nrow(worms_info_species) > 0){
    aphia_map <- worms_info_species[, c("AphiaID", "scientificname")]
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
# Load STATION and merge using station_2
# -----------------------------------------------------------------------

if(!is.na(station_sheet)){
  
  station_raw <- as.data.frame(read_excel(input_file, sheet = station_sheet, col_types = "text"))
  names(station_raw) <- tolower(gsub(" ", "_", trimws(names(station_raw))))
  
  st_station_col <- find_col(station_raw, c("station", "station_name", "station_id", "stationcode", "station_code", "stn"))
  st_year_col <- find_col(station_raw, c("year", "survey_year", "surveyyear", "yr"))
  
  if(is.na(st_station_col) || is.na(st_year_col)){
    stop("Cannot find station/year columns in station sheet.")
  }
  
  station_raw$station_clean <- normalize_station(station_raw[[st_station_col]])
  station_raw$year_clean <- get_year(station_raw[[st_year_col]])
  station_raw$station_2 <- build_station2(station_raw$station_clean, station_raw$year_clean)
  station_raw$station_2 <- toupper(trimws(as.character(station_raw$station_2)))
  
  pressure_candidates <- unique(c(
    "pressure_value", "pressure_value_1y", "pressure_value_3y", "pressure_value_5y",
    "sar1", "sar3", "sar5", "sar2009", "sarmax", "a_wm2019_2021"
  ))
  
  present_pressure <- intersect(pressure_candidates, names(station_raw))
  
  if(length(present_pressure) > 0){
    for(cc in present_pressure){
      station_raw[[cc]] <- to_num_safe(station_raw[[cc]])
    }
  }
  
  idx <- match(prepared_df$station_2, station_raw$station_2)
  cat("Match success by station_2 (%):", round(mean(!is.na(idx)) * 100, 2), "\n")
  
  prepared_df$SAR1 <- fill_sar(file_name, sar1_mapping, station_raw, idx)
  prepared_df$info_SAR1 <- sar1_info
  
  lon_cols <- c("longitude", "lon", "long", "longitude_shooting")
  lat_cols <- c("latitude", "lat", "latitude_hauling")
  lon_col <- lon_cols[lon_cols %in% names(station_raw)]
  lat_col <- lat_cols[lat_cols %in% names(station_raw)]
  
  if(length(lon_col) > 0) prepared_df$lon <- to_num_safe(station_raw[[lon_col[1]]][idx])
  if(length(lat_col) > 0) prepared_df$lat <- to_num_safe(station_raw[[lat_col[1]]][idx])
  
  if("gear" %in% names(station_raw)) prepared_df$gear <- station_raw$gear[idx]
  if("month" %in% names(station_raw)) prepared_df$month <- suppressWarnings(as.integer(station_raw$month[idx]))
  if("depth" %in% names(station_raw)) prepared_df$depth <- to_num_safe(station_raw$depth[idx])
  
  if("total_biomass" %in% names(station_raw)) prepared_df$Total_biomass <- to_num_safe(station_raw$total_biomass[idx])
  if("total_abundance" %in% names(station_raw)) prepared_df$Total_abundance <- to_num_safe(station_raw$total_abundance[idx])
  
  msfd_cols <- c("habitat_type", "msfd_broad_ch", "msfd_bbht", "habitat_type_msfd")
  msfd_col <- msfd_cols[msfd_cols %in% names(station_raw)]
  if(length(msfd_col) > 0) prepared_df$MSFD_broad_Ch <- station_raw[[msfd_col[1]]][idx]
  
  if("sediment" %in% names(station_raw)) prepared_df$sediment <- station_raw$sediment[idx]
}

# -----------------------------------------------------------------------
# Biomass / Abundance units from Metadata
# -----------------------------------------------------------------------

possible_meta_sheets <- c("Metadata and protocols", "Metadata_and_protocols", "Metadata_and_Protocols")
meta_sheet <- sheets[sheets %in% possible_meta_sheets]

biomass_units <- NA_character_
abundance_units <- NA_character_

if(length(meta_sheet) > 0){
  meta <- as.data.frame(read_excel(input_file, sheet = meta_sheet[1], col_names = FALSE))
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
# Merge AMBI-BESITO by AphiaID
# -----------------------------------------------------------------------

n_before <- nrow(prepared_df)
TotalBiomass_before <- sum(prepared_df$Biomass, na.rm = TRUE)

prepared_df$ambi_besito <- NULL
prepared_df$BESITO <- NULL

prepared_df <- merge(
  prepared_df,
  ambi_besito[, c("aphiaid", "ambi_besito")],
  by.x = "AphiaID",
  by.y = "aphiaid",
  all.x = TRUE,
  sort = FALSE
)

# Missing AMBI matches are assigned ambi_besito = 1
prepared_df$ambi_besito <- ifelse(
  is.na(prepared_df$ambi_besito) | trimws(as.character(prepared_df$ambi_besito)) == "",
  "1",
  as.character(prepared_df$ambi_besito)
)

# Final BESITO column for compatibility with downstream SoS scripts
prepared_df$BESITO <- prepared_df$ambi_besito

n_after <- nrow(prepared_df)
TotalBiomass_after <- sum(prepared_df$Biomass, na.rm = TRUE)

Proportion <- NA_real_
if(!is.na(TotalBiomass_before) && TotalBiomass_before > 0){
  Proportion <- TotalBiomass_after / TotalBiomass_before
}

cat("AMBI-BESITO coverage (biomass proportion):", round(Proportion * 100, 2), "%\n")

coverage_summary <- data.frame(
  file = file_name,
  n_rows_before = n_before,
  n_rows_after = n_after,
  TotalBiomass_before = TotalBiomass_before,
  TotalBiomass_after = TotalBiomass_after,
  Proportion = Proportion,
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------
# Force final column order
# -----------------------------------------------------------------------

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
  "ambi_besito",
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
  "SAR1",
  "info_SAR1"
)

prepared_df <- prepared_df[, final_col_order[final_col_order %in% names(prepared_df)], drop = FALSE]

prepared_df_export <- prepared_df
if("TaxCode" %in% names(prepared_df_export)) prepared_df_export$TaxCode <- NULL

# -----------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------

output_file <- file.path(
  output_folder,
  paste0(tools::file_path_sans_ext(file_name), "_data_ready.xlsx")
)

write.xlsx(prepared_df_export, output_file, rowNames = FALSE)
cat("Saved:", output_file, "\n")

coverage_out <- file.path(output_folder, "AMBI_BESITO_coverage_summary.xlsx")
write.xlsx(coverage_summary, coverage_out, rowNames = FALSE)
cat("Saved AMBI-BESITO coverage summary:", coverage_out, "\n")

cat("Finished:", file_name, "\n")
