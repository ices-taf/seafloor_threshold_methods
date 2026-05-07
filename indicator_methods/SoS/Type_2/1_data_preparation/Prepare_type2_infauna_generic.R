# =======================================================================
# Prepare Type 2 INFAUNA datasets for SoS calculation
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script prepares Type 2 infauna datasets for subsequent
# sentinel species selection, SoS calculation, and pressure-state analysis.
#
# It reads raw Type 2 Excel datasets from the repository data folder,
# harmonises biological and station-level information, standardises
# variable names, retrieves missing taxonomic information where possible,
# builds station-year or station-year-replicate identifiers when needed,
# integrates pressure metrics, and exports one prepared data file
# per dataset.
#
# This script is designed as a GENERAL workflow for Type 2 infauna
# datasets. Due to differences in data structure across datasets
# (e.g. station identifiers, replicate handling, or pressure format),
# some datasets require dedicated preparation scripts (special cases).
#
# These special-case scripts complement this general workflow and
# ensure accurate harmonisation where dataset-specific handling is needed.
#
# -----------------------------------------------------------------------
# Main steps
# -----------------------------------------------------------------------
# - Read Type 2 raw datasets from the repository data folder
# - Select infauna datasets for processing
# - Detect biological and station sheets using flexible matching
# - Standardise station, year, taxonomic and biological variables
# - Build station_2 identifiers for joining biological and station data
# - Harmonise taxonomy using WoRMS (AphiaID and species names)
# - Merge station-level information into biological records
# - Extract and standardise pressure metrics when available
# - Merge BESITO sensitivity scores
# - Assign BESITO = 1 when no score is available
# - Export one prepared "_data_ready.xlsx" file per input dataset
# - Export a BESITO coverage summary file
#
# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
# - Raw Type 2 Excel datasets stored in:
#   ../../../data/Type2/
# - External BESITO lookup table
#
# -----------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------
# - One prepared "_data_ready.xlsx" file per input dataset saved in:
#   ../prepared_data/INFAUNA_DATA/
# - One BESITO coverage summary file saved in:
#   ../prepared_data/INFAUNA_DATA/
#
# -----------------------------------------------------------------------
# Notes
# -----------------------------------------------------------------------
# - This script is intended for INFAUNA datasets only
# - WoRMS is used to complete missing AphiaID or species names when possible
# - BESITO is treated as an external dependency
# - Some datasets require matching using composite identifiers
#   (e.g. station + year; station + year + replicate)
# - Dedicated scripts are provided for datasets requiring:
#     * alternative station matching strategies
#     * dataset-specific replicate handling
#     * pressure metrics stored in non-standard columns
#     * dataset-specific cleaning steps
#
# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx
# - worms

# ----------------------------
# Load libraries
# ----------------------------
library(readxl)
library(openxlsx)
library(worms)

# ----------------------------
# Clear workspace
# ----------------------------
rm(list = ls())
gc()

# -----------------------------------------------------------------------
# Input and output folders
# -----------------------------------------------------------------------

input_folder  <- "../../../data/Type2"
output_folder <- "../prepared_data/INFAUNA_DATA"

if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# -----------------------------------------------------------------------
# List input files
# -----------------------------------------------------------------------
all_files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)
all_files <- all_files[!grepl("^~\\$", basename(all_files))] # Remove temporary Excel lock files
all_files <- all_files[!grepl("^BS_southernbaltic_oxygendepletion\\.xlsx$", basename(all_files))] # Skip coverage-only file

# -----------------------------------------------------------------------
# Keep ONLY Type 2 INFAUNA datasets handled by the general workflow
# -----------------------------------------------------------------------
infauna_files <- c(
  "BS_gotland_btrawling.xlsx",
  "BS_southernbaltic_btrawling.xlsx",
  "NS_doggerbank_btrawling.xlsx",
  "NS_fladenground_btrawling.xlsx",
  "NS_Hinderbanken_btrawling.xlsx",
  "NS_Kattegat_btrawling.xlsx",
  "NS_Kattegat_btrawling_old.xlsx",
  "NS_LongForties_btrawling.xlsx",
  "NS_SilverPit_btrawling.xlsx",
  "WMS_IMPEC_btrawling.xlsx"
)

all_files <- all_files[basename(all_files) %in% infauna_files]

cat("Files to process:\n")
print(basename(all_files))

# ----------------------------
# Helper functions
# ----------------------------
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

norm_rep <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  toupper(x)
}

build_station2 <- function(station, year, replicate = NULL){
  st <- trimws(as.character(station))
  yr <- trimws(as.character(year))
  if(!is.null(replicate)){
    rp <- norm_rep(replicate)
    paste(st, yr, rp, sep = "_")
  } else {
    paste(st, yr, sep = "_")
  }
}

detect_bio_sheet <- function(sheets){
  bio <- sheets[sheets %in% c("Biological information","Biological info","Biological_information","Biological_Information")]
  if(length(bio) == 0) return(NA_character_)
  bio[1]
}

detect_station_sheet <- function(sheets, bio_sheet){
  possible_station_sheets <- c(
    "Station information","station information",
    "Station info","station info",
    "Station_information","station_information","Station_Information",
    "Station raw","Station_raw","Station","Stations"
  )
  st <- sheets[sheets %in% possible_station_sheets]
  if(length(st) > 0) return(st[1])
  rest <- setdiff(sheets, bio_sheet)
  if(length(rest) == 0) return(NA_character_)
  rest[1]
}

# ----------------------------
# Pressure mappings
# ----------------------------
sar3_mapping <- list(
  "WMS_IMPEC_btrawling.xlsx" = "a_wm2019_2021"
)
sar3_info <- "Average over the last 3 years prior to the sample"

# Where information on temporal aggregation is not provided, SAR is treated as an annual value.
sar1_mapping <- list(
  "BS_gotland_btrawling.xlsx"        = "pressure_value",
  "BS_southernbaltic_btrawling.xlsx" = "pressure_value",
  "NS_doggerbank_btrawling.xlsx"     = "pressure_value",
  "NS_fladenground_btrawling.xlsx"   = "pressure_value",
  "NS_Hinderbanken_btrawling.xlsx"   = "pressure_value",
  "NS_Kattegat_btrawling.xlsx"       = "pressure_value",
  "NS_Kattegat_btrawling_old.xlsx"   = "pressure_value",
  "NS_LongForties_btrawling.xlsx"    = "pressure_value",
  "NS_SilverPit_btrawling.xlsx"      = "pressure_value"
)
sar1_info <- "Average over the last 1 year prior to the sample"

fill_sar <- function(file_name, mapping, station_raw, idx){
  out <- rep(NA_real_, length(idx))
  if(file_name %in% names(mapping)){
    col <- mapping[[file_name]]
    col <- tolower(gsub(" ", "_", col))
    if(col %in% names(station_raw)){
      out <- to_num_safe(station_raw[[col]][idx])
    } else {
      warning(paste("Column", col, "not found in station sheet for", file_name))
    }
  }
  out
}

# ----------------------------
# Load BESITO once
# ----------------------------
besito_infauna_file <- "SET_PATH_TO_EXTERNAL_BESITO_INFAUNA_FILE"

besito_raw <- read.csv(besito_infauna_file, stringsAsFactors = FALSE)
names(besito_raw) <- trimws(tolower(names(besito_raw)))

if(!all(c("aphiaid","besito") %in% names(besito_raw))){
  stop("BESITO file does not contain columns 'aphiaid' and 'besito' (case-insensitive).")
}

besito_raw$aphiaid <- to_num_safe(besito_raw$aphiaid)
besito <- besito_raw[!duplicated(besito_raw$aphiaid), c("aphiaid","besito")]

# ----------------------------
# Store BESITO coverage results
# ----------------------------
coverage_summary <- data.frame(
  file = character(0),
  n_rows_before = integer(0),
  n_rows_after  = integer(0),
  TotalBiomass_before = numeric(0),
  TotalBiomass_after  = numeric(0),
  Proportion = numeric(0),
  stringsAsFactors = FALSE
)

# ----------------------------
# Main loop
# ----------------------------
for(file_path in all_files){
  
  file_name <- basename(file_path)
  cat("\n----------------------------------------\n")
  cat("Processing file:", file_name, "\n")
  
  # ----------------------------
  # Identify sheets
  # ----------------------------
  sheets <- excel_sheets(file_path)
  bio_sheet <- detect_bio_sheet(sheets)
  if(is.na(bio_sheet)) stop("No biological information sheet found in file.")
  
  station_sheet <- detect_station_sheet(sheets, bio_sheet)
  if(is.na(station_sheet)) warning("No station sheet found. Station-related fields will remain NA.")
  
  cat("BIO sheet:", bio_sheet, "\n")
  cat("STATION sheet:", station_sheet, "\n")
  
  # ----------------------------
  # Load BIO
  # ----------------------------
  bio_raw <- read_excel(file_path, sheet = bio_sheet)
  bio_raw <- as.data.frame(bio_raw)
  
  # ----------------------------
  # Create EMPTY standard table
  # ----------------------------
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
    SAR3 = NA_real_,
    info_SAR3 = NA_character_,
    SAR5 = NA_real_,
    info_SAR5 = NA_character_,
    SAR2009 = NA_real_,
    info_SAR2009 = NA_character_,
    SARmax = NA_real_,
    info_SARmax = NA_character_,
    BESITO = NA_character_,
    depth = NA_real_,
    gear = NA_character_,
    sediment = NA_character_,
    stringsAsFactors = FALSE
  )
  
  prepared_df <- prepared_df[rep(1, nrow(bio_raw)), , drop = FALSE]
  
  # ----------------------------
  # Fill from BIO
  # ----------------------------
  prepared_df <- assign_column("station", c("station","Station","station_name","station_id","stationcode","stn"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("year", c("year","Year","survey_year","surveyyear","yr"), bio_raw, prepared_df, get_year)
  prepared_df <- assign_column("month", c("month","Month"), bio_raw, prepared_df, as.integer)
  prepared_df <- assign_column("replicate", c("replicate","Replicate","replicates","Replicates","rep","repl"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("lon", c("lon","Longitude","long","longitude_shooting"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("lat", c("lat","Latitude","latitude_hauling"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("AphiaID", c("AphiaID","aphiaid"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("TaxCode", c("TaxCode","taxcode"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Species", c("Species","species","scientificname"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Biomass", c("biomass","Biomass"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("Abundance", c("abundance","Abundance"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("MSFD_broad_Ch", c("MSFD_broad_Ch","MSFD_BBHT","Habitat_type_MSFD","habitat_type_msfd"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("depth", c("depth","Depth"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("gear", c("gear","Gear"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("sediment", c("sediment","Sediment"), bio_raw, prepared_df, as.character)
  
  prepared_df$station_2 <- build_station2(prepared_df$station, prepared_df$year)
  
  # ----------------------------
  # Fill missing AphiaID from TaxCode using WoRMS
  # ----------------------------
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
  
  # ----------------------------
  # Fill Species using WoRMS AphiaID
  # ----------------------------
  missing_species_idx <- which(!is.na(prepared_df$AphiaID) & (is.na(prepared_df$Species) | trimws(prepared_df$Species) == ""))
  
  if(length(missing_species_idx) > 0){
    aphia_ids_missing <- unique(prepared_df$AphiaID[missing_species_idx])
    worms_info <- wormsbyid(aphia_ids_missing)
    if(!is.null(worms_info) && nrow(worms_info) > 0){
      aphia_map <- worms_info[, c("AphiaID", "scientificname")]
      colnames(aphia_map) <- c("AphiaID", "Species_new")
      prepared_df <- merge(prepared_df, aphia_map, by = "AphiaID", all.x = TRUE, sort = FALSE)
      prepared_df$Species <- ifelse(is.na(prepared_df$Species) | trimws(prepared_df$Species) == "", prepared_df$Species_new, prepared_df$Species)
      prepared_df$Species_new <- NULL
    }
  }
  
  # ----------------------------
  # Load STATION and merge using station_2
  # ----------------------------
  if(!is.na(station_sheet)){
    
    station_raw <- read_excel(file_path, sheet = station_sheet, col_types = "text")
    station_raw <- as.data.frame(station_raw)
    names(station_raw) <- tolower(gsub(" ", "_", names(station_raw)))
    
    st_station_col <- find_col(station_raw, c("station","station_name","station_id","stationcode","station_code","stn"))
    st_year_col    <- find_col(station_raw, c("year","survey_year","surveyyear","yr"))
    st_rep_col     <- find_col(station_raw, c("replicate","replicates","rep","repl","haul","sample","subsample"))
    
    if(is.na(st_station_col) || is.na(st_year_col)){
      stop("Cannot find station/year columns in station sheet.")
    }
    
    st_year_vec <- get_year(station_raw[[st_year_col]])
    
    if(!is.na(st_rep_col)){
      station_raw$station_2 <- build_station2(station_raw[[st_station_col]], st_year_vec, station_raw[[st_rep_col]])
      if(any(trimws(prepared_df$replicate) != "", na.rm = TRUE)){
        prepared_df$station_2 <- build_station2(prepared_df$station, prepared_df$year, prepared_df$replicate)
      }
    } else {
      station_raw$station_2 <- build_station2(station_raw[[st_station_col]], st_year_vec)
      prepared_df$station_2 <- build_station2(prepared_df$station, prepared_df$year)
    }
    
    station_raw$station_2 <- toupper(trimws(as.character(station_raw$station_2)))
    prepared_df$station_2 <- toupper(trimws(as.character(prepared_df$station_2)))
    
    pressure_candidates <- unique(c(
      "pressure_value", "pressure_value_1y", "pressure_value_3y", "pressure_value_5y",
      "sar1","sar3","sar5","sar2009","sarmax",
      "a_wm2019_2021"
    ))
    
    present_pressure <- intersect(pressure_candidates, names(station_raw))
    if(length(present_pressure) > 0){
      for(cc in present_pressure){
        station_raw[[cc]] <- to_num_safe(station_raw[[cc]])
      }
    }
    
    idx <- match(prepared_df$station_2, station_raw$station_2)
    cat("Match success (%):", round(mean(!is.na(idx)) * 100, 2), "\n")
    
    prepared_df$SAR1 <- fill_sar(file_name, sar1_mapping, station_raw, idx)
    prepared_df$SAR3 <- fill_sar(file_name, sar3_mapping, station_raw, idx)
    
    if(file_name %in% names(sar1_mapping)) prepared_df$info_SAR1 <- sar1_info
    if(file_name %in% names(sar3_mapping)) prepared_df$info_SAR3 <- sar3_info
    
    lon_cols <- c("longitude","lon","long","longitude_shooting")
    lat_cols <- c("latitude","lat","latitude_hauling")
    lon_col <- lon_cols[lon_cols %in% names(station_raw)]
    lat_col <- lat_cols[lat_cols %in% names(station_raw)]
    
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
  
  # ----------------------------
  # Read Biomass / Abundance units from Metadata
  # ----------------------------
  possible_meta_sheets <- c("Metadata and protocols", "Metadata_and_protocols", "Metadata_and_Protocols")
  meta_sheet <- sheets[sheets %in% possible_meta_sheets]
  
  biomass_units <- NA_character_
  abundance_units <- NA_character_
  
  if(length(meta_sheet) > 0){
    meta <- read_excel(file_path, sheet = meta_sheet[1], col_names = FALSE)
    meta <- as.data.frame(meta)
    colnames(meta) <- paste0("col", seq_len(ncol(meta)))
    meta$col1_low <- tolower(trimws(as.character(meta$col1)))
    
    idx_biomass <- which(grepl("^biomass$", meta$col1_low) | (grepl("biomass", meta$col1_low) & !grepl("total", meta$col1_low)))
    if(length(idx_biomass) > 0){
      biomass_units <- as.character(meta$col2[idx_biomass[1]])
      if(toupper(biomass_units) %in% c("NA","")) biomass_units <- NA
    }
    
    idx_abundance <- which(grepl("^abundance$", meta$col1_low) | (grepl("abund", meta$col1_low) & !grepl("total", meta$col1_low)))
    if(length(idx_abundance) > 0){
      abundance_units <- as.character(meta$col2[idx_abundance[1]])
      if(toupper(abundance_units) %in% c("NA","")) abundance_units <- NA
    }
  }
  
  prepared_df$Biomass_units <- biomass_units
  prepared_df$Abundance_units <- abundance_units
  
  # ----------------------------
  # Merge BESITO and calculate coverage
  # ----------------------------
  n_before <- nrow(prepared_df)
  TotalBiomass <- sum(prepared_df$Biomass, na.rm = TRUE)
  
  prepared_df$BESITO <- NULL
  prepared_df <- merge(prepared_df, besito, by.x = "AphiaID", by.y = "aphiaid", all.x = TRUE, sort = FALSE)
  names(prepared_df)[names(prepared_df) == "besito"] <- "BESITO"
  
  prepared_df$BESITO <- ifelse(
    is.na(prepared_df$BESITO) | trimws(as.character(prepared_df$BESITO)) == "",
    "1",
    as.character(prepared_df$BESITO)
  )
  
  prepared_df <- prepared_df[!is.na(prepared_df$BESITO), , drop = FALSE]
  
  n_after <- nrow(prepared_df)
  TotalBiomassAfterMerging <- sum(prepared_df$Biomass, na.rm = TRUE)
  
  Propor <- NA_real_
  if(!is.na(TotalBiomass) && TotalBiomass > 0){
    Propor <- TotalBiomassAfterMerging / TotalBiomass
  }
  
  cat("BESITO coverage (biomass proportion):", round(Propor * 100, 2), "%\n")
  
  coverage_summary <- rbind(
    coverage_summary,
    data.frame(
      file = file_name,
      n_rows_before = n_before,
      n_rows_after  = n_after,
      TotalBiomass_before = TotalBiomass,
      TotalBiomass_after  = TotalBiomassAfterMerging,
      Proportion = Propor,
      stringsAsFactors = FALSE
    )
  )
  
  # ----------------------------
  # Force final column order
  # ----------------------------
  final_col_order <- c(
    "station","station_2","year","month","depth","gear","replicate","lon","lat",
    "AphiaID","Species","BESITO","TaxCode","Biomass","Biomass_units",
    "Abundance","Abundance_units","Total_biomass","Total_abundance",
    "MSFD_broad_Ch","sediment","SAR1","info_SAR1","SAR3","info_SAR3"
  )
  
  prepared_df <- prepared_df[, final_col_order[final_col_order %in% names(prepared_df)], drop = FALSE]
  
  prepared_df_export <- prepared_df
  if("TaxCode" %in% names(prepared_df_export)) prepared_df_export$TaxCode <- NULL
  
  output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(file_name), "_data_ready.xlsx"))
  write.xlsx(prepared_df_export, output_file, rowNames = FALSE)
  
  cat("Saved:", output_file, "\n")
  cat("Finished:", file_name, "\n\n")
}

