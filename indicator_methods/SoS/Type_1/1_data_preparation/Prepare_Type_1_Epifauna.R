# =========================================
# Type 1 EPIFAUNA (species by biomass) -> SoS ready
# =========================================
# Description:
# This script prepares Type 1 epifauna datasets (species by biomass)
# for SoS (Sentinels of the Seabed) calculation.
#
# It processes xlsx files from the Type 1 repository data folder,
# harmonises biological and station data, constructs station_2
# (station + year, no replicate), and assigns BESITO sensitivity scores.
#
# Purpose:
#  - Process all xlsx files in Type 1 starting with:
#      ES_Cantabrian_NIA, ES_Meditteranean_NIA
#  - Detect Type 1 sheets:
#      BIO     = "Species information"
#      STATION = "Time series information"
#    (fallback to alternative common names)
#  - Build station_2 = station + year (no replicate) and use as join key
#  - Extract Biomass / Abundance from alternative BIO column names:
#       Biomass   <- Outcome_value_biomass OR Outcome_value
#       Abundance <- Outcome_value_abundance OR Outcome_value_Number_individuals
#  - Merge station-level Total_biomass / Total_abundance when available
#  - Merge BESITO from epifauna Excel lookup table
#  - No default value assigned when missing
#
# Input:
# - Type 1 datasets (species by biomass) from the repository data folder
# - BESITO epifauna lookup table (external dependency)
#
# Output:
# - One prepared "data_ready.xlsx" file per input dataset for SoS calculation
#
# Notes:
# - Input data are stored in the repository (data folder)
# - The script uses WoRMS to complete missing taxonomic information when possible
# - The BESITO epifauna lookup table is not included yet and must be provided externally
# =========================================
# Required packages:
# - readxl
# - openxlsx
# - worms

library(readxl)
library(openxlsx)
library(worms)

rm(list = ls())
gc()

# ----------------------------
# Folders
# ----------------------------
input_folder  <- "../../../data/Type1/species by biomass"
output_folder <- "../prepared_data/EPIFAUNA_DATA"
if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# ----------------------------
# Select files by prefix
# ----------------------------
prefixes <- c("ES_Cantabrian_NIA", "ES_Meditteranean_NIA")

all_files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)
all_files <- all_files[!grepl("^~\\$", basename(all_files))]  # skip temp Excel files

keep <- rep(FALSE, length(all_files))
for(p in prefixes){
  keep <- keep | grepl(paste0("^", p), basename(all_files))
}
all_files <- all_files[keep]

cat("Files to process:\n"); print(basename(all_files))
if(length(all_files) == 0) stop("No matching files found with the requested prefixes in Type1.")

# ----------------------------
# Helpers
# ----------------------------
to_num_safe <- function(x){
  # If already numeric, return as is
  if (is.numeric(x)) return(x)
  
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "TRUE", "FALSE")] <- NA
  
  # Replace decimal commas with dots
  x <- gsub(",", ".", x)
  
  # Keep digits, decimal point, sign, and scientific notation
  x <- gsub("[^0-9eE\\.\\-\\+]", "", x)
  
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

# Find column name by candidates (case-insensitive exact match)
find_col <- function(df, candidates){
  nms <- names(df)
  nms_low <- tolower(trimws(nms))
  cand_low <- tolower(trimws(candidates))
  hit <- match(cand_low, nms_low)
  hit <- hit[!is.na(hit)]
  if(length(hit) == 0) return(NA_character_)
  nms[hit[1]]
}

# Assign a column if found (first match)
assign_column <- function(target_col, possible_names, df_raw, df_prepared, convert_fun = identity){
  col_name <- names(df_raw)[tolower(trimws(names(df_raw))) %in% tolower(trimws(possible_names))]
  if(length(col_name) >= 1){
    df_prepared[[target_col]] <- convert_fun(df_raw[[col_name[1]]])
  }
  df_prepared
}

# NIA Type1: Species information + Time series information
detect_bio_sheet <- function(sheets){
  low <- tolower(trimws(sheets))
  
  # NIA Type1 (tu caso)
  idx <- which(low %in% c("species information","species_information"))
  if(length(idx) > 0) return(sheets[idx[1]])
  
  # Alternative possible sheet names
  candidates <- c("biological information","biological_information","biological info","biological_info","bio")
  hit <- sheets[low %in% candidates]
  if(length(hit) > 0) return(hit[1])
  
  idx <- which(grepl("biolog", low))
  if(length(idx) > 0) return(sheets[idx[1]])
  
  # Last fallback
  sheets[1]
}

detect_station_sheet <- function(sheets){
  low <- tolower(trimws(sheets))
  
  # NIA Type1 (tu caso)
  idx <- which(low %in% c("time series information","time_series_information"))
  if(length(idx) > 0) return(sheets[idx[1]])
  
  # Alternative possible sheet names
  candidates <- c("station information","station_information","station info","station_info","station","stations")
  hit <- sheets[low %in% candidates]
  if(length(hit) > 0) return(hit[1])
  
  idx <- which(grepl("station", low))
  if(length(idx) > 0) return(sheets[idx[1]])
  
  NA_character_
}

# BIO: pick biomass column
pick_biomass_col <- function(df){
  candidates <- c(
    "outcome_value_biomass",
    "outcome_value biomass",
    "outcome_value"
  )
  find_col(df, candidates)
}

# BIO: pick abundance column
pick_abundance_col <- function(df){
  candidates <- c(
    "outcome_value_abundance",
    "outcome_value abundance",
    "outcome_value_number_individuals",
    "outcome_value number individuals",
    "number_individuals"
  )
  find_col(df, candidates)
}


# ----------------------------
# BESITO EPIFAUNA (NO default=1)
# ----------------------------
besito_epifauna_file <- "SET_PATH_TO_EXTERNAL_BESITO_EPIFAUNA_FILE"
if(besito_epifauna_file == "SET_PATH_TO_EXTERNAL_BESITO_EPIFAUNA_FILE"){
  stop("Please provide the path to the external BESITO epifauna lookup table.")
}
besito_epifauna <- read_excel(besito_epifauna_file)
besito_epifauna <- as.data.frame(besito_epifauna)
names(besito_epifauna) <- tolower(trimws(names(besito_epifauna)))

if(!all(c("aphiaid","besito") %in% names(besito_epifauna))){
  stop("The epifauna BESITO file must contain columns 'aphiaid' and 'besito' (case-insensitive).")
}

besito_epifauna$aphiaid <- to_num_safe(besito_epifauna$aphiaid)
besito_map <- besito_epifauna[!duplicated(besito_epifauna$aphiaid), c("aphiaid","besito")]

# ----------------------------
# Loop files
# ----------------------------
for(file_path in all_files){
  
  file_name <- basename(file_path)
  cat("\n----------------------------------------\n")
  cat("Processing file:", file_name, "\n")
  
  sheets <- excel_sheets(file_path)
  bio_sheet <- detect_bio_sheet(sheets)
  station_sheet <- detect_station_sheet(sheets)
  
  cat("BIO sheet:", bio_sheet, "\n")
  cat("STATION sheet:", station_sheet, "\n")
  
  # ----------------------------
  # Load BIO
  # ----------------------------
  bio_raw <- read_excel(file_path, sheet = bio_sheet)
  bio_raw <- as.data.frame(bio_raw)
  
  # ----------------------------
  # Create standard table (includes station_2)
  # ----------------------------
  prepared_df <- data.frame(
    station = NA_character_,
    station_2 = NA_character_,   # JOIN KEY station+year
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
    BESITO = NA_character_,
    depth = NA_real_,
    gear = NA_character_,
    sediment = NA_character_,
    stringsAsFactors = FALSE
  )
  prepared_df <- prepared_df[rep(1, nrow(bio_raw)), , drop = FALSE]
  
  # ----------------------------
  # Fill basics from BIO
  # ----------------------------
  prepared_df <- assign_column("station",   c("station","Station","station_name","stationcode","station_code","stn"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("year",      c("year","Year","survey_year","surveyyear","yr"),                     bio_raw, prepared_df, get_year)
  prepared_df <- assign_column("month",     c("month","Month"),                                                   bio_raw, prepared_df, as.integer)
  prepared_df <- assign_column("replicate", c("replicate","Replicate","replicates","Replicates","rep","repl"),    bio_raw, prepared_df, as.character)
  
  prepared_df <- assign_column("lon", c("lon","longitude","Longitude","long","longitude_shooting"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("lat", c("lat","latitude","Latitude","latitude_hauling"),           bio_raw, prepared_df, to_num_safe)
  
  prepared_df <- assign_column("AphiaID", c("AphiaID","aphiaid"), bio_raw, prepared_df, to_num_safe)
  prepared_df <- assign_column("TaxCode", c("TaxCode","taxcode","taxon","code"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Species", c("Species","species","scientificname","scientific_name"), bio_raw, prepared_df, as.character)
  
  # Biomass / Abundance from Outcome_* fields
  bcol <- pick_biomass_col(bio_raw)
  if(!is.na(bcol)){
    prepared_df$Biomass <- to_num_safe(bio_raw[[bcol]])
  } else {
    warning("No biomass column found in BIO (Outcome_value_biomass / Outcome_value). Biomass stays NA.")
  }
  
  acol <- pick_abundance_col(bio_raw)
  if(!is.na(acol)){
    prepared_df$Abundance <- to_num_safe(bio_raw[[acol]])
  } else {
    warning("No abundance column found in BIO (Outcome_value_abundance / Outcome_value_Number_individuals). Abundance stays NA.")
  }
  
  # Units directly from BIO if present (NIA Type1 usually has these)
  bu <- find_col(bio_raw, c("outcome_units_biomass","outcome_units biomass","biomass_units","biomass unit"))
  au <- find_col(bio_raw, c("outcome_units_number_individuals","outcome_units number individuals",
                            "outcome_units_number_individuals ","abundance_units","abundance unit"))
  if(!is.na(bu)) prepared_df$Biomass_units <- as.character(bio_raw[[bu]])
  if(!is.na(au)) prepared_df$Abundance_units <- as.character(bio_raw[[au]])
  
  # Build station_2 = station + year (JOIN KEY)
  prepared_df$station <- normalize_station(prepared_df$station)
  prepared_df$station_2 <- toupper(build_station2(prepared_df$station, prepared_df$year))
  
  # ----------------------------
  # WoRMS: Fill missing AphiaID from TaxCode (optional)
  # ----------------------------
  missing_idx <- which(is.na(prepared_df$AphiaID) & !is.na(prepared_df$TaxCode) & trimws(prepared_df$TaxCode) != "")
  taxnames_missing <- unique(prepared_df$TaxCode[missing_idx])
  
  if(length(taxnames_missing) > 0){
    worms_results <- tryCatch(wormsbymatchnames(taxnames_missing), error = function(e) NULL)
    if(!is.null(worms_results) && nrow(worms_results) > 0){
      # map TaxCode -> AphiaID
      aphia_map <- worms_results[, c("scientificname", "AphiaID")]
      colnames(aphia_map) <- c("TaxCode_map", "AphiaID_map")
      aphia_map$TaxCode_map <- as.character(aphia_map$TaxCode_map)
      aphia_map$AphiaID_map <- to_num_safe(aphia_map$AphiaID_map)
      
      m <- match(as.character(prepared_df$TaxCode), aphia_map$TaxCode_map)
      prepared_df$AphiaID[is.na(prepared_df$AphiaID) & !is.na(m)] <- aphia_map$AphiaID_map[m[is.na(prepared_df$AphiaID) & !is.na(m)]]
    }
  }
  
  # WoRMS: Fill Species using AphiaID (optional)
  missing_species_idx <- which(!is.na(prepared_df$AphiaID) & (is.na(prepared_df$Species) | trimws(prepared_df$Species) == ""))
  if(length(missing_species_idx) > 0){
    aphia_ids_missing <- unique(prepared_df$AphiaID[missing_species_idx])
    worms_info <- tryCatch(wormsbyid(aphia_ids_missing), error = function(e) NULL)
    
    # wormsbyid can return a data.frame or list depending on version
    if(is.data.frame(worms_info) && nrow(worms_info) > 0){
      aphia_map <- worms_info[, c("AphiaID", "scientificname")]
      colnames(aphia_map) <- c("AphiaID_map", "Species_map")
      aphia_map$AphiaID_map <- to_num_safe(aphia_map$AphiaID_map)
      
      m <- match(prepared_df$AphiaID, aphia_map$AphiaID_map)
      fill_idx <- which((is.na(prepared_df$Species) | trimws(prepared_df$Species) == "") & !is.na(m))
      prepared_df$Species[fill_idx] <- as.character(aphia_map$Species_map[m[fill_idx]])
    }
  }
  
  # ----------------------------
  # Load STATION and merge by station_2 (station+year)
  #  NIA Type1: "Time series information"
  # ----------------------------
  if(is.na(station_sheet)){
    warning("No Station/Time series sheet found. Station-level fields remain NA.")
  } else {
    
    station_raw <- read_excel(file_path, sheet = station_sheet, col_types = "text")
    station_raw <- as.data.frame(station_raw)
    names(station_raw) <- tolower(gsub(" ", "_", trimws(names(station_raw))))
    
    st_station_col <- find_col(station_raw, c("station","station_name","stationcode","station_code","stn"))
    st_year_col    <- find_col(station_raw, c("year","survey_year","surveyyear","yr"))
    
    if(is.na(st_station_col) || is.na(st_year_col)){
      warning("Cannot find station/year columns in STATION/Time series sheet. No merge performed.")
    } else {
      
      station_raw$station_clean <- normalize_station(station_raw[[st_station_col]])
      station_raw$year_clean    <- get_year(station_raw[[st_year_col]])
      station_raw$station_2     <- toupper(build_station2(station_raw$station_clean, station_raw$year_clean))
      
      idx <- match(prepared_df$station_2, station_raw$station_2)
      cat("Match success (%):", round(mean(!is.na(idx))*100, 2), "\n")
      
      # coords
      lon_col <- find_col(station_raw, c("longitude","lon","long","longitude_shooting"))
      lat_col <- find_col(station_raw, c("latitude","lat","latitude_hauling"))
      
      if(!is.na(lon_col)) prepared_df$lon <- to_num_safe(station_raw[[lon_col]][idx])
      if(!is.na(lat_col)) prepared_df$lat <- to_num_safe(station_raw[[lat_col]][idx])
      
      # depth/gear/sediment/MSFD
      dep_col <- find_col(station_raw, c("depth"))
      if(!is.na(dep_col)) prepared_df$depth <- to_num_safe(station_raw[[dep_col]][idx])
      
      gear_col <- find_col(station_raw, c("gear"))
      if(!is.na(gear_col)) prepared_df$gear <- station_raw[[gear_col]][idx]
      
      sed_col <- find_col(station_raw, c("sediment"))
      if(!is.na(sed_col)) prepared_df$sediment <- station_raw[[sed_col]][idx]
      
      msfd_col <- find_col(station_raw, c("msfd_broad_ch","msfd_bbht","habitat_type_msfd","habitat_type"))
      if(!is.na(msfd_col)) prepared_df$MSFD_broad_Ch <- station_raw[[msfd_col]][idx]
      
      # totals when exist (NIA Type1 often uses total_Number_individuals)
      tb_col <- find_col(station_raw, c("total_biomass","totalbiomass"))
      ta_col <- find_col(station_raw, c("total_abundance","totalabundance","total_number_individuals","total_number_individuals_"))
      
      if(!is.na(tb_col)) prepared_df$Total_biomass <- to_num_safe(station_raw[[tb_col]][idx])
      if(!is.na(ta_col)) prepared_df$Total_abundance <- to_num_safe(station_raw[[ta_col]][idx])
    }
  }
  
  # ----------------------------
  # If units are still NA, try metadata sheet (optional fallback)
  # ----------------------------
  if(all(is.na(prepared_df$Biomass_units)) || all(is.na(prepared_df$Abundance_units))){
    biomass_units <- NA_character_
    abundance_units <- NA_character_
    
    meta_candidates <- c("metadata and protocols","metadata_and_protocols","metadata_and_protocols ")
    meta_sheet <- sheets[tolower(trimws(sheets)) %in% meta_candidates]
    
    if(length(meta_sheet) > 0){
      meta <- as.data.frame(read_excel(file_path, sheet = meta_sheet[1], col_names = FALSE))
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
    
    if(all(is.na(prepared_df$Biomass_units))) prepared_df$Biomass_units <- biomass_units
    if(all(is.na(prepared_df$Abundance_units))) prepared_df$Abundance_units <- abundance_units
  }
  
  # ----------------------------
  # Merge BESITO epifauna (NO default=1)
  # ----------------------------
  # preserve order using match (avoid merge reordering)
  bes_m <- match(prepared_df$AphiaID, besito_map$aphiaid)
  prepared_df$BESITO <- as.character(besito_map$besito[bes_m])
  
  # ----------------------------
  # Final columns + Export (include station_2)
  # ----------------------------
  final_col_order <- c(
    "station","station_2","year","month","depth","gear","replicate","lon","lat",
    "AphiaID","Species","BESITO","TaxCode",
    "Biomass","Biomass_units",
    "Abundance","Abundance_units",
    "Total_biomass","Total_abundance",
    "MSFD_broad_Ch","sediment"
  )
  
  prepared_df <- prepared_df[, final_col_order[final_col_order %in% names(prepared_df)], drop = FALSE]
  
  prepared_df_export <- prepared_df
  if("TaxCode" %in% names(prepared_df_export)) prepared_df_export$TaxCode <- NULL
  
  output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(file_name), "_data_ready.xlsx"))
  write.xlsx(prepared_df_export, output_file, rowNames = FALSE)
  
  cat("Saved:", output_file, "\n")
}
