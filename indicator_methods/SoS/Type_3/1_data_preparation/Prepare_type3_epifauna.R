# =======================================================================
# Prepare Type 3 EPIFAUNA datasets for SoS calculation
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script prepares Type 3 epifauna datasets for subsequent
# sentinel species selection, SoS calculation, and pressure-state analysis.
#
# It reads raw Type 3 Excel datasets from the repository data folder,
# harmonises biological and station-level information, standardises
# variable names, retrieves missing taxonomic information where possible,
# integrates SAR pressure metrics, and exports one prepared data file
# per dataset.
#
# The script is designed for EPIFAUNA datasets only.
# Infauna datasets requiring dataset-specific handling are treated in
# separate scripts.
#
# -----------------------------------------------------------------------
# Main steps
# -----------------------------------------------------------------------
# - Read Type 3 raw datasets from the repository data folder
# - Exclude infauna datasets from processing
# - Detect biological and station sheets using flexible matching
# - Standardise station, year, taxonomic and biological variables
# - Harmonise taxonomy using WoRMS (AphiaID and species names)
# - Merge station-level information into biological records
# - Extract and standardise SAR pressure metrics when available
# - Merge BESITO sensitivity scores
# - Export one prepared "_data_ready.xlsx" file per input dataset
#
# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
# - Raw Type 3 Excel datasets stored in:
#   ../../../data/Type3/
# - External BESITO lookup table
#
# -----------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------
# - One prepared "_data_ready.xlsx" file per input dataset saved in:
#   ../prepared_data/EPIFAUNA_DATA/
#
# -----------------------------------------------------------------------
# Notes
# -----------------------------------------------------------------------
# - This script is intended for EPIFAUNA datasets only
# - WoRMS is used to complete missing AphiaID or species names when possible
# - BESITO is treated as an external dependency
# - Some Type 3 infauna datasets require special-case preparation scripts
#   due to differences in station identifiers, SAR format, or sheet structure
#
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

input_folder  <- "../../../data/Type3"
output_folder <- "../prepared_data/EPIFAUNA_DATA"

if(!dir.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)
}

# -----------------------------------------------------------------------
# List input files
# -----------------------------------------------------------------------

all_files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)
all_files <- all_files[!grepl("^~\\$", basename(all_files))] # Remove temporary Excel lock files

# -----------------------------------------------------------------------
# Exclude habitat coverage datasets 
# -----------------------------------------------------------------------
all_files <- all_files[!grepl("BS_FI_habitatcoverage\\.xlsx$", all_files)] # 

# -----------------------------------------------------------------------
# Exclude INFAUNA datasets (handled in separate scripts)
# -----------------------------------------------------------------------
infauna_files <- c(
  "CS_NS_UKhabitats.xlsx",
  "NS_BEhabitats.xlsx",
  "NS_DKhabitats.xlsx",
  "NS_NLhabitats.xlsx",
  "WMS_APPEALMED.xlsx"
)
all_files <- all_files[!basename(all_files) %in% infauna_files]

# ----------------------------
for(file_path in all_files){
  file_name <- basename(file_path)
  cat("Processing file:", file_name, "\n")
  
  # ----------------------------
  # Identify biological sheet
  # ----------------------------
  sheets <- excel_sheets(file_path)
  bio_sheet <- sheets[sheets %in% c("Biological information","Biological info","Biological_information","Biological_Information")]
  if(length(bio_sheet) == 0) stop("No biological information sheet found in file.")
  
  # ----------------------------
  # Load Biological Information
  # ----------------------------
  bio_raw <- read_excel(file_path, sheet = bio_sheet[1])
  
  # ----------------------------
  # Create EMPTY standard table (only final columns)
  # ----------------------------
  prepared_df <- data.frame(
    station = NA_character_,
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
  prepared_df <- prepared_df[rep(1, nrow(bio_raw)), ] # replicate rows
  
  # ----------------------------
  # Fill columns explicitly (case-insensitive)
  # ----------------------------
  assign_column <- function(target_col, possible_names, df_raw, df_prepared, convert_fun = identity){
    col_name <- names(df_raw)[tolower(names(df_raw)) %in% tolower(possible_names)]
    if(length(col_name) == 1){
      df_prepared[[target_col]] <- convert_fun(df_raw[[col_name]])
    }
    return(df_prepared)
  }
  
  prepared_df <- assign_column("station", c("station", "Station","station_name"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("year", c("year", "Year"), bio_raw, prepared_df, as.integer)
  prepared_df <- assign_column("month", c("month", "Month"), bio_raw, prepared_df, as.integer)
  prepared_df <- assign_column("replicate", c("replicate", "Replicate","replicates", "Replicates"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("lon", c("lon", "Longitude", "long","longitude_shooting"), bio_raw, prepared_df, as.numeric)
  prepared_df <- assign_column("lat", c("lat", "Latitude","latitude_hauling"), bio_raw, prepared_df, as.numeric)
  prepared_df <- assign_column("AphiaID", c("AphiaID"), bio_raw, prepared_df, as.numeric)
  prepared_df <- assign_column("TaxCode", c("TaxCode", "taxcode"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Species", c("Species", "species"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("Biomass", c("biomass", "Biomass"), bio_raw, prepared_df, as.numeric)
  prepared_df <- assign_column("Abundance", c("abundance", "Abundance"), bio_raw, prepared_df, as.numeric)
  prepared_df <- assign_column("MSFD_broad_Ch", c("MSFD_broad_Ch","MSFD_BBHT","Habitat_type_MSFD"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("BESITO", c("BESITO"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("depth", c("depth", "Depth"), bio_raw, prepared_df, as.numeric)
  prepared_df <- assign_column("gear", c("gear", "Gear"), bio_raw, prepared_df, as.character)
  prepared_df <- assign_column("sediment", c("sediment", "Sediment"), bio_raw, prepared_df, as.character)
  
  # ----------------------------
  # Fill missing AphiaID from TaxCode using WoRMS
  # ----------------------------
  missing_idx <- which(is.na(prepared_df$AphiaID) & !is.na(prepared_df$TaxCode) & prepared_df$TaxCode != "")
  taxnames_missing <- unique(prepared_df$TaxCode[missing_idx])
  if(length(taxnames_missing) > 0){
    worms_results <- wormsbymatchnames(taxnames_missing)
    aphia_map <- worms_results[, c("scientificname", "AphiaID")]
    colnames(aphia_map) <- c("TaxCode", "AphiaID_new")
    
    # Merge while keeping original order
    prepared_df <- merge(prepared_df, aphia_map, by = "TaxCode", all.x = TRUE, sort = FALSE)
    prepared_df$AphiaID <- ifelse(is.na(prepared_df$AphiaID), prepared_df$AphiaID_new, prepared_df$AphiaID)
    prepared_df$AphiaID_new <- NULL
  }
  
  # ----------------------------
  # Fill Species using WoRMS AphiaID
  # ----------------------------
  missing_species_idx <- which(!is.na(prepared_df$AphiaID) & (is.na(prepared_df$Species) | prepared_df$Species == ""))
  if(length(missing_species_idx) > 0){
    aphia_ids_missing <- unique(prepared_df$AphiaID[missing_species_idx])
    
    # Call WoRMS to retrieve scientific names
    worms_info <- wormsbyid(aphia_ids_missing)
    
    aphia_map <- worms_info[, c("AphiaID", "scientificname")]
    colnames(aphia_map) <- c("AphiaID", "Species_new")
    
    prepared_df <- merge(prepared_df, aphia_map, by = "AphiaID", all.x = TRUE, sort = FALSE)
    
    prepared_df$Species <- ifelse(is.na(prepared_df$Species) | prepared_df$Species == "", prepared_df$Species_new, prepared_df$Species)
    
    # Remove auxiliary column
    prepared_df$Species_new <- NULL
  }
  
  # ----------------------------
  # Load Station Information and merge
  # ----------------------------
  possible_station_sheets <- c("Station information","station information", "Station info","station info", "Station_information","station_information","Station_Information")
  station_sheet <- sheets[sheets %in% possible_station_sheets]
  if(length(station_sheet) == 0){
    warning("No Station Information sheet found. Station-related columns will remain NA.")
  } else {
    # Load station sheet
    station_raw <- read_excel(file_path, sheet = station_sheet[1])
    # Normalize column names: lowercase + underscores
    names(station_raw) <- tolower(gsub(" ", "_", names(station_raw)))
    
    # Match stations
    if("station" %in% names(station_raw)){
      
      idx <- match(tolower(prepared_df$station), tolower(station_raw$station))
      
      
      
      
      # ----------------------------
      # SAR columns mapping by file
      # ----------------------------
      # SAR5
      sar5_mapping <- list(
        "BoBIC_IberianChabitats.xlsx" = "pressure_value",
        "BoBIC_GulfofCadizhabitats.xlsx" = "pressure_value",
        "WMS_EShabitats.xlsx" = "pressure_value",
        "WMS_IEOESPhabitats.xlsx" = "pressure_value",
        "NS_BEhabitats.xlsx" = "pressure_value",
        "WMS_NOURMED.xlsx" = "sar5",
        "BoBIC_CGFS.xlsx" = "pressure_value2",
        "FR_ORHAGO.xlsx" = "pressure_value2",
        "CS_EVHOE.xlsx" = "pressure_value2",
        "WMS_FRMEDITS.xlsx" = "sar5",
        "WMS_APPEALMED.xlsx" = "sar5",
        "CS_NS_IBTSFR.xlsx" = "pressure_value2",
        "NS_NLhabitats.xlsx" = "pressure_value_5y"
      )
      sar5_info <- "Average over the last 5 years prior to the sample"
      
      # SAR1
      sar1_mapping <- list(
        "BoBIC_CGFS.xlsx" = "pressure_value",
        "FR_ORHAGO.xlsx" = "pressure_value",
        "CS_EVHOE.xlsx" = "pressure_value",
        "CS_NS_IBTSFR.xlsx" = "pressure_value",
        "WMS_FRMEDITS.xlsx" = "pressure_value",
        "NS_DKhabitats.xlsx" = "pressure_value",
        "WMS_NOURMED.xlsx" = "sar1",
        "WMS_ISCMS_IRBIMCNR.xlsx" = "pressure_value",
        "WMS_APPEALMED.xlsx" = "pressure_value"
      )
      sar1_info <- "Average over the last 1 year prior to the sample"
      
      # SARmax
      sarmax_mapping <- list(
        "BoBIC_CGFS.xlsx" = "pressure_value3",
        "FR_ORHAGO.xlsx" = "pressure_value3",
        "CS_EVHOE.xlsx" = "pressure_value3",
        "CS_NS_IBTSFR.xlsx" = "pressure_value3",
        "WMS_NOURMED.xlsx" = "sarmax",
        "WMS_FRMEDITS.xlsx" = "sarmax",
        "WMS_APPEALMED.xlsx" = "sarmax"
      )
      sarmax_info <- "Maximum SAR"
      
      # SAR3
      sar3_mapping <- list(
        "WMS_ISCMS_IRBIMCNR.xlsx" = "pressure_value_3yravg"
      )
      sar3_info <- "Average over the last 3 years prior to the sample"
      
      # SAR2009
      sar2009_mapping <- list(
        "CS_NS_UKhabitats.xlsx" = "pressure_value"
      )
      sar2009_info <- "Average from 2009 to the sampling year"
      
      # ----------------------------
      # Fill SAR columns
      # ----------------------------
      # SAR5
      if(file_name %in% names(sar5_mapping)){
        col_sar5 <- sar5_mapping[[file_name]]
        if(col_sar5 %in% names(station_raw)){
          prepared_df$SAR5 <- suppressWarnings(as.numeric(station_raw[[col_sar5]][idx]))
          prepared_df$SAR_info5 <- sar5_info
        }
      }
      
      # SAR1
      if(file_name %in% names(sar1_mapping)){
        col_sar1 <- sar1_mapping[[file_name]]
        if(col_sar1 %in% names(station_raw)){
          prepared_df$SAR1 <- suppressWarnings(as.numeric(station_raw[[col_sar1]][idx]))
          prepared_df$SAR_info1 <- sar1_info
        }
      }
      
      # SARmax
      if(file_name %in% names(sarmax_mapping)){
        col_sarmax <- sarmax_mapping[[file_name]]
        if(col_sarmax %in% names(station_raw)){
          prepared_df$SARmax <- suppressWarnings(as.numeric(station_raw[[col_sarmax]][idx]))
          prepared_df$SAR_info_max <- sarmax_info
        }
      }
      
      # SAR3
      if(file_name %in% names(sar3_mapping)){
        col_sar3 <- sar3_mapping[[file_name]]
        if(col_sar3 %in% names(station_raw)){
          prepared_df$SAR3 <- suppressWarnings(as.numeric(station_raw[[col_sar3]][idx]))
          prepared_df$SAR_info3 <- sar3_info
        }
      }
      
      # SAR2009
      if(file_name %in% names(sar2009_mapping)){
        col_sar2009 <- sar2009_mapping[[file_name]]
        if(col_sar2009 %in% names(station_raw)){
          prepared_df$SAR2009 <- suppressWarnings(as.numeric(station_raw[[col_sar2009]][idx]))
          prepared_df$SAR_info2009 <- sar2009_info
        }
      }
      
      prepared_df$info_SAR5 <- sar5_info
      prepared_df$info_SAR1 <- sar1_info
      prepared_df$info_SARmax <- sarmax_info
      prepared_df$info_SAR3 <- sar3_info
      prepared_df$info_SAR2009 <- sar2009_info
      
      # ----------------------------
      # Other station-level fields
      # ----------------------------
      
      longitude_cols <- c("longitude","Longitude", "lon","long", "longitude_shooting")
      longitude_col <- longitude_cols[longitude_cols %in% names(station_raw)]
      if(length(longitude_col) > 0) prepared_df$lon <- station_raw[[longitude_col[1]]][idx]
      
      # Latitude column check (same approach)
      latitude_cols <- c("latitude", "Latitude","lat", "latitude_hauling", "lat")
      latitude_col <- latitude_cols[latitude_cols %in% names(station_raw)]
      if(length(latitude_col) > 0) prepared_df$lat <- station_raw[[latitude_col[1]]][idx]
      
      # Continue with the rest of your checks
      if("gear" %in% names(station_raw)) prepared_df$gear <- station_raw$gear[idx]
      if("month" %in% names(station_raw)) prepared_df$month <- station_raw$month[idx]
      if("total_biomass" %in% names(station_raw)){
        prepared_df$Total_biomass <- station_raw$total_biomass[idx]
      } else if("total_biomass.x" %in% names(station_raw)){
        prepared_df$Total_biomass <- station_raw$total_biomass.x[idx]
      }
      if("total_abundance" %in% names(station_raw)) prepared_df$Total_abundance <- station_raw$total_abundance[idx]
      msfd_cols <- c("habitat_type", "msfd_broad_ch", "MSFD_broad_Ch", "MSFD_BBHT","habitat_type_MSFD","habitat_type_msfd","msfd_bbht")
      msfd_col <- msfd_cols[msfd_cols %in% names(station_raw)]
      if(length(msfd_col) > 0) prepared_df$MSFD_broad_Ch <- station_raw[[msfd_col[1]]][idx]
      
      if("sediment" %in% names(station_raw)) prepared_df$sediment <- station_raw$sediment[idx]
      if("depth" %in% names(station_raw)) prepared_df$depth <- station_raw$depth[idx]
      
      
    }
  }
  
  # ----------------------------
  # Read Biomass / Abundance units from Metadata
  # ----------------------------
  # Identify Metadata sheet
  possible_meta_sheets <- c(
    "Metadata and protocols", "Metadata_and_protocols", "Metadata_and_Protocols"
  )
  meta_sheet <- sheets[sheets %in% possible_meta_sheets]
  biomass_units <- NA_character_
  abundance_units <- NA_character_
  if(length(meta_sheet) > 0){
    meta <- read_excel(file_path, sheet = meta_sheet[1], col_names = FALSE)
    colnames(meta) <- paste0("col", seq_len(ncol(meta)))
    meta$col1_low <- tolower(trimws(meta$col1))
    
    # ---- Biomass (NOT total biomass)
    idx_biomass <- which(
      grepl("^biomass$", meta$col1_low) | 
        (grepl("biomass", meta$col1_low) & !grepl("total", meta$col1_low))
    )
    if(length(idx_biomass) > 0){
      biomass_units <- as.character(meta$col2[idx_biomass[1]])
      if(toupper(biomass_units) %in% c("NA", "")) biomass_units <- NA
    }
    
    # ---- Abundance (NOT total abundance)
    idx_abundance <- which(
      grepl("^abundance$", meta$col1_low) | 
        (grepl("abund", meta$col1_low) & !grepl("total", meta$col1_low))
    )
    if(length(idx_abundance) > 0){
      abundance_units <- as.character(meta$col2[idx_abundance[1]])
      if(toupper(abundance_units) %in% c("NA", "")) abundance_units <- NA
    }
  }
  
  # Fill Biomass / Abundance units
  prepared_df$Biomass_units <- biomass_units
  prepared_df$Abundance_units <- abundance_units
  
  # ----------------------------
  # Load BESITO epifauna, merge, and assign to prepared_df
  # ----------------------------
  # File paths
  
  besito_epifauna_file <- "SET_PATH_TO_EXTERNAL_BESITO_EPIFAUNA_FILE"
  
  # Load files
  besito_epifauna <- read_excel(besito_epifauna_file)
  
  # Normalized names
  names(besito_epifauna) <- tolower(names(besito_epifauna))
  
  # Check required columns
  if(!all(c("aphiaid","besito") %in% names(besito_epifauna))){
    stop("The epifauna file does not contain 'AphiaID' and 'BESITO' columns")  }
  
  # Remove duplicates
  besito <- besito_epifauna[!duplicated(besito_epifauna$aphiaid), ]
  prepared_df$BESITO <- NULL
  
  # Merge with prepared_df
  prepared_df <- merge(prepared_df, besito[, c("aphiaid","besito")], by.x = "AphiaID", by.y = "aphiaid", all.x = TRUE, sort = FALSE)
  
  # names
  names(prepared_df)[names(prepared_df) == "besito"] <- "BESITO"
  
  # ----------------------------
  # Check final table
  # ----------------------------
  str(prepared_df)
  head(prepared_df)
  
  # ----------------------------
  # Construct output file name
  # ----------------------------
  
  # ----------------------------
  # Force final column order
  # ----------------------------
  final_col_order <- c(
    "station","year","month","depth","gear","replicate","lon","lat", 
    "AphiaID","Species", "BESITO","TaxCode", "Biomass","Biomass_units", 
    "Abundance","Abundance_units", "Total_biomass","Total_abundance", 
    "MSFD_broad_Ch","sediment","SAR1","info_SAR1", "SAR3","info_SAR3", 
    "SAR5","info_SAR5", "SAR2009","info_SAR2009", "SARmax","info_SARmax"
  )
  
  # Keep only existing columns and reorder
  prepared_df <- prepared_df[, final_col_order[final_col_order %in% names(prepared_df)]]
  
  prepared_df_export <- prepared_df
  prepared_df_export$TaxCode <- NULL
  
  output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(file_name), "_data_ready.xlsx"))
  write.xlsx(prepared_df_export, output_file, rowNames = FALSE)
  
  cat("Finished:", file_name, "\n\n")
}
