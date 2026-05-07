# =======================================================================
# Identification of sentinel species and SoS calculation for Data Type 2
# Template script
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script provides a template to identify sentinel species and calculate
# SoS values for prepared Data Type 2 datasets.
#
# It is intended to run on one prepared dataset at a time.
#
# Data Type 2 datasets are usually smaller than Data Type 3 datasets and
# often correspond to a single habitat or case study. Therefore, this script
# is intentionally less strict than the Data Type 3 workflow.
#

# -----------------------------------------------------------------------
# Methodological note for Data Type 2 datasets
# -----------------------------------------------------------------------
# Data Type 2 datasets contain pressure-state information but usually have
# fewer stations/hauls and a more limited pressure gradient than Data Type 3.
#
# Therefore:
# - no automatic filter requiring >20 hauls per habitat is applied here;
# - the script assumes that the selected dataset corresponds to the target
#   habitat or case study;
# - pressure classes must be checked manually before using the sentinel
#   species function;
# - the sentinel species list must be reviewed by experts before final use.

# -----------------------------------------------------------------------
# Pressure-class definition
# -----------------------------------------------------------------------
# The default approach is to classify pressure into five pressure classes:
#
#   No pressure
#   Low pressure
#   Medium pressure
#   High pressure
#   Very high pressure
#
# However, Data Type 2 datasets often include few stations/hauls. If one or
# more pressure classes are empty, or if the reference class contains too
# few samples, the sentinel-species function may fail or produce a weakly
# supported list.
#
# In those cases, pressure classes may be simplified manually into broader
# groups, for example:
#
#   1 = reference / low-pressure group
#   2 = pressured group
#
# This simplification is not the ideal default. It is a pragmatic solution
# for small Type 2 datasets and must be documented for each dataset.
# -----------------------------------------------------------------------
# Reference-condition selection for Data Type 2 datasets
# -----------------------------------------------------------------------
# The reference group used to identify sentinel species must be checked
# manually for each Data Type 2 dataset.
#
# For bottom-trawling datasets, the initial recommendation is to start with
# low or negligible trawling pressure as the reference condition. For example:
#
#   SAR <= 0.10
#
# However, Data Type 2 datasets often contain few stations/hauls. If the
# reference group contains too few samples, the reference condition may need
# to be relaxed stepwise, for example:
#
#   1. First try SAR <= 0.10
#   2. If too few reference samples are available, try SAR <= 0.33
#   3. If still too few reference samples are available, try SAR <= 0.65
#
# Unlike Data Type 3, no fixed minimum number of reference hauls is enforced
# automatically here. The selected reference threshold must be justified
# based on the number of available samples and expert judgement.
#
# Important:
# - Do not automatically use the same reference threshold for all datasets.
# - Always inspect the number of samples in the reference class.
# - Use the least relaxed threshold that provides a workable reference group.
# - Document the selected threshold for each dataset.
#
# In very small datasets, the sentinel-species function may return fewer
# than the standard target number of sentinel species. In those cases, the
# partial list may still be used, but it must be clearly documented and
# reviewed by experts.

# -----------------------------------------------------------------------
# Special note for oxygen depletion datasets
# -----------------------------------------------------------------------
# Some Type 2 datasets do not represent bottom-trawling pressure.
# For oxygen-depletion datasets, the pressure gradient should be interpreted
# according to oxygen conditions, not SAR.
#
# In general:
# - high oxygen values indicate better/reference conditions;
# - low oxygen values indicate stronger oxygen-depletion pressure.
#
# Therefore, for oxygen-depletion datasets, the reference group should be
# defined using high oxygen values, and the selected threshold must be
# checked and documented.
#
# In addition, some oxygen-depletion datasets may not use BESITO directly.
# Instead, BESITO-like scores may be derived from AMBI ecological groups
# during data preparation. If so, this must be documented in the preparation
# script and in the analysis notes.

# -----------------------------------------------------------------------
# Ecological review of sentinel species
# -----------------------------------------------------------------------
# The sentinel species list produced by this script is not automatically
# final. It must be reviewed by regional experts.
#
# Species may be excluded from the final sentinel list when they are not
# considered representative of habitat condition, including:
# - invasive or non-indigenous species;
# - parasitic species;
# - species known to be unsuitable indicators for the habitat or pressure.
#
# All expert exclusions should be documented.

# -----------------------------------------------------------------------
# Input / output description
# -----------------------------------------------------------------------
# INPUT:
# - One prepared Data Type 2 dataset produced by the data-preparation
#   workflow.
# - The input file must contain, at minimum:
#     * station or station_2 identifier
#     * Species
#     * BESITO sensitivity score, or BESITO-like sensitivity score
#     * Biomass and/or Abundance
#     * pressure information, usually stored in SAR1, SAR3, SAR5, or another
#       prepared pressure column
#
# OUTPUT:
# - One output folder per input dataset
# - Sentinel species list
# - SoS-ready dataset
# - Simple diagnostic plots
# - Processing summary

# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx
# - vegan
# - ggplot2
# - mgcv
# - parallel
# - pbapply
# - dplyr

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(vegan)
library(ggplot2)
library(mgcv)
library(parallel)
library(pbapply)
library(dplyr)


# -----------------------------------------------------------------------
# Clear workspace
# -----------------------------------------------------------------------

rm(list = ls())
gc()

# -----------------------------------------------------------------------
# User settings
# -----------------------------------------------------------------------

input_file <- file.path(
  "../prepared_data/INFAUNA_DATA",
  "DATASET_NAME_data_ready.xlsx"
)

output_base_folder <- file.path("../sos_output", "INFAUNA")

# Select the station identifier.
# Use station_2 when the prepared file contains a more appropriate
# sample-level or station-year identifier.
#station_id_col <- "station"
station_id_col <- "station_2"

# Select the continuous pressure column.
pressure_col <- "SAR1"

# Use biomass by default. Set to FALSE to use abundance.
use_biomass <- TRUE

# Number of cores for TS_species_Para.
number_of_cores <- 4

# Light frequency filter. Set to 0 to keep all species.
species_frequency_threshold <- 0.5

# -----------------------------------------------------------------------
# Pressure-class settings
# -----------------------------------------------------------------------

# Option 1: five pressure classes.
# This is the preferred/default structure when enough samples are available.
use_five_pressure_classes <- TRUE

# Example thresholds for bottom-trawling pressure.
# Modify if needed for each dataset.
no_pressure_max     <- 0.33
low_pressure_max    <- 1.20
medium_pressure_max <- 3.00
high_pressure_max   <- 6.00

# Option 2: simplified two-class structure.
# Use only when the five-class structure is not feasible.
reference_pressure_threshold <- 0.33

# Direction of the reference condition:
# FALSE = low pressure values are reference, e.g. SAR datasets.
# TRUE  = high pressure-variable values are reference, e.g. oxygen datasets.
reverse_pressure_direction <- FALSE

# Example for oxygen depletion:
# use_five_pressure_classes <- FALSE
# reference_pressure_threshold <- 4.5
# reverse_pressure_direction <- TRUE

# -----------------------------------------------------------------------
# Expert-based species exclusions
# -----------------------------------------------------------------------

species_to_remove <- c(
  "Epizoanthus paguriphilus",
  "Calliactis parasitica",
  "Marenzelleria"
)


aphiaid_to_remove <- numeric(0)

# -----------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------

to_num_safe <- function(x){
  if(is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "TRUE", "FALSE")] <- NA
  x <- gsub(",", ".", x)
  x <- gsub("[^0-9eE+\\-.]", "", x)
  suppressWarnings(as.numeric(x))
}

clean_file_label <- function(x){
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

safe_diversity <- function(x){
  x <- as.numeric(x)
  if(all(is.na(x)) || sum(x, na.rm = TRUE) <= 0) return(NA_real_)
  vegan::diversity(x)
}

make_pressure_classes <- function(pressure,
                                  use_five_classes = TRUE,
                                  reverse_direction = FALSE,
                                  reference_threshold = 0.33,
                                  no_pressure_max = 0.33,
                                  low_pressure_max = 1.20,
                                  medium_pressure_max = 3.00,
                                  high_pressure_max = 6.00){
  
  pressure <- as.numeric(pressure)
  
  if(use_five_classes){
    
    if(reverse_direction){
      # Use this only after checking the pressure variable carefully.
      out <- ifelse(
        pressure >= high_pressure_max, "No pressure",
        ifelse(pressure >= medium_pressure_max, "Low pressure",
               ifelse(pressure >= low_pressure_max, "Medium pressure",
                      ifelse(pressure >= no_pressure_max, "High pressure", "Very high pressure")))
      )
    } else {
      out <- ifelse(
        pressure <= no_pressure_max, "No pressure",
        ifelse(pressure > no_pressure_max & pressure <= low_pressure_max, "Low pressure",
               ifelse(pressure > low_pressure_max & pressure <= medium_pressure_max, "Medium pressure",
                      ifelse(pressure > medium_pressure_max & pressure <= high_pressure_max, "High pressure", "Very high pressure")))
      )
    }
    
    out <- factor(
      out,
      levels = c("No pressure", "Low pressure", "Medium pressure", "High pressure", "Very high pressure")
    )
    
  } else {
    
    if(reverse_direction){
      out <- ifelse(pressure >= reference_threshold, "Reference", "Pressured")
    } else {
      out <- ifelse(pressure <= reference_threshold, "Reference", "Pressured")
    }
    
    out <- factor(out, levels = c("Reference", "Pressured"))
  }
  
  out
}

make_group_vector_for_function <- function(pressure_classes){
  pressure_classes <- as.character(pressure_classes)
  out <- rep(NA_character_, length(pressure_classes))
  
  out[pressure_classes == "No pressure"] <- "1"
  out[pressure_classes == "Low pressure"] <- "2"
  out[pressure_classes == "Medium pressure"] <- "3"
  out[pressure_classes == "High pressure"] <- "4"
  out[pressure_classes == "Very high pressure"] <- "5"
  
  out[pressure_classes == "Reference"] <- "1"
  out[pressure_classes == "Pressured"] <- "2"
  
  out
}

# -----------------------------------------------------------------------
# Updated sentinel species function
# -----------------------------------------------------------------------

TS_species_Para <- function(sp_data, group_vect, Besito, NumCor, table){
  
  ####Combinaciones dentro del grupo usando SIMPER
  table_list <- list()
  group <- as.character(unique(group_vect))
  cutoff <- 90
  
  for (g in group){
    
    # Set up parallelization
    n_cores <- NumCor  # Change to the number of cores you want to use
    cl <- makeCluster(n_cores)
    
    dbg <- na.omit(sp_data[group_vect == g, ])
    take <- t(combn(1:nrow(dbg), 2))
    
    # Export dbg and take to the parallel environment (FIXED LINE)
    clusterExport(cl, c("dbg", "take"), envir = environment())
    
    contr <- t(pbsapply(1:nrow(take), function(j) {
      md <- 2 * pmin(dbg[take[j, 1], ], dbg[take[j, 2], ])
      me <- dbg[take[j, 1], ] + dbg[take[j, 2], ]
      100 * (md / sum(me))
    }, cl = cl, simplify = "matrix"))
    
    # Stop parallelization
    stopCluster(cl)
    
    contr <- as.data.frame(contr) %>% 
      mutate(across(everything(), unlist))
    contr<-na.omit(as.data.frame(contr))
    colnames(contr)<-c(names(dbg))
    ndbg<-ncol(dbg)
    x1<-colMeans(contr)
    x2<-apply(contr, 2, sd)
    df1<-data.frame(x1)
    df2<-data.frame(x2)
    x<-c(names(dbg))
    y1<-df1$x1
    y2<-df2$x2
    z2<-data.frame(Species=rep(NA, ndbg), Similarity=rep(NA, ndbg), SD=rep(NA, ndbg))
    z2$Species<-x
    z2$Similarity<-y1
    z2$SD<-y2
    
    ##ordenaci?n de especies por similitud
    z2$Similarity <- as.numeric(as.character(z2$Similarity))
    db<-z2[order(z2$Similarity,decreasing = TRUE),]
    b<-sum(db$Similarity)
    ##contribution to mean similarity
    db$contribution<-((100*db$Similarity)/b)
    
    ##c?lculo de similitud media acumulada /% y corte al 90/% 
    
    d<-0
    for (i in 1:ncol(dbg)){
      d<-(db$contribution[i]+d)
      db$acum[i]<-d
    }
    
    db$Similarity <- round(db$Similarity,2)
    db$SD <- round(db$SD,2)
    db$contribution <- round(db$contribution,2)
    db$acum <- round(db$acum,2)
    db$acum[1]<- ifelse(db$acum[1]>90, 90, db$acum[1])
    
    r<-db[which(db$acum<=cutoff & db$contribution>0),]
    
    Abundance0<-apply(dbg,2,mean)
    Ab_SD0<-apply(dbg,2,sd)
    Ab_SD10<-data.frame(Ab_SD0)
    Ab0<-data.frame(Abundance0)
    x0<-c(names(dbg))
    y0<-round(Ab0$Abundance0,2)
    w0<-round(Ab_SD10$Ab_SD0,2)	
    z0<-cbind(x0,y0,w0)
    rb0<-merge(z0, r, by.x = "x0", by.y = "Species")
    table<-rb0[order(rb0$Similarity,decreasing = TRUE),]
    names(table)<-c("Species", "Av.Abund", "SD.Abund", "Av.Si", "SD.Si", "Contr", "Cum")
    table$Group <- g
    table_list[[g]] <- table
    
    
  }
  
  table_final <- do.call(rbind, table_list) #END OF SIMPER
  
  #Seleccionamos las especies del grupo 1 
  Sp_Group1 <- table_final[table_final$Group=="1",]
  Sp_Group1_vect <- Sp_Group1$Species
  names(Besito) <- c("Species", "BESITO")
  Besito_SIMPER <- as.data.frame(unique(Besito[Besito$Species%in%Sp_Group1_vect,]))
  
  #C?lculo de frecuencias  
  SpNames <- colnames(sp_data)
  sp_data$Filter <- group_vect 
  LowEffort_Data <- sp_data[sp_data$Filter=="1",]
  Freq <- vector()
  Per <- vector()
  for( i in 1:(ncol(LowEffort_Data)-1)){
    ColOnly <- LowEffort_Data[,i]
    ColOnly_Bi <- ifelse(ColOnly>0,1,0)
    Freq[i] <- sum(ColOnly_Bi)
    Per[i] <- (sum(ColOnly_Bi)/nrow(LowEffort_Data))*100
  }
  
  MyFreqMatrix <- cbind.data.frame(SpNames, Freq, Per)
  Th <- ifelse(round(length(group_vect[group_vect==1])/10)<=2,2,round(length(group_vect[group_vect==1])/10))
  MyFreqMatrix <- MyFreqMatrix[MyFreqMatrix$Freq>=Th,]
  names(MyFreqMatrix) <- c("Species", "Freq", "Per")
  
  MyFreqMatrixWithBesito <- unique(merge(MyFreqMatrix, Besito, by="Species"))
  MyFreqMatrixWithBesito <- MyFreqMatrixWithBesito[order(MyFreqMatrixWithBesito$Freq, decreasing=TRUE),]
  
  #Seleccionamos las especies de sensibilidad 5
  
  Sens5_Simper <-  Besito_SIMPER[Besito_SIMPER$BESITO==5,]
  SIM_Freq_Sens <- Sens5_Simper
  
  Mns <- "10 Species reached after include species with a sensitive of 5 from SIMPER"
  
  if (length(Sens5_Simper)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens5_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==5,]
  Sp_Vect5 <- unique(c(as.character(Sens5_Simper$Species), as.character(Sens5_Freq$Species)))
  MyCut<- ifelse(length(Sp_Vect5)>=10,10,length(Sp_Vect5))
  MyCutSp <- Sp_Vect5[MyCut]
  MyCut_df <- Sens5_Freq[Sens5_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens5_Freq[Sens5_Freq$Freq>=MyCut_Frq,]
  SIM_Freq_Sens <- unique(c(as.character(Sens5_Simper$Species), as.character(MyMat$Species)))
  SIM_Freq_Sens <- SIM_Freq_Sens[SIM_Freq_Sens!="integer(0)"]
  
  Mns <- "10 Species reached after include species with a sensitive of 5 ordered by Frecuency"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  #Seleccionamos las especies de sensibilidad 4
  
  Sens4_Simper <-  Besito_SIMPER[Besito_SIMPER$BESITO==4,]
  SIM_Freq_Sens <- unique(c(SIM_Freq_Sens,Sens4_Simper$Species))
  
  Mns <- "10 Species reached after include species with a sensitive of 4 from SIMPER"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens4_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==4,]
  Sp_Vect4 <- unique(c(as.character(SIM_Freq_Sens), as.character(Sens4_Simper$Species),as.character(Sens4_Freq$Species)))
  MyCut <- ifelse(length(Sp_Vect4)>=10,10,length(Sp_Vect4))
  MyCutSp <- Sp_Vect4[MyCut]
  MyCut_df <- Sens4_Freq[Sens4_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens4_Freq[Sens4_Freq$Freq>=MyCut_Frq,]
  SIM_Freq_Sens <- unique(c(as.character(SIM_Freq_Sens),as.character(Sens4_Simper$Species), as.character(MyMat$Species)))
  SIM_Freq_Sens <- SIM_Freq_Sens[SIM_Freq_Sens!="integer(0)"]
  
  Mns <- "10 Species reached after include species with a sensitive of 4 ordered by Frecuency"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  #Seleccionamos las especies de sensibilidad 3
  
  Sens3_Simper <-  Besito_SIMPER[Besito_SIMPER$BESITO==3,]
  SIM_Freq_Sens <- unique(c(SIM_Freq_Sens,Sens3_Simper$Species))
  
  Mns <- "10 Species reached after include species with a sensitive of 3 from SIMPER"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens3_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==3,]
  Sp_Vect3 <- unique(c(as.character(SIM_Freq_Sens), as.character(Sens3_Simper$Species),as.character(Sens3_Freq$Species)))
  MyCut<- ifelse(length(Sp_Vect3)>=10,10,length(Sp_Vect3))
  MyCutSp <- Sp_Vect3[MyCut]
  MyCut_df <- Sens3_Freq[Sens3_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens3_Freq[Sens3_Freq$Freq>=MyCut_Frq,]
  SIM_Freq_Sens <- unique(c(as.character(SIM_Freq_Sens),as.character(Sens3_Simper$Species), as.character(MyMat$Species)))
  SIM_Freq_Sens <- SIM_Freq_Sens[SIM_Freq_Sens!="integer(0)"]
  
  Mns <- "5 Species reached after include species with a sensitive of 3 ordered by Frecuency"
  
  if (length(SIM_Freq_Sens)>=5) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens2_Simper <-  Besito_SIMPER[Besito_SIMPER$BESITO==2,]
  SIM_Freq_Sens <- unique(c(SIM_Freq_Sens,Sens2_Simper$Species))
  
  Mns <- "5 Species reached after include species with a sensitive of 2 from SIMPER"
  
  if (length(SIM_Freq_Sens)>=5) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens2_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==2,]
  Sp_Vect2 <- unique(c(as.character(SIM_Freq_Sens), as.character(Sens2_Simper$Species),as.character(Sens2_Freq$Species)))
  MyCut<- ifelse(length(Sp_Vect2)>=5,5,length(Sp_Vect2))
  MyCutSp <- Sp_Vect2[MyCut]
  MyCut_df <- Sens2_Freq[Sens2_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens2_Freq[Sens2_Freq$Freq>=MyCut_Frq,]
  SIM_Freq_Sens2 <- unique(c(as.character(SIM_Freq_Sens),as.character(Sens3_Simper$Species), as.character(MyMat$Species)))
  SIM_Freq_Sens2 <- SIM_Freq_Sens2[SIM_Freq_Sens2!="integer(0)"]
  
  Mns <- "5 Species reached after include species with a sensitive of 2 ordered by Frecuency"
  if (length(SIM_Freq_Sens2)>=5) {
    print(Mns)
    return(SIM_Freq_Sens2)
  }
  
  
  Mns <- paste0(
    "Careful: Loop finished without reaching the minimum number of species required (5). ",
    "Returning the partial sentinel-species list."
  )
  warning(Mns)
  
  SIM_Freq_Sens2 <- unique(as.character(SIM_Freq_Sens2))
  SIM_Freq_Sens2 <- SIM_Freq_Sens2[
    !is.na(SIM_Freq_Sens2) &
      SIM_Freq_Sens2 != "" &
      SIM_Freq_Sens2 != "integer(0)"
  ]
  
  return(SIM_Freq_Sens2)
}


# -----------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------

if(!file.exists(input_file)){
  stop(paste("Input file not found:", input_file))
}

SOS_data <- as.data.frame(read_excel(input_file))
file_name <- gsub("_data_ready.xlsx$", "", basename(input_file))
analysis_label <- clean_file_label(file_name)

output_folder <- file.path(output_base_folder, file_name)
if(!dir.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)
}

cat("Input file:", input_file, "
")
cat("Output folder:", output_folder, "
")

# -----------------------------------------------------------------------
# Check required columns
# -----------------------------------------------------------------------

required_cols <- c(station_id_col, "Species", "BESITO", pressure_col)
missing_cols <- setdiff(required_cols, names(SOS_data))

if(length(missing_cols) > 0){
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

if(use_biomass && !("Biomass" %in% names(SOS_data))){
  stop("use_biomass = TRUE but Biomass column is missing.")
}

if(!use_biomass && !("Abundance" %in% names(SOS_data))){
  stop("use_biomass = FALSE but Abundance column is missing.")
}

if(!("MSFD_broad_Ch" %in% names(SOS_data))){
  SOS_data$MSFD_broad_Ch <- file_name
}

# -----------------------------------------------------------------------
# Basic cleaning
# -----------------------------------------------------------------------

SOS_data[[station_id_col]] <- as.character(SOS_data[[station_id_col]])
SOS_data$Species <- as.character(SOS_data$Species)
SOS_data$BESITO <- suppressWarnings(as.numeric(as.character(SOS_data$BESITO)))
SOS_data$Pressure <- to_num_safe(SOS_data[[pressure_col]])

if("Biomass" %in% names(SOS_data)) SOS_data$Biomass <- to_num_safe(SOS_data$Biomass)
if("Abundance" %in% names(SOS_data)) SOS_data$Abundance <- to_num_safe(SOS_data$Abundance)

measure_col <- ifelse(use_biomass, "Biomass", "Abundance")
SOS_data$Measure <- SOS_data[[measure_col]]

cat("Rows before species exclusions:", nrow(SOS_data), "
")


if("AphiaID" %in% names(SOS_data) && length(aphiaid_to_remove) > 0){
  SOS_data <- SOS_data[!(SOS_data$AphiaID %in% aphiaid_to_remove), ]
}

cat("Rows after species exclusions:", nrow(SOS_data), "
")

cat("Rows before filtering valid BESITO and pressure:", nrow(SOS_data), "
")
SOS_data <- SOS_data[!is.na(SOS_data$BESITO) & !is.na(SOS_data$Pressure), ]
cat("Rows after filtering valid BESITO and pressure:", nrow(SOS_data), "
")

# -----------------------------------------------------------------------
# Define pressure classes
# -----------------------------------------------------------------------

SOS_data$Pressure_class <- make_pressure_classes(
  pressure = SOS_data$Pressure,
  use_five_classes = use_five_pressure_classes,
  reverse_direction = reverse_pressure_direction,
  reference_threshold = reference_pressure_threshold,
  no_pressure_max = no_pressure_max,
  low_pressure_max = low_pressure_max,
  medium_pressure_max = medium_pressure_max,
  high_pressure_max = high_pressure_max
)

cat("Pressure-class distribution:
")
print(table(SOS_data$Pressure_class, useNA = "ifany"))

class_counts <- table(SOS_data$Pressure_class)
if(any(class_counts == 0) || any(class_counts < 2)){
  warning(
    "One or more pressure classes are empty or contain fewer than 2 samples. ",
    "Consider simplifying the pressure classification manually for this Type 2 dataset."
  )
}

# -----------------------------------------------------------------------
# Species frequency filter
# -----------------------------------------------------------------------

HaulData <- unique(SOS_data[, c(station_id_col, "Pressure", "Pressure_class"), drop = FALSE])

HaulsWithSp <- unique(SOS_data[, c(station_id_col, "Species"), drop = FALSE])
Df_Freq <- as.data.frame(table(HaulsWithSp$Species))
Df_Freq <- Df_Freq[Df_Freq$Freq > 0, ]
Df_Freq <- Df_Freq[order(Df_Freq$Freq, decreasing = TRUE), ]
Df_Freq$Per <- (Df_Freq$Freq / nrow(HaulData)) * 100

if(species_frequency_threshold > 0){
  FinalSp_List <- Df_Freq[Df_Freq$Per > species_frequency_threshold, ]
  FinalSp_vect <- unique(as.character(FinalSp_List$Var1))
  SOS_data <- SOS_data[SOS_data$Species %in% FinalSp_vect, ]
}

cat("Number of species retained after frequency filter:", length(unique(SOS_data$Species)), "
")

# -----------------------------------------------------------------------
# Build species-by-station matrix
# -----------------------------------------------------------------------

SOS_data$LogMeasure <- log(SOS_data$Measure + 1)

matriz_msfd <- as.data.frame(tapply(
  SOS_data$LogMeasure,
  list(SOS_data$Species, SOS_data[[station_id_col]]),
  sum
))

matriz_msfd[] <- lapply(matriz_msfd, function(x) replace(x, is.na(x), 0))
matriz_msfd_invertida <- as.data.frame(t(matriz_msfd))

# -----------------------------------------------------------------------
# Build pressure group vector for TS_species_Para
# -----------------------------------------------------------------------

PressureDf <- unique(SOS_data[, c(station_id_col, "Pressure_class"), drop = FALSE])
PressureDf$Group_for_function <- make_group_vector_for_function(PressureDf$Pressure_class)

rownames(PressureDf) <- PressureDf[[station_id_col]]
PressureDf2 <- as.data.frame(PressureDf[, "Group_for_function", drop = FALSE])
rownames(PressureDf2) <- PressureDf[[station_id_col]]
names(PressureDf2) <- "Group_for_function"

matriz_msfd_invertida2 <- merge(matriz_msfd_invertida, PressureDf2, by = 0)
rownames(matriz_msfd_invertida2) <- matriz_msfd_invertida2$Row.names
matriz_msfd_invertida2$Row.names <- NULL

matriz_msfd_invertida <- matriz_msfd_invertida[rownames(matriz_msfd_invertida2), , drop = FALSE]

cat("Group distribution used by TS_species_Para:
")
print(table(matriz_msfd_invertida2$Group_for_function, useNA = "ifany"))

# -----------------------------------------------------------------------
# Run sentinel species function
# -----------------------------------------------------------------------

BESITO_Simple <- unique(SOS_data[, c("Species", "BESITO")])

SIMP_Freq_Sens <- TS_species_Para(
  sp_data = matriz_msfd_invertida,
  group_vect = matriz_msfd_invertida2$Group_for_function,
  Besito = BESITO_Simple,
  NumCor = number_of_cores
)

SIMP_Freq_Sens <- unique(as.character(SIMP_Freq_Sens))
SIMP_Freq_Sens <- SIMP_Freq_Sens[!is.na(SIMP_Freq_Sens) & SIMP_Freq_Sens != ""]
# Remove expert-excluded species ONLY from sentinel list
if(length(species_to_remove) > 0){
  SIMP_Freq_Sens <- SIMP_Freq_Sens[!(SIMP_Freq_Sens %in% species_to_remove)]
}

output_species_path <- file.path(output_folder, paste0("Species_", analysis_label, ".csv"))
write.csv(data.frame(x = SIMP_Freq_Sens), output_species_path, row.names = FALSE)

cat("Sentinel species exported to:", output_species_path, "
")
cat("Number of sentinel species:", length(SIMP_Freq_Sens), "
")

# -----------------------------------------------------------------------
# Calculate Shannon diversity and richness
# -----------------------------------------------------------------------

Lv <- as.character(unique(matriz_msfd_invertida2$Group_for_function))
ResMat <- list()

for(i in seq_along(Lv)){
  
  Mylevel <- Lv[i]
  MatByLevel <- na.omit(matriz_msfd_invertida[matriz_msfd_invertida2$Group_for_function == Mylevel, , drop = FALSE])
  
  Div <- vector()
  Rich <- vector()
  station_id <- vector()
  
  if(nrow(MatByLevel) > 0){
    for(x in 1:nrow(MatByLevel)){
      DataByRow <- MatByLevel[x, ]
      Div[x] <- safe_diversity(DataByRow)
      DataByRow_Bi <- ifelse(DataByRow > 0, 1, 0)
      Rich[x] <- sum(DataByRow_Bi, na.rm = TRUE)
      station_id[x] <- rownames(DataByRow)
    }
  }
  
  ResMat[[i]] <- cbind.data.frame(station_id, Div, Rich)
}

DivData <- do.call(rbind.data.frame, ResMat)
names(DivData) <- c(station_id_col, "Shannon", "Richness")

# -----------------------------------------------------------------------
# Calculate SoS by station/haul
# -----------------------------------------------------------------------

OnlySOS_sp <- SOS_data[SOS_data$Species %in% SIMP_Freq_Sens, ]

if(nrow(OnlySOS_sp) > 0){
  SOSData_Agg <- aggregate(
    OnlySOS_sp[, c("Measure")],
    by = list(OnlySOS_sp[[station_id_col]]),
    sum,
    na.rm = TRUE
  )
  names(SOSData_Agg) <- c(station_id_col, "Measure_TS")
} else {
  SOSData_Agg <- data.frame(
    station_id = unique(SOS_data[[station_id_col]]),
    Measure_TS = 0,
    stringsAsFactors = FALSE
  )
  names(SOSData_Agg)[1] <- station_id_col
}

total_by_station <- aggregate(
  SOS_data[, c("Measure")],
  by = list(SOS_data[[station_id_col]]),
  sum,
  na.rm = TRUE
)
names(total_by_station) <- c(station_id_col, "Total_measure")

station_info_cols <- c(
  station_id_col,
  "station",
  "station_2",
  "Pressure_class",
  "Pressure",
  "MSFD_broad_Ch",
  "lon",
  "lat",
  "depth",
  "gear",
  "sediment"
)
station_info_cols <- unique(station_info_cols[station_info_cols %in% names(SOS_data)])
station_info <- unique(SOS_data[, station_info_cols, drop = FALSE])

DataBYlance <- merge(station_info, total_by_station, by = station_id_col, all.x = TRUE)
DataBYlance <- merge(DataBYlance, SOSData_Agg, by = station_id_col, all.x = TRUE)
DataBYlance <- merge(DataBYlance, DivData, by = station_id_col, all.x = TRUE)

DataBYlance$Measure_TS[is.na(DataBYlance$Measure_TS)] <- 0
DataBYlance$SoS <- DataBYlance$Measure_TS / DataBYlance$Total_measure
DataBYlance$SoS[is.nan(DataBYlance$SoS)] <- NA
DataBYlance$SoS[is.infinite(DataBYlance$SoS)] <- NA

# Legacy-compatible names
DataBYlance$PerTS_ByWeight <- DataBYlance$SoS
DataBYlance$TrawlingEffort <- DataBYlance$Pressure
DataBYlance$TrawlingEffort_Levels <- DataBYlance$Pressure_class

if(use_biomass){
  DataBYlance$WeightTS <- DataBYlance$Measure_TS
  DataBYlance$Total_biomass_2 <- DataBYlance$Total_measure
} else {
  DataBYlance$Num_TS <- DataBYlance$Measure_TS
  DataBYlance$Total_abundance_2 <- DataBYlance$Total_measure
}

# -----------------------------------------------------------------------
# Check if the model is significant
# -----------------------------------------------------------------------
# This block follows the same structure used for Data Type 3 datasets.
#
# For Type 2 datasets, this should be interpreted only as a diagnostic
# check of whether SoS shows a significant smooth relationship with the
# pressure variable. Threshold estimation must be done later in dedicated
# pressure-state analysis scripts.
#
# IMPORTANT FOR DATA TYPE 2:
# - Data Type 2 datasets usually have fewer stations/hauls.
# - If the model cannot be fitted because there are too few observations,
#   too few unique pressure values, or no variation in SoS, this does not
#   necessarily mean the dataset is invalid.
# - It means that the response curve is not statistically supported for
#   that dataset under the current configuration.
#
# SPECIAL CASES:
# - For bottom-trawling pressure, the common pressure domain SAR <= 12 can
#   be used, as in Data Type 3.
# - For non-SAR pressure variables, such as oxygen depletion, this filter
#   should be reviewed and modified or removed.

DataBYlance_gam <- DataBYlance

# Fit the GAM only within the common pressure domain when the pressure
# variable is a SAR-like bottom-trawling pressure.
if(!reverse_pressure_direction){
  DataBYlance_gam <- DataBYlance_gam[DataBYlance_gam$TrawlingEffort <= 12, ]
}

DataBYlance_gam <- DataBYlance_gam[
  !is.na(DataBYlance_gam$TrawlingEffort) &
    !is.na(DataBYlance_gam$PerTS_ByWeight),
]

gam_significant <- NA_integer_
gam_pvalue <- NA_real_

if(nrow(DataBYlance_gam) >= 6 &&
   length(unique(DataBYlance_gam$TrawlingEffort)) >= 3 &&
   length(unique(DataBYlance_gam$PerTS_ByWeight)) >= 2){
  
  Mymodel_UBS <- gam(
    PerTS_ByWeight ~ s(TrawlingEffort, k = 3),
    data = DataBYlance_gam,
    family = binomial,
    method = "REML"
  )
  
  gam_plot_path <- file.path(
    output_folder,
    paste0("GAM_diagnostic_", analysis_label, ".png")
  )
  
  png(filename = gam_plot_path, width = 1600, height = 1200, res = 150)
  plot(Mymodel_UBS, shade = TRUE, seWithMean = TRUE)
  dev.off()
  
  summary(Mymodel_UBS)
  
  # Extract p-value of the smooth term
  gam_summary <- summary(Mymodel_UBS)
  gam_pvalue <- gam_summary$s.table[1, "p-value"]
  
  # Create binary flag: 1 = significant, 0 = not significant
  gam_significant <- ifelse(!is.na(gam_pvalue) && gam_pvalue < 0.05, 1, 0)
  
} else {
  warning(
    "GAM diagnostic was not fitted: not enough observations, unique pressure values, or SoS variation."
  )
}

# Add the flag to the full exported dataset
DataBYlance$gam_significant <- gam_significant
DataBYlance$gam_pvalue <- gam_pvalue

# -----------------------------------------------------------------------
# Export SoS-ready data
# -----------------------------------------------------------------------

output_data_path <- file.path(output_folder, paste0("Data_Ready_", analysis_label, ".csv"))
write.csv(DataBYlance, output_data_path, row.names = FALSE)
cat("SoS-ready data exported to:", output_data_path, "
")

# -----------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------

# Build file path for the plot
plot_output_path <- file.path(
  output_folder,
  paste0("plot_msfd_", analysis_label, ".png")
)

# Open PNG device
png(filename = plot_output_path, width = 2000, height = 1200, res = 150)

# Set layout
par(mfrow = c(2, 3))

# Generate plots
plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$PerTS_ByWeight,
     main = "Proportion of sentinel species")

if(use_biomass){
  plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$WeightTS,
       main = "Sentinel species biomass")
  plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Total_biomass_2,
       main = "Total biomass")
} else {
  plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Num_TS,
       main = "Sentinel species abundance")
  plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Total_abundance_2,
       main = "Total abundance")
}

plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Shannon,
     main = "Shannon index")
plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Richness,
     main = "Richness")
plot(DataBYlance$TrawlingEffort, DataBYlance$PerTS_ByWeight,
     main = "SoS vs pressure", xlab = pressure_col, ylab = "SoS")

# Close device to save the plot
dev.off()

# Boxplot of SoS by pressure class
msfd_box <- ggplot(DataBYlance) +
  aes(x = TrawlingEffort_Levels, y = PerTS_ByWeight) +
  geom_boxplot(
    fill = "#3FBCC8",
    alpha = 0.2,
    notch = FALSE,
    outlier.colour = "red",
    outlier.fill = "red",
    outlier.size = 3
  ) +
  labs(
    x = "Pressure class",
    y = "Sentinel species proportion",
    title = file_name,
    subtitle = paste0("Reference threshold: ", reference_pressure_threshold)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 20L,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 17L,
      hjust = 0.5
    ),
    axis.title.y = element_text(size = 14L),
    axis.title.x = element_text(size = 14L)
  ) +
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "gray86")
  )

fig_path <- file.path(
  output_folder,
  paste0("Box_plot_propor_SSP_", analysis_label, ".pdf")
)

ggsave(fig_path, msfd_box, width = 8, height = 6)

# -----------------------------------------------------------------------
# Processing summary
# -----------------------------------------------------------------------

processing_summary <- data.frame(
  dataset = file_name,
  station_id_col = station_id_col,
  pressure_col = pressure_col,
  use_biomass = use_biomass,
  n_records = nrow(SOS_data),
  n_stations = length(unique(SOS_data[[station_id_col]])),
  n_species = length(unique(SOS_data$Species)),
  n_sentinel_species = length(SIMP_Freq_Sens),
  gam_significant = gam_significant,
  gam_pvalue = gam_pvalue,
  pressure_classes = paste(names(table(SOS_data$Pressure_class)), table(SOS_data$Pressure_class), sep = "=", collapse = "; "),
  stringsAsFactors = FALSE
)

summary_path <- file.path(output_folder, paste0("Processing_summary_", analysis_label, ".csv"))
write.csv(processing_summary, summary_path, row.names = FALSE)

cat("Processing summary exported to:", summary_path, "
")
cat("Finished:", file_name, "
")
