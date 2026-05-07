# =======================================================================
# Identification of Sentinel Species for SoS in Data Type 1 datasets
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script calculates sentinel species lists and SoS values for
# prepared Data Type 1 datasets.
#
# It is designed to run on both EPIFAUNA and INFAUNA datasets.
# Users should specify the data_type variable ("EPIFAUNA" or "INFAUNA").
# The script will then read all prepared datasets from the corresponding
# folder and generate one output folder per dataset.
#
# -----------------------------------------------------------------------
# Methodological note: adaptation of sentinel species selection for
# Data Type 1 datasets
# -----------------------------------------------------------------------
# Data Type 1 datasets do not include pressure classes and therefore do
# not allow the definition of a low-pressure reference group as in
# Data Types 2-3.
#
# For this reason, sentinel species selection was adapted accordingly.
# Instead of using the standard SIMPER- and pressure-group-based
# procedure, sentinel species are identified here using a frequency-based
# approach.
#
# Species are first screened according to their occurrence frequency
# across samples within the time series, applying a minimum frequency
# threshold that scales with sample size. Among those meeting the
# frequency criterion, species with higher BESITO sensitivity scores are
# prioritised sequentially (from BESITO 5 downwards) until a minimum
# sentinel list size is achieved.
#
# This adaptation is intended to identify species that are both typical
# of the monitored community (frequently occurring) and potentially
# sensitive.
#
# Under standard conditions, a minimum of 10 sentinel species is used
# (or 5 when the availability of highly sensitive species is limited).
# However, in some data-poor infaunal Data Type 1 datasets, limited
# BESITO coverage substantially restricts the number of candidate
# sentinel species. In such cases, the minimum number of sentinel
# species required for SoS calculation may be reduced to 2 in order to
# retain these datasets in the analysis.
#
# The resulting sentinel species list should be reviewed by regional
# experts prior to final use. Species considered non-representative of
# habitat condition (e.g. parasitic, invasive, or otherwise unsuitable
# taxa) may be excluded from the final sentinel list based on expert
# ecological judgement.
#
# This adaptation allows SoS values to be calculated for data-limited
# infaunal habitats, but entails increased uncertainty. Results based on
# fewer than 3 sentinel species should therefore be interpreted with
# particular caution, and regarded as indicative rather than robust.
#
# SoS is then calculated as the proportion of the community represented
# by sentinel species at each station-year, using biomass when available
# and abundance otherwise.
# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx
# - vegan
# - pbapply
# - parallel
# - dplyr
# - ggplot2

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(vegan)
library(ggplot2)
library(pbapply)
library(parallel)
library(dplyr)

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

input_folder <- file.path("../prepared_data", paste0(data_type, "_DATA"))

all_files <- list.files(input_folder, pattern = "_data_ready\\.xlsx$", full.names = TRUE)

if(length(all_files) == 0){
  stop(paste("No prepared data files found in:", input_folder))
}

output_base_folder <- file.path("../sos_output", data_type)

if(!dir.exists(output_base_folder)){
  dir.create(output_base_folder, recursive = TRUE)
}
# -----------------------------------------------------------------------
# SoS function adapted for Data Type 1 datasets
# -----------------------------------------------------------------------

TS_species_Adapted_Type1 <- function(sp_data, Besito, table){
  
  
  # Frequency calculation
  SpNames <- colnames(sp_data)
  Freq <- vector()
  Per <- vector()
  for( i in 1:(ncol(sp_data))){
    ColOnly <- sp_data[,i]
    ColOnly_Bi <- ifelse(ColOnly>0,1,0)
    Freq[i] <- sum(ColOnly_Bi)
    Per[i] <- (sum(ColOnly_Bi)/nrow(sp_data))*100
  }
  
  MyFreqMatrix <- cbind.data.frame(SpNames, Freq, Per)
  Th <- ifelse((nrow(sp_data)/10)<=2,2,round((nrow(sp_data)/10)))
  MyFreqMatrix <- MyFreqMatrix[MyFreqMatrix$Freq>=Th,]
  names(MyFreqMatrix) <- c("Species", "Freq", "Per")
  
  MyFreqMatrixWithBesito <- unique(merge(MyFreqMatrix, Besito, by="Species"))
  MyFreqMatrixWithBesito <- MyFreqMatrixWithBesito[order(MyFreqMatrixWithBesito$Freq, decreasing=TRUE),]
  
  # Select species with BESITO sensitivity = 5
  
  SIM_Freq_Sens <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==5,]
  
  Mns <- "10 Species reached after include species with a sensitive of 5 from SIMPER"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens5_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==5,]
  Sp_Vect5 <- unique(as.character(Sens5_Freq$Species))
  MyCut<- ifelse(length(Sp_Vect5)>=10,10,length(Sp_Vect5))
  MyCutSp <- Sp_Vect5[MyCut]
  MyCut_df <- Sens5_Freq[Sens5_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens5_Freq[Sens5_Freq$Freq>=MyCut_Frq,]
  SIM_Freq_Sens5 <- unique(as.character(MyMat$Species))
  SIM_Freq_Sens <- SIM_Freq_Sens5[SIM_Freq_Sens5!="integer(0)"]
  
  Mns <- "10 Species reached after include species with a sensitive of 4 ordered by Frequency"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens4_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==4,]
  Sp_Vect4 <- unique(as.character(Sens4_Freq$Species))
  MyCut<- ifelse(length(Sp_Vect4)>=10,10,length(Sp_Vect4))
  MyCutSp <- Sp_Vect4[MyCut]
  MyCut_df <- Sens4_Freq[Sens4_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens4_Freq[Sens4_Freq$Freq>=MyCut_Frq,]
  AcumSp_Sens <-SIM_Freq_Sens5
  SIM_Freq_Sens4 <- c(AcumSp_Sens, unique(as.character(MyMat$Species)))
  SIM_Freq_Sens <- SIM_Freq_Sens4[SIM_Freq_Sens4!="integer(0)"]
  
  Mns <- "10 Species reached after include species with a sensitive of 4 ordered by Frequency"
  
  if (length(SIM_Freq_Sens)>=10) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens3_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==3,]
  Sp_Vect3 <- unique(as.character(Sens3_Freq$Species))
  MyCut<- ifelse(length(Sp_Vect3)>=10,10,length(Sp_Vect3))
  MyCutSp <- Sp_Vect3[MyCut]
  MyCut_df <- Sens3_Freq[Sens3_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens3_Freq[Sens3_Freq$Freq>=MyCut_Frq,]
  AcumSp_Sens <-SIM_Freq_Sens4
  SIM_Freq_Sens3 <- c(AcumSp_Sens, unique(as.character(MyMat$Species)))
  SIM_Freq_Sens <- SIM_Freq_Sens3[SIM_Freq_Sens3!="integer(0)"]
  
  Mns <- "10 Species reached after include species with a sensitive of 3 ordered by Frequency"
  
  
  if (length(SIM_Freq_Sens)>=5) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Sens2_Freq <-  MyFreqMatrixWithBesito[MyFreqMatrixWithBesito$BESITO==2,]
  Sp_Vect2 <- unique(as.character(Sens2_Freq$Species))
  MyCut<- ifelse(length(Sp_Vect2)>=10,10,length(Sp_Vect2))
  MyCutSp <- Sp_Vect2[MyCut]
  MyCut_df <- Sens2_Freq[Sens2_Freq$Species==MyCutSp,]
  MyCut_Frq <- MyCut_df$Freq
  MyMat <-   Sens2_Freq[Sens2_Freq$Freq>=MyCut_Frq,]
  AcumSp_Sens <-SIM_Freq_Sens3
  SIM_Freq_Sens2 <- c(AcumSp_Sens, unique(as.character(MyMat$Species)))
  SIM_Freq_Sens <- SIM_Freq_Sens2[SIM_Freq_Sens2!="integer(0)"]
  
  Mns <- "5 Species reached after include species with a sensitive of 2 ordered by Frequency"
  if (length(SIM_Freq_Sens) >= 5) {
    print(Mns)
    return(SIM_Freq_Sens)
  }
  
  Mns <- "Careful: Loop finished without reach minimum level of species"
  print(Mns)
  return(SIM_Freq_Sens)
}

# -----------------------------------------------------------------------
# Loop through all prepared datasets
# -----------------------------------------------------------------------

for(input_file in all_files){
  
  SOS_data <- read_excel(input_file)
  
  file_name <- gsub("_data_ready\\.xlsx$", "", basename(input_file))
  
  output_folder <- file.path(output_base_folder, file_name)
  
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  cat("\n----------------------------------------\n")
  cat("Processing dataset:", file_name, "\n")

  # -----------------------------------------------------------------------
  # Optional expert-based exclusion of species
  # -----------------------------------------------------------------------
  # The resulting sentinel species list should be reviewed by regional
  # experts prior to final use.
  #
  # Species considered non-representative of habitat condition
  # (e.g. parasitic, invasive or otherwise unsuitable taxa)
  # may be excluded from the final sentinel list based on expert
  # ecological judgement.
  #
  # Examples of manual exclusions are shown below.
  # -----------------------------------------------------------------------
nrow(SOS_data)
# Example expert-based exclusions
  
SOS_data <- SOS_data[SOS_data$Species != "Epizoanthus paguriphilus", ] 
SOS_data <- SOS_data[SOS_data$Species != "Calliactis parasitica", ]
nrow(SOS_data)
# -----------------------------------------------------------------------
# Filtering: removal of rows with NA values in
# BESITO 
# -----------------------------------------------------------------------

# Number of rows before filtering
nrow(SOS_data)

# Remove rows with NA in BESITO 
SOS_data <- SOS_data[!is.na(SOS_data$BESITO), ]

# Number of rows after filtering
nrow(SOS_data)

# 1. Filter only haul-related data
HaulData <- unique(SOS_data[, c("station_2", "lon", "lat")])

# 2. Filter species with frequency > 0.5% (if applicable)
HaulsWithSp <- unique(SOS_data[, c("station_2", "Species", "lon", "lat")])
Df_Freq <- as.data.frame(table(HaulsWithSp$Species))
Df_Freq <- Df_Freq[Df_Freq$Freq > 0, ]
Df_Freq <- Df_Freq[order(Df_Freq$Freq, decreasing = TRUE), ]
Df_Freq$Per <- (Df_Freq$Freq / nrow(HaulData)) * 100
#
# Get the list of species with more than 0.5% frequency
FinalSp_List <- Df_Freq[Df_Freq$Per > 0.5, ]
FinalSp_vect <- unique(FinalSp_List$Var1)

# Filter the data to keep only relevant species
SOS_data <- SOS_data[SOS_data$Species %in% FinalSp_vect, ]


# 3. Generate species data matrix for the current habitat
SOS_data$LogPeso <- log(SOS_data$Biomass + 1)
matriz_msfd <- as.data.frame(tapply(SOS_data$LogPeso, list(SOS_data$Species, SOS_data$station_2), sum))
matriz_msfd[] <- lapply(matriz_msfd, function(x) replace(x, is.na(x), 0))
matriz_msfd_invertida <- as.data.frame(t(matriz_msfd))
#


#
# Extract BESITO data
BESITO_Simple <- unique(SOS_data[, c("Species", "BESITO")])

# 4. Run the TS_species function for the current habitat
SIMP_Freq_Sens <- TS_species_Adapted_Type1(matriz_msfd_invertida, BESITO_Simple)

output_path <- file.path(
  output_folder,
  paste0("Species_", file_name, ".csv")
)

write.csv(SIMP_Freq_Sens, output_path, row.names = FALSE)
#
print(paste("Sentinel species for", file_name, "has been exported to:", output_path))
#
#

# -----------------------------------------------------------------------
# OUTPUT PREPARING BY MSFD ####
# Add to the MSFD matrix: information on sentinel species proportion, 
# calculation of diversity, richness, etc.
# -----------------------------------------------------------------------

SOS_Species <- read.csv(output_path, stringsAsFactors = FALSE)

# Open sentinel species list
SOS_Species <- SOS_Species$x




OnlySOS_sp <-  SOS_data[SOS_data$Species%in%SOS_Species,]
head(OnlySOS_sp)
nrow(OnlySOS_sp)

NoTSOS_sp<-  SOS_data[which(!SOS_data$station_2%in%OnlySOS_sp$station_2),]
NoTSOS_sp$WeightTS <- 0
NoTSOS_sp$Num_TS <- 0

#We aggregate by haul

SOSData_Agg <- aggregate(OnlySOS_sp [,c("Biomass", "Abundance")], by=list(OnlySOS_sp$station_2),sum)
head(SOSData_Agg)
nrow(SOSData_Agg)

names(SOSData_Agg) <- c("station_2", "WeightTS", "Num_TS")



#Adding All the info
DataOnlyWithHab_4 <- merge(SOSData_Agg, SOS_data, by="station_2")
DataOnlyWithHab_4 <- rbind(DataOnlyWithHab_4,NoTSOS_sp)

head(DataOnlyWithHab_4)
nrow(DataOnlyWithHab_4)

# ----------------------------------------------------------------------- 
# Calculate proportion of target species:
# - Use WeightTS / Total_biomass if WeightTS is available
# - Otherwise, use NUM TS / Total_abundance if no weight data
# ----------------------------------------------------------------------- 

DataOnlyWithHab_4$WeightTS[is.na(DataOnlyWithHab_4$WeightTS)] <- 0
DataOnlyWithHab_4$Total_biomass <- as.numeric(as.character(DataOnlyWithHab_4$Total_biomass))

# Recalculate Total_biomass_2 as the sum of  biomass by station
DataOnlyWithHab_4$Total_biomass_2 <- ave(
  DataOnlyWithHab_4$Biomass,
  DataOnlyWithHab_4$station_2,
  FUN = sum
)
DataOnlyWithHab_4$PerTS_ByWeight <- DataOnlyWithHab_4$WeightTS/DataOnlyWithHab_4$Total_biomass_2
names(DataOnlyWithHab_4)

DataSubset <- subset(DataOnlyWithHab_4, select=c( "WeightTS", "Num_TS", "PerTS_ByWeight","Total_biomass_2",
                                                 "station_2", "lon", "lat"))

nrow(DataSubset)

DataBYlance <- unique(DataSubset)



#

# -----------------------------------------------------------------------
# # Export data
# -----------------------------------------------------------------------



output_path <- file.path(
  output_folder,
  paste0("Data_Ready_", file_name, ".csv")
)

write.csv(DataBYlance, output_path, row.names = FALSE)
cat("Finished:", file_name, "\n")
}