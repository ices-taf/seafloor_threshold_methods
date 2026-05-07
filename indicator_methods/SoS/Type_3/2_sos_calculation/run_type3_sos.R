# =======================================================================
# Identification of Sentinel Species and SoS calculation for Data Type 3 datasets
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script identifies sentinel species lists and calculates SoS values
# for prepared Data Type 3 datasets.
#
# It is intended to run on one prepared dataset at a time. The user must
# specify the input_file corresponding to the dataset of interest.
# The script then creates one output folder, named after the input file,
# where all habitat-specific outputs are saved.
#
# Data Type 3 datasets include trawling pressure information and allow
# sentinel species to be defined under low- or negligible-pressure
# reference conditions. Once the sentinel species list is established
# for each habitat, SoS is calculated as the proportion of the community
# represented by sentinel species at each haul, using biomass when
# available and abundance otherwise.

# -----------------------------------------------------------------------
# Methodological note: sentinel species selection and SoS calculation
# for Data Type 3 datasets
# -----------------------------------------------------------------------
# Data Type 3 datasets are characterised by relatively high sampling
# coverage across habitats and trawling pressure gradients, allowing
# SoS to be calculated under reference conditions and evaluated along
# a pressure gradient.
#
# Only habitats with more than 20 hauls are retained for SoS analysis.
# For each retained habitat, sentinel species are defined using samples
# under low or negligible trawling pressure.
#
# The reference-pressure class should be selected stepwise:
#   1. SAR <= 0.10
#   2. if fewer than 4 hauls are available, use SAR <= 0.33
#   3. if fewer than 4 hauls are still available, use SAR <= 0.65
#
# The selected threshold should be the lowest one providing at least
# 4 hauls in the reference group.
#
# Sentinel species are identified using SIMPER- and frequency-based
# procedures, prioritising species with higher BESITO sensitivity scores.
# Under standard conditions, a minimum of 10 sentinel species is used,
# or 5 when the availability of highly sensitive species is limited.
#
# Once the sentinel species list has been defined for a given habitat,
# SoS is calculated for each haul as the proportion of total biomass
# represented by sentinel species. Where biomass is not available or is
# considered unsuitable, abundance-based proportions may be used instead.
#
# The resulting sentinel species list should be reviewed by regional
# experts prior to final use. Species considered non-representative of
# habitat condition (e.g. parasitic, invasive, or otherwise unsuitable
# taxa) may be excluded from the final sentinel list based on expert
# ecological judgement.

# -----------------------------------------------------------------------
# Input / output description
# -----------------------------------------------------------------------
# INPUT:
# - One prepared Data Type 3 dataset produced by the data preparation
#   workflow.
# - The input file must contain, at minimum:
#     * station- or haul-level identifiers
#     * habitat information
#     * species information
#     * biomass and/or abundance
#     * trawling pressure information (e.g. SAR5, SAR3 or SAR1)
#
# - An external BESITO sensitivity table is required. This table must
#   contain at least:
#     * Species names (matching those in the dataset)
#     * BESITO sensitivity scores (1-5)
#
#   Species without an assigned BESITO score should be assigned a value
#   of 1 (lowest sensitivity), ensuring they are not selected as sentinel
#   species (only species with sensitivity >= 2 are eligible).
#
# OUTPUT:
# - One output folder per input dataset
# - Sentinel species list for each analysed habitat
# - SoS-ready dataset for each analysed habitat
# - Diagnostic plots and exploratory summaries, where applicable

# -----------------------------------------------------------------------
# NOTE ON STATION IDENTIFIERS
# -----------------------------------------------------------------------
# Some datasets (e.g. NS_BE and NS_NL) contain both "station" and "station_2".
# In these cases, "station_2" should be used as the correct haul identifier.
#
# To handle this, manually set the variable `use_station_2` to TRUE
# for those datasets. Otherwise, "station" will be used by default.

# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - readxl
# - openxlsx
# - vegan
# - ggplot2
# - mgcv


# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(vegan)
library(ggplot2)
library(mgcv)


# -----------------------------------------------------------------------
# Clear workspace
# -----------------------------------------------------------------------

rm(list = ls())
gc()

# -----------------------------------------------------------------------
# Input and output paths
# -----------------------------------------------------------------------

# INPUT:
# This script is designed to run on one prepared Data Type 3 dataset at a time.
# To analyse a different dataset, modify the input_file path accordingly.
input_file <- file.path(
  "../prepared_data/EPIFAUNA_DATA",
  "BoBIC_IberianChabitats_data_ready.xlsx"
)

if(!file.exists(input_file)){
  stop(paste("Input file not found:", input_file))
}

SOS_data <- read_excel(input_file)

# OUTPUT:
# Results are organised in a dataset-specific folder.
# A new folder is created for each input dataset, named after the file.
# Within this folder, all outputs are stored separately for each habitat,
# including:
#   - sentinel species lists
#   - SoS-ready datasets
#   - diagnostic plots
#
# Folder structure example:
# sos_output/
#   ????????? EPIFAUNA/
#       ????????? BoBIC_IberianChabitats/
#           ????????? Species_UBatSed.csv
#           ????????? Data_Ready_UBatSed.csv
#           ????????? plot_msfd_UBatSed.png
#           ????????? Box_plot_propor_SSP_UBatSed.pdf
#           ????????? ...

file_name <- gsub("_data_ready.xlsx$", "", basename(input_file))

output_base_folder <- file.path("../sos_output", "EPIFAUNA")

if(!dir.exists(output_base_folder)){
  dir.create(output_base_folder, recursive = TRUE)
}

output_folder <- file.path(output_base_folder, file_name)

if(!dir.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)
}
# -----------------------------------------------------------------------
# # Sentinel species  function
# -----------------------------------------------------------------------


TS_species_Para <- function(sp_data, group_vect, Besito, NumCor, table){
  
  #### Pairwise combinations within groups using SIMPER
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
    
    # Order species by similarity
    z2$Similarity <- as.numeric(as.character(z2$Similarity))
    db<-z2[order(z2$Similarity,decreasing = TRUE),]
    b<-sum(db$Similarity)
    ##contribution to mean similarity
    db$contribution<-((100*db$Similarity)/b)
    
    # Calculate cumulative mean similarity (%) and apply the 90% cutoff
    
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
  
  # Select species from group 1
  Sp_Group1 <- table_final[table_final$Group=="1",]
  Sp_Group1_vect <- Sp_Group1$Species
  names(Besito) <- c("Species", "BESITO")
  Besito_SIMPER <- as.data.frame(unique(Besito[Besito$Species%in%Sp_Group1_vect,]))
  
  # Frequency calculation
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
  
  # Select species with sensitivity score 5
  
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
  
  # Select species with sensitivity score 4
  
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
  
  # Select species with sensitivity score 3
  
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
  
  return(SIM_Freq_Sens2)
  Mns <- "Careful: Loop finished without reaching the minimum number of species required (5)."
  warning(Mns)
  return(NULL) # Return NULL or partial list if minimum isn't met
}

# -----------------------------------------------------------------------
# Remove species considered non-representative of habitat condition
# (e.g. parasitic or otherwise unsuitable taxa) based on expert
# ecological judgement.
# -----------------------------------------------------------------------

nrow(SOS_data)
SOS_data <- SOS_data[SOS_data$Species != "Epizoanthus paguriphilus", ] 
nrow(SOS_data)
SOS_data <- SOS_data[SOS_data$Species != "Calliactis parasitica", ]
nrow(SOS_data)

# -----------------------------------------------------------------------
# Define Trawling Effort column, prioritizing SAR5, SAR3, SAR1
# -----------------------------------------------------------------------
# 
# Define trawling pressure, prioritising SAR5, then SAR3,
# then SAR2009, and finally SAR1.
# The first available metric is used.

SOS_data$TrawlingEffort <- ifelse(
  !is.na(SOS_data$SAR5), SOS_data$SAR5,
  ifelse(!is.na(SOS_data$SAR3), SOS_data$SAR3,
         ifelse(!is.na(SOS_data$SAR2009), SOS_data$SAR2009,
                ifelse(!is.na(SOS_data$SAR1), SOS_data$SAR1, NA)))
)
# -----------------------------------------------------------------------
# Define the effort levels in text
# -----------------------------------------------------------------------

# For Data Type 3 datasets, the reference-pressure class used to define
# sentinel species should be selected stepwise.
#
# Start with SAR <= 0.10 as the reference ("No Effort") class.
# If fewer than 4 hauls are available in this class for the habitat under
# analysis, relax the criterion to SAR <= 0.33.
# If fewer than 4 hauls are still available, relax further to SAR <= 0.65.
#
# Only habitats with more than 20 hauls in total should be considered for
# SoS analysis.
#
# The code below shows one example using a selected reference threshold.
# Repeat the same workflow for each habitat that meets the minimum
# requirement of 20 hauls, choosing the lowest reference threshold
# (0.10, 0.33, or 0.65) that provides at least 4 hauls in the reference
# group.


 SOS_data$TrawlingEffort_Levels <- SOS_data$TrawlingEffort
#
#
 SOS_data$TrawlingEffort_Levels <- ifelse(SOS_data$TrawlingEffort <= 0.1, "No Effort",
                                          ifelse(SOS_data$TrawlingEffort > 0.1 & SOS_data$TrawlingEffort <= 1.2, "Low Effort",
                                                 ifelse(SOS_data$TrawlingEffort > 1.2 & SOS_data$TrawlingEffort <= 3, "Medium Effort",
                                                        ifelse(SOS_data$TrawlingEffort > 3 & SOS_data$TrawlingEffort <= 6, "High Effort", "Very High Effort"))))

#
 
 # -----------------------------------------------------------------------
 # Filtering: removal of rows with NA values in
 # BESITO and Trawling Effort
 # -----------------------------------------------------------------------
 
 # Number of rows before filtering
 nrow(SOS_data)
 
 # Remove rows with NA in BESITO or TrawlingEffort
 SOS_data <- SOS_data[!is.na(SOS_data$BESITO) & !is.na(SOS_data$TrawlingEffort), ]
 
 # Number of rows after filtering
 nrow(SOS_data)
 
 
 # Check the effort levels distribution
#
 table(SOS_data$TrawlingEffort_Levels)

 # -----------------------------------------------------------------------
 # Extract matrix with information only for the hauls
 # -----------------------------------------------------------------------
 
 names(SOS_data)
 # Set the correct variable, station_2 for NS_NL or NS_BE datasets
 OnlyHauls <- unique(SOS_data[,c("station", "TrawlingEffort", "TrawlingEffort_Levels", "depth", "MSFD_broad_Ch", "lon", "lat")])
#
# # Check the number of rows in the resulting dataset
 nrow(OnlyHauls)
#
 # Count the number of hauls per habitat (MSFD_broad_Ch).
 
 habitat_counts <- table(OnlyHauls$MSFD_broad_Ch)
#
 # For Data Type 3 datasets, only habitats with more than 20 hauls
 # should be retained for SoS analysis. 
 habitats_to_keep <- names(habitat_counts[habitat_counts > 20])
#
 # Keep only habitats fulfilling the minimum requirement of 20 hauls
 OnlyHauls_filtered <- OnlyHauls[OnlyHauls$MSFD_broad_Ch %in% habitats_to_keep, ]

# # Visualize the filtered habitats
 table(OnlyHauls_filtered$MSFD_broad_Ch)
#
 
 
 #_______________________________________________________________________
 
 # # -----------------------------------------------------------------------
 # Example workflow for one habitat
 # -----------------------------------------------------------------------
 # The following block illustrates the SoS workflow for a single habitat.
 #
 # The same workflow should be repeated for each habitat retained after
 # filtering (i.e. habitats with more than 20 hauls).
 #
 # For each habitat, the reference-pressure threshold used to define
 # "No Effort" should be chosen stepwise:
 #   1. SAR <= 0.10
 #   2. if fewer than 4 hauls are available, use SAR <= 0.33
 #   3. if fewer than 4 hauls are still available, use SAR <= 0.65
 #
 # The selected threshold should be the lowest one providing at least
 # 4 hauls in the reference group.
 
 #________________________________________________________________________
 SOS_data$TrawlingEffort_Levels <- SOS_data$TrawlingEffort
 
 
 SOS_data$TrawlingEffort_Levels <- ifelse(SOS_data$TrawlingEffort <= 0.1, "No Effort", 
                                          ifelse(SOS_data$TrawlingEffort > 0.1 & SOS_data$TrawlingEffort <= 1.2, "Low Effort", 
                                                 ifelse(SOS_data$TrawlingEffort > 1.2 & SOS_data$TrawlingEffort <= 3, "Medium Effort", 
                                                        ifelse(SOS_data$TrawlingEffort > 3 & SOS_data$TrawlingEffort <= 6, "High Effort", "Very High Effort"))))
 
 
 
 # In this example, SAR <= 0.10 was retained as the reference threshold,
 # as it provided at least 4 hauls in the reference group for this habitat.
 # -----------------------------------------------------------------------
 # # RUN THE FUNCTION FOR EACH HABITAT (msfd_bht) ####
 # -----------------------------------------------------------------------
 
 # Get unique MSFD_BHT values from the filtered data
 msfd_bht <- unique(OnlyHauls_filtered$MSFD_broad_Ch)[1]  # First habitat

 # Filter the data for the specific habitat
 msfd_data <- SOS_data[SOS_data$MSFD_broad_Ch == msfd_bht, ]
 print(paste("Number of records for", msfd_bht, ":", nrow(msfd_data)))

 # 1. Filter only haul-related data
 HaulData <- unique(msfd_data[, c("station", "lon", "lat", "TrawlingEffort", "TrawlingEffort_Levels")])
 print(paste("Number of hauls for", msfd_bht, ":", nrow(HaulData)))

 # 2. Filter species with frequency > 0.5% (if applicable)
 HaulsWithSp <- unique(msfd_data[, c("station", "Species", "lon", "lat", "TrawlingEffort", "TrawlingEffort_Levels")])
 Df_Freq <- as.data.frame(table(HaulsWithSp$Species))
 Df_Freq <- Df_Freq[Df_Freq$Freq > 0, ]
 Df_Freq <- Df_Freq[order(Df_Freq$Freq, decreasing = TRUE), ]
 Df_Freq$Per <- (Df_Freq$Freq / nrow(HaulData)) * 100
#
 # Get the list of species with more than 0.5% frequency
 FinalSp_List <- Df_Freq[Df_Freq$Per > 0.5, ]
 FinalSp_vect <- unique(FinalSp_List$Var1)

 # Filter the data to keep only relevant species
 msfd_data <- msfd_data[msfd_data$Species %in% FinalSp_vect, ]
 table(msfd_data$TrawlingEffort_Levels)

 # 3. Generate species data matrix for the current habitat
 msfd_data$LogPeso <- log(msfd_data$Biomass + 1)
 matriz_msfd <- as.data.frame(tapply(msfd_data$LogPeso, list(msfd_data$Species, msfd_data$station), sum))
 matriz_msfd[] <- lapply(matriz_msfd, function(x) replace(x, is.na(x), 0))
 matriz_msfd_invertida <- as.data.frame(t(matriz_msfd))
#
# # Generate fishing effort levels matrix for the 0.1 threshold
 FishingEffortDf <- unique(msfd_data[, c("station", "TrawlingEffort_Levels")])
 FishingEffortDf$TrawlingEffort_Levels <- as.character(FishingEffortDf$TrawlingEffort_Levels)
 FishingEffortDf$TrawlingEffort_Levels[FishingEffortDf$TrawlingEffort_Levels == "No Effort"] <- "1"
 FishingEffortDf$TrawlingEffort_Levels[FishingEffortDf$TrawlingEffort_Levels == "Low Effort"] <- "2"
 FishingEffortDf$TrawlingEffort_Levels[FishingEffortDf$TrawlingEffort_Levels == "Medium Effort"] <- "3"
 FishingEffortDf$TrawlingEffort_Levels[FishingEffortDf$TrawlingEffort_Levels == "High Effort"] <- "4"
 FishingEffortDf$TrawlingEffort_Levels[FishingEffortDf$TrawlingEffort_Levels == "Very High Effort"] <- "5"
#
 table(FishingEffortDf$TrawlingEffort_Levels)
#
#
# # Create a numeric vector for effort levels
 rownames(FishingEffortDf) <- FishingEffortDf$station
 FishingEffortDf2 <- as.data.frame(FishingEffortDf[, 2])
 rownames(FishingEffortDf2) <- FishingEffortDf$station
 names(FishingEffortDf2) <- "TrawlingEffort_Levels"
#
# # Merge the matrices (species data and effort data)
 matriz_msfd_invertida2 <- merge(matriz_msfd_invertida, FishingEffortDf2, by = 0)
#
 # Extract BESITO data
 BESITO_Simple <- unique(msfd_data[, c("Species", "BESITO")])

 # 4. Run the TS_species function for the current habitat
 SIMP_Freq_Sens <- TS_species_Para(matriz_msfd_invertida, matriz_msfd_invertida2$TrawlingEffort_Levels, BESITO_Simple,16)
 
 # 5. Export the list of sentinel species for the current habitat
 
 
 output_path <- file.path(
   output_folder,
   paste0("Species_", msfd_bht, ".csv")
 )
 
 write.csv(SIMP_Freq_Sens, output_path, row.names = FALSE)
#
 print(paste("Sentinel species for", msfd_bht, "has been exported to:", output_path))
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
 
 
 # -----------------------------------------------------------------------
  # Calculation of diversity, richness, etc.
 # ----------------------------------------------------------------------- 
 
 Lv <- as.character(unique(matriz_msfd_invertida2$TrawlingEffort_Levels))
 ResMat <- list()
 
 for (i in 1:length(Lv)){
   
   Mylevel <- Lv[i]
   MatByLevel<- na.omit(matriz_msfd_invertida[matriz_msfd_invertida2$TrawlingEffort_Levels==Mylevel, ])
   
   Div <- vector()
   Rich <- vector()
   station <- vector()
   
   for (x in 1:nrow(MatByLevel))
   {
     DataByRow <- MatByLevel[x,]
     Div[x] <- diversity(DataByRow)
     DataByRow_Bi <- ifelse(DataByRow>0,1,0)
     Rich[x] <- sum(DataByRow_Bi)
     station[x] <- rownames(DataByRow)
   }
   temp <- cbind.data.frame(station, Div, Rich)
   names(temp) <- c("station", "Shannon", "Richness")
   ResMat[[i]] <- cbind.data.frame(station, Div, Rich)
   
 }
 
 DivData <- do.call(rbind.data.frame, ResMat)  
 
 
 # Select only rows with sentinel species in our data matrix 
 
 
 OnlySOS_sp <-  msfd_data[msfd_data$Species%in%SOS_Species,]
 head(OnlySOS_sp)
 nrow(OnlySOS_sp)
 
 NoTSOS_sp<-  msfd_data[which(!msfd_data$station%in%OnlySOS_sp$station),]
 
 #We aggregate by haul
 
 SOSData_Agg <- aggregate(OnlySOS_sp [,c("Biomass", "Abundance")], by=list(OnlySOS_sp$station),sum)
 head(SOSData_Agg)
 nrow(SOSData_Agg)
 
 names(SOSData_Agg) <- c("station", "WeightTS", "Num_TS")
 
 NoTSOS_sp$WeightTS <- 0
 NoTSOS_sp$Num_TS <- 0
 
 #Adding All the info
 DataOnlyWithHab_4 <- merge(SOSData_Agg, msfd_data, by="station")
 DataOnlyWithHab_4 <- rbind(DataOnlyWithHab_4,NoTSOS_sp)
 
 head(DataOnlyWithHab_4)
 nrow(DataOnlyWithHab_4)
 
 # ----------------------------------------------------------------------- 
 # Calculate the proportion of sentinel species.
 # Use sentinel-species biomass relative to total biomass when biomass is
 # available. If biomass is not available or not suitable, abundance-based
 # proportions should be used instead.
 # ----------------------------------------------------------------------- 
 
 DataOnlyWithHab_4$WeightTS[is.na(DataOnlyWithHab_4$WeightTS)] <- 0
 DataOnlyWithHab_4$Total_biomass <- as.numeric(as.character(DataOnlyWithHab_4$Total_biomass))
 
 DataOnlyWithHab_4$Total_biomass_2 <- ave(
   DataOnlyWithHab_4$Biomass,
   DataOnlyWithHab_4$station,
   FUN = sum
 )
 DataOnlyWithHab_4$PerTS_ByWeight <- DataOnlyWithHab_4$WeightTS/DataOnlyWithHab_4$Total_biomass_2
 names(DataOnlyWithHab_4)
 
 
 
 DataSubset <- subset(DataOnlyWithHab_4, select=c("TrawlingEffort_Levels", "TrawlingEffort" , "MSFD_broad_Ch",
                                                  "WeightTS", "Num_TS", "PerTS_ByWeight","Total_biomass_2",
                                                  "station", "lon", "lat"))
 
 nrow(DataSubset)
 
 DataBYlance <- unique(DataSubset)
 nrow(DataBYlance)
 
 DataBYlance$TrawlingEffort_Levels <- ordered(DataBYlance$TrawlingEffort_Levels, levels=c("No Effort", "Low Effort", "Medium Effort", "High Effort", "Very High Effort"))
 levels(DataBYlance$TrawlingEffort_Levels) <- c("No pressure", "Low pressure", "Medium pressure", "High pressure", "Very high pressure")
 table(DataBYlance$TrawlingEffort_Levels)
 
 
 # -----------------------------------------------------------------------
 # # Merge data
 # -----------------------------------------------------------------------
 
  DataBYlance <- merge(DataBYlance, DivData, by="station")
 
 # -----------------------------------------------------------------------
 # Check if the models are significant
 # -----------------------------------------------------------------------

 # Fit the GAM only within the common pressure domain (SAR <= 12)
 DataBYlance_gam <- DataBYlance[DataBYlance$TrawlingEffort <= 12, ]
 
 Mymodel_UBS <- gam(
   PerTS_ByWeight ~ s(TrawlingEffort, k = 3),
   data = DataBYlance_gam,
   family = binomial,
   method = "REML"
 )
 
 plot(Mymodel_UBS, shade = TRUE, seWithMean = TRUE)
 summary(Mymodel_UBS)
 
 # Extract p-value of the smooth term
 gam_summary <- summary(Mymodel_UBS)
 gam_pvalue <- gam_summary$s.table[1, "p-value"]
 
 # Create binary flag: 1 = significant, 0 = not significant
 gam_significant <- ifelse(!is.na(gam_pvalue) && gam_pvalue < 0.05, 1, 0)
 
 # Add the flag to the full exported dataset
 DataBYlance$gam_significant <- gam_significant
 
 
 output_path <- file.path(
   output_folder,
   paste0("Data_Ready_", msfd_bht, ".csv")
 )
 
 write.csv(DataBYlance, output_path, row.names = FALSE)
 
 # -----------------------------------------------------------------------
 #Plots
 # -----------------------------------------------------------------------
 
 # Build file path for the plot
 plot_output_path <- file.path(
   output_folder,
   paste0("plot_msfd_", msfd_bht, ".png")
 )
 
 # Open PNG device
 png(filename = plot_output_path, width = 2000, height = 1200, res = 150)
 
 # Set layout
 par(mfrow = c(2, 3))
 
 # Generate plots
 plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$PerTS_ByWeight, 
      main = "Proportion of sentinel species")
 plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$WeightTS, 
      main = "Sentinel species biomass (g)")
 plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Total_biomass_2, 
      ylim = c(0, 500000), main = "Total biomass (g)")
 plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Div, main = "Shannon index")
 plot(DataBYlance$TrawlingEffort_Levels, DataBYlance$Rich, main = "Richness")
 
 # Close device to save the plot
 dev.off()
 
 library(ggplot2)
 

 #Other
 
 msfd_box<-ggplot(DataBYlance) +
   aes(x = TrawlingEffort_Levels, y = PerTS_ByWeight) +
   geom_boxplot(fill = "#3FBCC8",alpha=0.2,notch=F, outlier.colour="red", outlier.fill="red", outlier.size=3) +
   labs(
     x = "Trawling effort",
     y = "Sentinel Species Proportion",
     title = "UBS ",
     subtitle = "SSpressure <=0.1"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 20L,
                               face = "bold",
                               hjust = 0.5),
     plot.subtitle = element_text(size = 17L,
                                  hjust = 0.5),
     axis.title.y = element_text(size = 14L),
     axis.title.x = element_text(size = 14L)
   ) + theme(axis.line = element_line(linetype = "solid"),
             axis.ticks = element_line(colour = "black"),
             panel.grid.major = element_line(colour = "gray86"))
 
 fig_path <- file.path(
   output_folder,
   paste0("Box_plot_propor_SSP_", msfd_bht, ".pdf")
 ) 
 
 plot(msfd_box)
 dev.off()
 
 #________________________________________________________________________________________________
 
 # Repeat the same workflow for each remaining habitat that:
 #   - has more than 20 hauls in total, and
 #   - has at least 4 hauls in the selected reference-pressure class
 #     after stepwise relaxation (SAR <= 0.10, then 0.33, then 0.65). 
#_______________________________________________________________________________________________
 

 
 
