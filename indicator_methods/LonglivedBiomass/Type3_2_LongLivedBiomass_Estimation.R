#---
#   title: "Loading and initial formatting of Type 3 data"
#   subtitle: "Indicator: long-lived biomass"
#   author: "Jochen Depestele"
#---

# SESSION SETUP ####
data_folder <- "./data"
if(all(dir.exists(paste0(data_folder,c("Type 1","Type2","Type3"))))){
  cat("\n=> Locate data folders (Type 1, Type2, Type3) \nfrom the data call in data_folder !\n ")}

library(readxl)
library(data.table)
library(worrms)
library(pbapply)
library(dplyr)
library(tools)
library(readr)

# Functions ----
logit = function(p){log(p/(1-p))}

# Estimate slope and intercept of cumulative binomial distribution functions of 
# the relative biomass distribution across longevity classes
f_coeflmod <- function(L1, L1_3, L3_10, L10){ 
  L_vct=c(L1, L1_3, L3_10, L10)
  dat <- data.frame(Cumb=cumsum(L_vct[c(1:3)])/sum(L_vct),
                    ll=c(log(1),log(3),log(10)))
  suppressWarnings(mod1 <- glm(Cumb~ll, family = binomial, data = dat))
  lintercept = as.numeric(coef(mod1)["(Intercept)"])
  lslope = as.numeric(coef(mod1)["ll"])
  return(c(lintercept=lintercept,
           lslope=lslope))
}

# # Predict the longevity for a relative cumulative biomass probability
f_predlong <- function(lintercept,lslope,prob=0.5){
  as.numeric(exp(logit(prob)-lintercept/lslope)) # prob=0.5 results in medlong
}

# LOAD DATA ----
B_dt <- readRDS("./indicator_methods/LonglivedBiomass/Type3_biomass_long_dt.RDS")
B_dt[, biomass_g_per_sqm := as.numeric(ifelse(biomass_g_per_sqm=="NA",NA_character_,biomass_g_per_sqm))]

B_dt[, L_info_avail := ifelse((!is.na(L1) & !is.na(L3) & !is.na(L10) & !is.na(L10plus)),
                              "yes","no")]
B_dt[,
     .(sum(as.numeric(biomass_g_per_sqm))),
     by=.(.id, L_info_avail)]
B_dt <- B_dt[!is.na(biomass_g_per_sqm)]
Linfo <- B_dt[, .(biom_sum = sum(biomass_g_per_sqm)), by = .(.id, L_info_avail)]
Linfo[, percentage := round(biom_sum / sum(biom_sum) * 100, 2),by=.(.id)]
Linfo <- dcast(Linfo, .id~L_info_avail, value.var="percentage")
# Linfo
# # Key: <.id>
# #   .id    no   yes
# # <char> <num> <num>
# #   1:                BoBIC_CGFS  7.28 92.72
# # 2: BoBIC_GulfofCadizhabitats 26.18 73.82
# # 3:    BoBIC_IberianChabitats 71.02 28.98
# # 4:                  CS_EVHOE  5.08 94.92
# # 5:              CS_NS_IBTSFR 12.64 87.36
# # 6:                 FR_ORHAGO 15.93 84.07
# # 7:             NS_BEhabitats  1.56 98.44
# # 8:             NS_DKhabitats  4.22 95.78
# # 9:             NS_NLhabitats  3.57 96.43
# # 10:            WMS_EShabitats 11.27 88.73
# # 11:              WMS_FRMEDITS  6.92 93.08
# # 12:        WMS_IEOESPhabitats 19.59 80.41
# # 13:               WMS_NOURMED 18.69 81.31
table(ifelse(Linfo$yes>90, "90%_traits", "few_traits")) # 6 out of 7 datasets have longevity traits for 90 % of the biomass
Linfo[yes>90,]$.id

nrow(B_dt[.id %in% Linfo[yes>90,]$.id & L_info_avail=="yes"])/nrow(B_dt) # only 51% of the records is kept, 49% disregarded!!!
sum(B_dt[.id %in% Linfo[yes>90,]$.id & L_info_avail=="yes"]$biomass_g_per_sqm) / sum(B_dt$biomass_g_per_sqm) # 96% of the biomass is retained; 4% is disregarded

B_dt <- B_dt[.id %in% Linfo[yes>90,]$.id & L_info_avail=="yes"]

Lclasses <- c("L1", "L3", "L10", "L10plus")
# For each species: estimate its biomass per longevity class
B_dt[, 
     (Lclasses) := lapply(.SD, function(x) x * as.numeric(biomass_g_per_sqm)), 
     .SDcols = Lclasses]
# Summarize over species to estimate station-specific biomass-at-longevity-class
B_dt <- B_dt[,
             lapply(.SD, sum, na.rm=T),
             by=.(.id, station, year, month),
             .SDcols = Lclasses] # exclude aphiaid to summarize over all species
# selcols <- c(".id","station","year")
# unique(B_dt[,..selcols])
# nrow(B_dt[rowSums(B_dt[,..Lclasses])==0]) # 2 records
# The biomass in longevity classes cannot be zero
B_dt <- B_dt[rowSums(B_dt[,..Lclasses])!=0] 
if(all(rowSums(B_dt[,..Lclasses])==0)){
  "Check that the total biomass in any sample is not zero"}
unique(B_dt$month)
B_dt[, month:=NULL]

## Add station info ----
Stn_dt <- readRDS(file="./indicator_methods/LonglivedBiomass/Type3_stations_dt.RDS")
Stn_dt[, .N, by = .(station, year, .id)][N > 1]
Stn_dt[station==115 & year==2011 & .id=="NS_BEhabitats"]
selcols <- c(".id","station","year","habitat_type","latitude","longitude","month","gear_cat",
             "pressure_type","pressure_value","pressure_value2","pressure_value3")
Stn_dt <- Stn_dt[,..selcols]
Stn_dt[.id=="BoBIC_CGFS", gear_cat:="trawl"]
dim(Stn_dt); dim(unique(Stn_dt))
Stn_dt <- unique(Stn_dt)
Stn_dt[, .N, by = .(station, year, .id)][N > 1]
Stn_dt[station==92200017 & year==2015 & .id=="NS_DKhabitats"]

selcols <- c(".id","station","year","habitat_type","gear_cat","pressure_type") # ,"month" 
Stn_dt[, .N, by = selcols][N > 1]
names(Stn_dt)[!names(Stn_dt) %in% selcols]

numcols <- names(Stn_dt)[!names(Stn_dt) %in% selcols] 
Stn_dt[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
# Stn_dt[, sapply(.SD, class), .SDcols = numcols]
Stn_dt <- Stn_dt[,
                 lapply(.SD, mean, na.rm=T), # NOTE THAT LAT, LON, MONTHS and PRESSURE VALUES HAVE BEEN AVERAGED !!!
                 by=selcols,
                 .SDcols=numcols]
Stn_dt[, .N, by = selcols][N > 1]

selcols <- intersect(names(B_dt), names(Stn_dt))
selcols
# ".id"     "station" "year"
setkeyv(Stn_dt, selcols)
setkeyv(B_dt, selcols)

all_dt <- Stn_dt[B_dt, on = selcols, nomatch = 0]
dim(B_dt); dim(Stn_dt); dim(all_dt)

table(all_dt$pressure_type, all_dt$month)
plot(all_dt$longitude, all_dt$latitude)
table(all_dt$.id)


all_dt[,c("lintercept", "lslope") := 
       transpose(.(do.call(f_coeflmod, setNames(.SD, names(Lclasses))))),
     by=.(.id, station, year, gear_cat, latitude, longitude),
     .SDcols = Lclasses]

all_dt[, medlong := f_predlong(lintercept, lslope)]
all_dt[, Q90long := f_predlong(lintercept, lslope,prob=0.9)] # Longevity of the top 10% biomass

# library(ggplot2)
# library(colorspace)
# ggplot(all_dt, 
#        aes(x=longitude, y=latitude, col=log(Q90long))) +
#   geom_point() +
#   scale_color_continuous_sequential(palette="Heat")

saveRDS(all_dt, "./indicator_methods/LonglivedBiomass/Type3_biomass_long_meta_dt.RDS")

