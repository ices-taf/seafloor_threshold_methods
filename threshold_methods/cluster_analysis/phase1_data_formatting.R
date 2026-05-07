library(tidyverse)
library(data.table)
library(readxl)
library(sf)
library(terra)

rm(list=ls()) #clean R environment 
options(scipen = 999) #adopt scientific notation
set.seed(666)

#Read all the excel files
dataset.list<-list()
dataset.names<-list.files("./WKBENTH4/data/Type3",pattern=".xlsx",full.names = T)

#Get Tab2 information
for (i in 1:length(dataset.names)) {
  dataset.list[[i]]<-read_excel(dataset.names[i], sheet = 2) 
}

names(dataset.list)<-sub("\\.xlsx$", "", sub("^.*Type3/", "", dataset.names))


#Formatting BoBIC_CGFS

dataset.list$BoBIC_CGFS<-dataset.list$BoBIC_CGFS[,c("station","year","longitude","latitude","month","depth","gear","replicate","habitat_type",
                                                    "pressure_type2","pressure_value2","total_biomass","total_abundance","richness",
                                                    "Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]

colnames(dataset.list$BoBIC_CGFS)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type",
                                     "pressure_type","pressure_value","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")

dataset.list$BoBIC_CGFS$sample_ID<-paste(dataset.list$BoBIC_CGFS$station,dataset.list$BoBIC_CGFS$year,dataset.list$BoBIC_CGFS$month,dataset.list$BoBIC_CGFS$replicates,sep="_")
dataset.list$BoBIC_CGFS$dataset<-rep("BoBIC_CGFS",nrow(dataset.list$BoBIC_CGFS))

#Formatting BoBIC_GulfofCadizhabitats
dataset.list$BoBIC_GulfofCadizhabitats<-dataset.list$BoBIC_GulfofCadizhabitats[,c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type",
                                                    "pressure_type","pressure_value","total_biomass","total_abundance","richness",
                                                    "Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$BoBIC_GulfofCadizhabitats)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type",
                                     "pressure_type","pressure_value","total_biomass","total_abundance","richness",
                                     "relM_biomass","relM_abundance","SoS")

dataset.list$BoBIC_GulfofCadizhabitats$sample_ID<-paste(dataset.list$BoBIC_GulfofCadizhabitats$station,dataset.list$BoBIC_GulfofCadizhabitats$year,dataset.list$BoBIC_GulfofCadizhabitats$month,dataset.list$BoBIC_GulfofCadizhabitats$replicates,sep="_")
dataset.list$BoBIC_GulfofCadizhabitats$dataset<-rep("BoBIC_GulfofCadizhabitats",nrow(dataset.list$BoBIC_GulfofCadizhabitats))

#Formatting BoBIC_IberianChabitats
dataset.list$BoBIC_IberianChabitats<-dataset.list$BoBIC_IberianChabitats[,c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type",
                                                                   "pressure_type","pressure_value","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$BoBIC_IberianChabitats)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type",
                                                    "pressure_type","pressure_value","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")
dataset.list$BoBIC_IberianChabitats$sample_ID<-paste(dataset.list$BoBIC_IberianChabitats$station,dataset.list$BoBIC_IberianChabitats$year,dataset.list$BoBIC_IberianChabitats$month,dataset.list$BoBIC_IberianChabitats$replicates,sep="_")
dataset.list$BoBIC_IberianChabitats$dataset<-rep("BoBIC_IberianChabitats",nrow(dataset.list$BoBIC_IberianChabitats))

#Formatting BS_FI_habitatcoverage
dataset.list$BS_FI_habitatcoverage<-dataset.list$BS_FI_habitatcoverage[,c("Station","Date","Gear","replicate","latitude ETRS-TM35FIN","longitude ETRS-TM35FIN","Pressure_type","Phosphorus")]
colnames(dataset.list$BS_FI_habitatcoverage)<-c("station","date","gear","replicates","latitude","longitude","pressure_type","pressure_value")

# parse to POSIXct
dataset.list$BS_FI_habitatcoverage$date <- as.POSIXct(dataset.list$BS_FI_habitatcoverage$date,
                 format = "%d.%m.%Y %H.%M.%S",
                 tz = "UTC")

# add columns
dataset.list$BS_FI_habitatcoverage$year  <- as.integer(format(dataset.list$BS_FI_habitatcoverage$date, "%Y"))
dataset.list$BS_FI_habitatcoverage$month <- as.integer(format(dataset.list$BS_FI_habitatcoverage$date, "%m"))

BS_FI_habitatcoverage.df <- dataset.list$BS_FI_habitatcoverage

#convert comma-decimal strings to numeric
BS_FI_habitatcoverage.df$northing <- as.numeric(sub(",", ".", BS_FI_habitatcoverage.df$latitude,  fixed = TRUE))   # Y
BS_FI_habitatcoverage.df$easting  <- as.numeric(sub(",", ".", BS_FI_habitatcoverage.df$longitude, fixed = TRUE))   # X

#make points in ETRS-TM35FIN (EPSG:3067) and transform to WGS84 (EPSG:4326)
pts_3067 <- st_as_sf(BS_FI_habitatcoverage.df, coords = c("easting", "northing"), crs = 3067, remove = FALSE)
pts_4326 <- st_transform(pts_3067, 4326)

#extract lon/lat
xy <- st_coordinates(pts_4326)
BS_FI_habitatcoverage.df$lon_wgs84 <- xy[, "X"]
BS_FI_habitatcoverage.df$lat_wgs84 <- xy[, "Y"]

#replace columns
dataset.list$BS_FI_habitatcoverage$latitude<-BS_FI_habitatcoverage.df$lat_wgs84
dataset.list$BS_FI_habitatcoverage$longitude<-BS_FI_habitatcoverage.df$lon_wgs84 
dataset.list$BS_FI_habitatcoverage <- dataset.list$BS_FI_habitatcoverage %>% select(-date)

dataset.list$BS_FI_habitatcoverage$sample_ID<-paste(dataset.list$BS_FI_habitatcoverage$station,dataset.list$BS_FI_habitatcoverage$year,dataset.list$BS_FI_habitatcoverage$month,dataset.list$BS_FI_habitatcoverage$replicates,sep="_")
dataset.list$BS_FI_habitatcoverage$dataset<-rep("BS_FI_habitatcoverage",nrow(dataset.list$BS_FI_habitatcoverage))


#Formatting CS_EVHOE
dataset.list$CS_EVHOE<-dataset.list$CS_EVHOE[,c("station","year","longitude","latitude","month","depth","gear","replicate","habitat_type","pressure_type2",
                                                "pressure_value2","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$CS_EVHOE)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type","pressure_type",
                                                          "pressure_value","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")
dataset.list$CS_EVHOE$sample_ID<-paste(dataset.list$CS_EVHOE$station,dataset.list$CS_EVHOE$year,dataset.list$CS_EVHOE$month,dataset.list$CS_EVHOE$replicates,sep="_")
dataset.list$CS_EVHOE$dataset<-rep("CS_EVHOE",nrow(dataset.list$CS_EVHOE))

#Formatting CS_NS_IBTSFR
dataset.list$CS_NS_IBTSFR<-dataset.list$CS_NS_IBTSFR[,c("station","year","longitude","latitude","month","depth","gear","replicate","habitat_type","pressure_type2",
                                                "pressure_value2","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$CS_NS_IBTSFR)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type","pressure_type",
                                   "pressure_value","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")
dataset.list$CS_NS_IBTSFR$sample_ID<-paste(dataset.list$CS_NS_IBTSFR$station,dataset.list$CS_NS_IBTSFR$year,dataset.list$CS_NS_IBTSFR$month,dataset.list$CS_NS_IBTSFR$replicates,sep="_")
dataset.list$CS_NS_IBTSFR$dataset<-rep("CS_NS_IBTSFR",nrow(dataset.list$CS_NS_IBTSFR))


#Formatting CS_NS_UKhabitats
dataset.list$CS_NS_UKhabitats<-dataset.list$CS_NS_UKhabitats[,c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value","habitat_type",
                                                                "total_abundance","richness","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$CS_NS_UKhabitats)<-c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value","habitat_type",
                                           "total_abundance","richness","relM_abundance","SoS")

dataset.list$CS_NS_UKhabitats$sample_ID<-paste(dataset.list$CS_NS_UKhabitats$station,dataset.list$CS_NS_UKhabitats$year,dataset.list$CS_NS_UKhabitats$month,dataset.list$CS_NS_UKhabitats$replicates,sep="_")
dataset.list$CS_NS_UKhabitats$dataset<-rep("CS_NS_UKhabitats",nrow(dataset.list$CS_NS_UKhabitats))

#Formatting FR_ORHAGO
dataset.list$FR_ORHAGO<-dataset.list$FR_ORHAGO[,c("station","year","longitude","latitude","month","depth","gear","replicate","habitat_type","pressure_type2","pressure_value2",
                                                  "total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$FR_ORHAGO)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type","pressure_type","pressure_value",
                                    "total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")

dataset.list$FR_ORHAGO$sample_ID<-paste(dataset.list$FR_ORHAGO$station,dataset.list$FR_ORHAGO$year,dataset.list$FR_ORHAGO$month,dataset.list$FR_ORHAGO$replicates,sep="_")
dataset.list$FR_ORHAGO$dataset<-rep("FR_ORHAGO",nrow(dataset.list$FR_ORHAGO))

#Formatting NS_BEhabitats
dataset.list$NS_BEhabitats<-dataset.list$NS_BEhabitats[,c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value",
                                                          "habitat_type","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$NS_BEhabitats)<-c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value",
                                        "habitat_type","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")

dataset.list$NS_BEhabitats$sample_ID<-paste(dataset.list$NS_BEhabitats$station,dataset.list$NS_BEhabitats$year,dataset.list$NS_BEhabitats$month,dataset.list$NS_BEhabitats$replicates,sep="_")
dataset.list$NS_BEhabitats$dataset<-rep("NS_BEhabitats",nrow(dataset.list$NS_BEhabitats))

#Formatting NS_DKhabitats
dataset.list$NS_DKhabitats<-dataset.list$NS_DKhabitats[,c("sample_ID","station","year","longitude","latitude","month","depth","gear","replicate","pressure_type","pressure_value",
                                                          "habitat_type","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)")]
colnames(dataset.list$NS_DKhabitats)<-c("sample_ID","station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value",
                                        "habitat_type","total_biomass","total_abundance","richness","relM_biomass","relM_abundance")
dataset.list$NS_DKhabitats$dataset<-rep("NS_DKhabitats",nrow(dataset.list$NS_DKhabitats))

#Formatting NS_NLhabitats
dataset.list$NS_NLhabitats<-dataset.list$NS_NLhabitats[,c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value_5y",
                                                          "habitat_type","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]

colnames(dataset.list$NS_NLhabitats)<-c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value",
                                        "habitat_type","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")
dataset.list$NS_NLhabitats$sample_ID<-paste(dataset.list$NS_NLhabitats$station,dataset.list$NS_NLhabitats$year,dataset.list$NS_NLhabitats$month,dataset.list$NS_NLhabitats$replicates,sep="_")
dataset.list$NS_NLhabitats$dataset<-rep("NS_NLhabitats",nrow(dataset.list$NS_NLhabitats))

#Formatting WMS_APPEALMED
dataset.list$WMS_APPEALMED<-dataset.list$WMS_APPEALMED[,c("station","year","longitude","latitude","month","depth","gear","replicates","Pressure_type","SAR5","habitat_type","total_abundance","richness","Rel Margalef div (dens)")]
colnames(dataset.list$WMS_APPEALMED)<-c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value","habitat_type","total_abundance","richness","relM_abundance")
dataset.list$WMS_APPEALMED$sample_ID<-paste(dataset.list$WMS_APPEALMED$station,dataset.list$WMS_APPEALMED$year,dataset.list$WMS_APPEALMED$month,dataset.list$WMS_APPEALMED$replicates,sep="_")
dataset.list$WMS_APPEALMED$dataset<-rep("WMS_APPEALMED",nrow(dataset.list$WMS_APPEALMED))


#Formatting WMS_EShabitats
dataset.list$WMS_EShabitats<-dataset.list$WMS_EShabitats[,c("station","year","longitude","latitude","depth","month","gear","replicates","pressure_type","pressure_value",
                                                            "habitat_type","total_biomass.x","richness","Rel Margalef div (biom)","SoS_2026")]
colnames(dataset.list$WMS_EShabitats)<-c("station","year","longitude","latitude","depth","month","gear","replicates","pressure_type","pressure_value",
                                        "habitat_type","total_biomass","richness","relM_biomass","SoS")
dataset.list$WMS_EShabitats$sample_ID<-paste(dataset.list$WMS_EShabitats$station,dataset.list$WMS_EShabitats$year,dataset.list$WMS_EShabitats$month,dataset.list$WMS_EShabitats$replicates,sep="_")
dataset.list$WMS_EShabitats$dataset<-rep("WMS_EShabitats",nrow(dataset.list$WMS_EShabitats))

#Formatting WMS_FRMEDITS
dataset.list$WMS_FRMEDITS<-dataset.list$WMS_FRMEDITS[,c("station","year","longitude","latitude","month","gear","habitat_type","pressure_type","SAR5",
                                                        "total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$WMS_FRMEDITS)<-c("station","year","longitude","latitude","month","gear","habitat_type","pressure_type","pressure_value",
                                       "total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")

dataset.list$WMS_FRMEDITS$replicates<-rep("1",nrow(dataset.list$WMS_FRMEDITS))
dataset.list$WMS_FRMEDITS$sample_ID<-paste(dataset.list$WMS_FRMEDITS$station,dataset.list$WMS_FRMEDITS$year,dataset.list$WMS_FRMEDITS$month,dataset.list$WMS_FRMEDITS$replicates,sep="_")
dataset.list$WMS_FRMEDITS$dataset<-rep("WMS_FRMEDITS",nrow(dataset.list$WMS_FRMEDITS))

#Formatting WMS_IEOESPhabitats
dataset.list$WMS_IEOESPhabitats<-dataset.list$WMS_IEOESPhabitats[,c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value",
                                                                    "habitat_type","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]

colnames(dataset.list$WMS_IEOESPhabitats)<-c("station","year","longitude","latitude","month","depth","gear","replicates","pressure_type","pressure_value",
                                             "habitat_type","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")
dataset.list$WMS_IEOESPhabitats$sample_ID<-paste(dataset.list$WMS_IEOESPhabitats$station,dataset.list$WMS_IEOESPhabitats$year,dataset.list$WMS_IEOESPhabitats$month,dataset.list$WMS_IEOESPhabitats$replicates,sep="_")
dataset.list$WMS_IEOESPhabitats$dataset<-rep("WMS_IEOESPhabitats",nrow(dataset.list$WMS_IEOESPhabitats))

#Formatting WMS_ISCMS_IRBIMCNR
dataset.list$WMS_ISCMS_IRBIMCNR<-dataset.list$WMS_ISCMS_IRBIMCNR[,c("station","year","latitude_shooting","longitude_shooting","latitude_hauling","longitude_hauling","month","depth","gear","pressure_type","pressure_value_3yravg",
                                                                    "habitat_type","total_biomass","total_abundance","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$WMS_ISCMS_IRBIMCNR)<-c("station","year","latitude_shooting","longitude_shooting","latitude_hauling","longitude_hauling","month","depth","gear","pressure_type","pressure_value",
                                             "habitat_type","total_biomass","total_abundance","richness","relM_biomass","relM_abundance","SoS")

dataset.list$WMS_ISCMS_IRBIMCNR$latitude<-(dataset.list$WMS_ISCMS_IRBIMCNR$latitude_shooting+dataset.list$WMS_ISCMS_IRBIMCNR$latitude_hauling)/2
dataset.list$WMS_ISCMS_IRBIMCNR$longitude<-(dataset.list$WMS_ISCMS_IRBIMCNR$longitude_shooting+dataset.list$WMS_ISCMS_IRBIMCNR$longitude_hauling)/2
dataset.list$WMS_ISCMS_IRBIMCNR <- dataset.list$WMS_ISCMS_IRBIMCNR %>% select(-c(latitude_shooting,longitude_shooting,longitude_hauling,latitude_hauling))

dataset.list$WMS_ISCMS_IRBIMCNR$replicates<-rep("1",nrow(dataset.list$WMS_ISCMS_IRBIMCNR))
dataset.list$WMS_ISCMS_IRBIMCNR$sample_ID<-paste(dataset.list$WMS_ISCMS_IRBIMCNR$station,dataset.list$WMS_ISCMS_IRBIMCNR$year,dataset.list$WMS_ISCMS_IRBIMCNR$month,dataset.list$WMS_ISCMS_IRBIMCNR$replicates,sep="_")
dataset.list$WMS_ISCMS_IRBIMCNR$dataset<-rep("WMS_ISCMS_IRBIMCNR",nrow(dataset.list$WMS_ISCMS_IRBIMCNR))

#Formatting WMS_NOURMED
dataset.list$WMS_NOURMED<-dataset.list$WMS_NOURMED[,c("station","year","longitude","latitude","month","depth","gear","replicate","habitat_type", "SAR5","total_biomass","richness","Rel Margalef div (biom)","Rel Margalef div (dens)","SoS_2026")]
colnames(dataset.list$WMS_NOURMED)<-c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type", "pressure_value","total_biomass","richness","relM_biomass","relM_abundance","SoS")
dataset.list$WMS_NOURMED$sample_ID<-paste(dataset.list$WMS_NOURMED$station,dataset.list$WMS_NOURMED$year,dataset.list$WMS_NOURMED$month,dataset.list$WMS_NOURMED$replicates,sep="_")
dataset.list$WMS_NOURMED$dataset<-rep("WMS_NOURMED",nrow(dataset.list$WMS_NOURMED))
dataset.list$WMS_NOURMED$pressure_type<-rep("SAR_5yr_avg",nrow(dataset.list$WMS_NOURMED))

#Merge everything into a single dataframe
dataset.merged<-rbindlist(dataset.list,fill=T)
dataset.merged<-as.data.frame(dataset.merged)

#be sure that the right columns are numeric
cols_num <- c("longitude","latitude","depth","pressure_value","total_biomass","total_abundance","SoS")
cols_num <- intersect(cols_num, names(dataset.merged))

dataset.merged[cols_num] <- lapply(dataset.merged[cols_num], function(x) {
  as.numeric(gsub(",", ".", x))  
})

cols_int <- c("year","month","richness")
cols_int <- intersect(cols_int, names(dataset.merged))

dataset.merged[cols_int] <- lapply(dataset.merged[cols_int], function(x) {
  as.integer(gsub(",", ".", x))
})


#check depth and transfrom all the negative values in positive
dataset.merged$depth<-ifelse(dataset.merged$depth<0,dataset.merged$depth*-1,dataset.merged$depth)

#check gear
table(dataset.merged$gear)
dataset.merged$gear[dataset.merged$gear%in%c("Day grab","Day grab (DA)")]<-"day_grab"
dataset.merged$gear[dataset.merged$gear%in%c("beamtrawl","BeamTrawl")]<-"beam_trawl"
dataset.merged$gear[dataset.merged$gear%in%c("Box-corer")]<-"box_corer"
dataset.merged$gear[dataset.merged$gear%in%c("Hamon grab (HM)")]<-"hamon_grab"
dataset.merged$gear[dataset.merged$gear%in%c("Haps")]<-"haps_corer"
dataset.merged$gear[dataset.merged$gear%in%c("OTB","otter_trawl","ottertrawl","ottertrawl_GOV")]<-"otter_trawl"
dataset.merged$gear[dataset.merged$gear%in%c("Van Veen grab")]<-"vanveen_grab"
dataset.merged$gear[dataset.merged$gear%in%c("Vegetation mapping")]<-"vegetation_mapping"
dataset.merged$gear[dataset.merged$gear%in%c("Video methods")]<-"video"
dataset.merged$gear[dataset.merged$gear%in%c("NA")]<-"bottom_trawl"
table(dataset.merged$gear)

#check habitat type
table(dataset.merged$habitat_type)
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral coarse sediment","Circalittoral coarse sediment (MC3)")]<-"Circalittoral coarse sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral mixed sediment","MC451")]<-"Circalittoral mixed sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral mud","Circalittoral mud (MC6)")]<-"Circalittoral mud"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral rock and biogenic reef")]<-"Circalittoral rock and biogenic reef"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("circalittoral sand","Circalittoral sand","Circalittoral sand (MC5)")]<-"Circalittoral sand"

dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Infralittoral coarse sediment","Infralittoral coarse sediment (MB3)")]<-"Infralittoral coarse sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Infralittoral mixed sediment")]<-"Infralittoral mixed sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Infralittoral mud")]<-"Infralittoral mud"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Infralittoral rock and biogenic reef")]<-"Infralittoral rock and biogenic reef"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Infralittoral sand","Infralittoral sand (MB5)")]<-"Infralittoral sand"

dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Lower bathyal rock and biogenic reef")]<-"Lower bathyal rock and biogenic reef"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Lower bathyal sediment")]<-"Lower bathyal sediment"

dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("MD451","Offshore circalittoral mixed sediment")]<-"Offshore circalittoral mixed sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Offshore circalittoral coarse sediment","Offshore circalittoral coarse sediment (MD3)")]<-"Offshore circalittoral coarse sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Offshore circalittoral mud","Offshore circalittoral mud (MD6)")]<-"Offshore circalittoral mud"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Offshore circalittoral rock and biogenic reef")]<-"Offshore circalittoral rock and biogenic reef"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("offshore circalittoral sand","Offshore circalittoral sand","Offshore circalittoral sand (MD5)")]<-"Offshore circalittoral sand"

dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Upper bathyal rock and biogenic reef")]<-"Upper bathyal rock and biogenic reef"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Upper bathyal sediment")]<-"Upper bathyal sediment"




dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral coarse sediment/Circalittoral sand","Circalittoral sand/Circalittoral coarse sediment")]<-"Circalittoral sand/Circalittoral coarse sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral mud/Circalittoral mixed sediment")]<-"Circalittoral mud/Circalittoral mixed sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral mud/Circalittoral sand","Circalittoral sand/Circalittoral mud")]<-"Circalittoral mud/Circalittoral sand"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral sand/Circalittoral mixed sediment")]<-"Circalittoral sand/Circalittoral mixed sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Circalittoral sand/Offshore circalittoral sand","Offshore circalittoral sand/Circalittoral sand")]<-"Circalittoral sand/Offshore circalittoral sand"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Lower bathyal sediment or Lower bathyal rock and biogenic reef")]<-"Lower bathyal sediment/Lower bathyal rock and biogenic reef"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Lower bathyal sediment/Upper bathyal sediment","Upper bathyal sediment or Lower bathyal sediment","Upper bathyal sediment/Lower bathyal sediment")]<-"Lower bathyal sediment/Upper bathyal sediment"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Offshore circalittoral mud or Offshore circalittoral sand")]<-"Offshore circalittoral mud/Offshore circalittoral sand"
dataset.merged$habitat_type[dataset.merged$habitat_type%in%c("Upper bathyal sediment or Upper bathyal rock and biogenic reef")]<-"Upper bathyal sediment/Upper bathyal rock and biogenic reef"
table(dataset.merged$habitat_type)

table(dataset.merged$pressure_type)
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("SAR_12_all")]<-"SAR_1yr_lag"
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("SAR")&dataset.merged$dataset%in%c("NS_BEhabitats")]<-"SAR_1yr_lag"
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("SAR5")]<-"SAR_5yr_avg"
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("SAR")&dataset.merged$dataset%in%c("BoBIC_GulfofCadizhabitats","BoBIC_IberianChabitats","NS_BEhabitats","WMS_APPEALMED","WMS_EShabitats","WMS_FRMEDITS","WMS_IEOESPhabitats")]<-"SAR_5yr_avg"
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("SAR")&dataset.merged$dataset%in%c("WMS_ISCMS_IRBIMCNR")]<-"SAR_3yr_avg"
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("subSAR")]<-"subSAR_total_avg"
dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("SA")]<-"SAR_5yr_avg"

dataset.merged$pressure_type[dataset.merged$pressure_type%in%c("Total phosphorus")]<-"total_phosphorus"
table(dataset.merged$pressure_type)



#Read ICES ecoregions
ICES.eco<-st_read("./WKBENTH4/data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp")
ICES.eco <- st_transform(ICES.eco, 4326)
ICES.eco <- st_make_valid(ICES.eco)

#Recreate sf points
dataset.pts <- st_as_sf(dataset.merged,
                coords = c("longitude", "latitude"),
                crs = 4326, remove = FALSE)

#find the ecoregion name field in the shapefile
names(ICES.eco)
eco_field <- "Ecoregion"

#spatial join
join.eco.sf <- st_join(dataset.pts, ICES.eco[, eco_field, drop = FALSE], join = st_intersects, left = TRUE)

# write back to the original df
dataset.merged$ICES_ecoregion <- NA_character_
dataset.merged$ICES_ecoregion <- join.eco.sf[[eco_field]]

#Add emodnet depth data
r_merged<-rast("./WKBENTH4/data/emodnet_dtm_bathymetry/emodnet_depth_merged.tif")

dataset.merged$emod_depth<-extract(r_merged, dataset.merged[,c("longitude","latitude")], method="near")[,2]
dataset.merged$emod_depth<-dataset.merged$emod_depth*-1


fwrite(dataset.merged,"./WKBENTH4/data/WKBENTH4_datacall_merged.csv")
