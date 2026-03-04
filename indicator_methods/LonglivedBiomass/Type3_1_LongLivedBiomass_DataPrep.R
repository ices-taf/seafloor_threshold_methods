#---
#   title: "Loading and initial formatting of Type 3 data"
#   subtitle: "Data preparation"
#   author: "Jochen Depestele"
#---

# SESSION SETUP ####
data_folder <- "./data"
if(all(dir.exists(paste0(data_folder,c("Type 1","Type2","Type3"))))){
  cat("\n=> Locate data folders (Type 1, Type2, Type3) \nfrom the data call in data_folder !\n ")}
traits_folder <- "C:/Users/jdepestele/OneDrive - ILVO/000_data/biota/traits_benth/"
if(!all(dir.exists(c(traits_folder)))){
  cat("\n=> Locate all folders with traits information !\n ")}

library(readxl)
library(data.table)
library(worrms)
library(pbapply)
library(dplyr)
library(tools)
library(readr)

# LOAD DATA ----
d_files <- list.files(paste0(data_folder,"/Type3"),  pattern = "\\.xlsx$|\\.xls$", full.names = T)
d_sheets <- lapply(d_files, function(xfile) {
  sheets <- excel_sheets(xfile)
  data.frame(
    file = basename(xfile),
    has_station_info = "Station information" %in% sheets,
    has_biological_info = "Biological information" %in% sheets
  )
})
d_sheets <- do.call(rbind, d_sheets)
d_sheets[with(d_sheets, !has_station_info | !has_biological_info),] # Which files do NOT have all worksheets?

## Meta data ====
meta_lst <- lapply(d_files, function(file)read_excel(file, sheet=excel_sheets(file)[1]))
names(meta_lst) <- gsub("\\.xlsx","",basename(d_files))
meta_dt <- rbindlist(lapply(meta_lst, function(x)x[,c("cl_name","cl_description")]), idcol=T)

## Stations ====
Stn_lst <- lapply(d_files, function(file)read_excel(file, sheet=excel_sheets(file)[2]))
names(Stn_lst) <- gsub("\\.xlsx","",basename(d_files))

Stn_lst <- lapply(Stn_lst, function(df) {
  names(df) <- tolower(names(df)) # Lowercase column names
  df})
col_freq <- table(unlist(lapply(Stn_lst, names)))
sort(col_freq)
basecols <- names(col_freq[col_freq >= length(Stn_lst)]) # These columns occur in every excel file
# basecols
basecols <- c(basecols, c("pressure_type","pressure_value",paste0("pressure_value",c("1","2","3"))), "month")

Stn_dt <- rbindlist(
  lapply(Stn_lst, function(x) x[, intersect(basecols, names(x)), drop = FALSE]), # select basecols when existing
  idcol = T,
  fill = TRUE) # pressure_value1 and pressure_value2 do not exist in most datafiles
Stn_dt[,gear:=tolower(gear)]

unique(Stn_dt[month=="Annual"]$.id)
Stn_dt[month=="Annual", month := NA]
Stn_dt[, month:=as.numeric(ifelse(month=="NA",NA_character_,month))]

to_table1 <- table(Stn_dt$.id, Stn_dt$pressure_type)
to_table2 <- table(Stn_dt$.id, Stn_dt$gear)

# sort(unique(Stn_dt$gear))
unique(Stn_dt$gear)[grepl("trawl|otb",tolower(unique(Stn_dt$gear)))]
# "otter_trawl"    "ottertrawl_gov" "beamtrawl"      "ottertrawl"     "otb"
unique(Stn_dt$gear)[grepl("veen|hamon|day|haps|core",tolower(unique(Stn_dt$gear)))]
# "hamon grab (hm)" "day grab (da)"   "van veen grab"   "haps"            "box-corer"       "day grab"
Stn_dt[,gear_cat := ifelse(gear %in% unique(Stn_dt$gear)[grepl("trawl|otb",tolower(unique(Stn_dt$gear)))],
                           "trawl",
                           ifelse(gear %in% unique(Stn_dt$gear)[grepl("veen|hamon|day|haps|core",tolower(unique(Stn_dt$gear)))],
                                  "grab",NA))]
c(table(Stn_dt$gear_cat), 
  setNames(sum(table(Stn_dt$gear_cat)), "total"),
  setNames(nrow(Stn_dt), "Nr records Stn_dt")) # All records were sampled with grabs or trawls
Stn_dt[!gear_cat %in% c("trawl","grab")]

## Remove duplicate records
# selcols <- c(".id","station","year", "gear","month")
# Stn_dt[,..selcols][duplicated(Stn_dt[,..selcols]) | duplicated(Stn_dt[,..selcols], fromLast=T)]
# selcols <- c(intersect(basecols, names(Stn_dt)), "gear_cat")
# Stn_dt[,..selcols][duplicated(Stn_dt[,..selcols]) | duplicated(Stn_dt[,..selcols], fromLast=T)]
# dim(Stn_dt[,..selcols]);dim(unique(Stn_dt[,..selcols]))
dim(Stn_dt);dim(unique(Stn_dt))
Stn_dt <- unique(Stn_dt)

saveRDS(Stn_dt, file="./indicator_methods/LonglivedBiomass/Type3_stations_dt.RDS")

to_table3 <- table(Stn_dt$.id, Stn_dt$gear_cat)

## Biological data ====

# Check if biomass data are existing
if(!file.exists("./indicator_methods/LonglivedBiomass/Type3_biomass_dt.RDS")){
  B_lst <- lapply(d_files, function(file)read_excel(file, sheet=excel_sheets(file)[3]))
  names(B_lst) <- gsub("\\.xlsx","",basename(d_files))
  B_lst <- lapply(B_lst, function(df) {
    names(df) <- tolower(names(df)) # Lowercase column names
    names(df) <- gsub("_name","", names(df))
    df
    })
  lapply(B_lst, names)
  
  col_freq <- table(unlist(lapply(B_lst, names)))
  sort(col_freq) 
  lapply(B_lst, function(x)"aphiaid" %in% names(x)) # 1 dataset does NOT have aphiaid => NS_BEhabitats
  
  # basecols <- names(col_freq[col_freq >= length(Stn_lst)]) # These columns occur in every excel file
  basecols <- c("station","latitude","longitude","year","month", 
                "aphiaid", "taxcode", "biomass")
  
  B_dt <- rbindlist(
    lapply(B_lst, function(x) x[, intersect(basecols, names(x)), drop = FALSE]), # select basecols when existing
    idcol = T,
    fill = TRUE) # pressure_value1 and pressure_value2 do not exist in most datafiles
  B_dt[,.id:=gsub("\\.xlsx","",.id)]
  # B_dt[.id=="NS_BEhabitats"]
  
  # PRE-PROCESSING: BIOMASS
  B_dt[,biomass := as.numeric(ifelse(biomass=="NA",NA_character_,biomass))]
  datasets_excluded <- unique(B_dt[is.na(biomass)]$.id) # THREE DATASETS HAVE NAs, and ARE FULLY OR PARTIALLY EXCLUDED BECAUSE THEY DO NOT HAVE BIOMASS INFO
  # "CS_NS_UKhabitats" "NS_NLhabitats"    "WMS_APPEALMED" 
  B_dt <- B_dt[!is.na(biomass)]
  
  # CONVERT BIOMASS VALUES TO THE SAME UNITS
  biom_unit <- meta_dt[cl_name=="biomass" & .id %in% unique(B_dt$.id)]
  biom_unit[, sampled_area_desc := stringr::str_split_i(cl_description, "per ", 2)]
  biom_unit[, sampled_area_desc := stringr::str_split_i(sampled_area_desc, "\r" , 1)]
  biom_unit[, sampled_area_unit := fifelse(grepl("\\bkm2\\b", sampled_area_desc, TRUE), "km2",
                                           fifelse(grepl("\\bha\\b", sampled_area_desc, TRUE), "ha",
                                                   fifelse(grepl("m²|\\bm2\\b|\\b1m2\\b", sampled_area_desc, TRUE), "m2",
                                                           NA_character_)))]
  biom_unit[, sampled_area := fifelse(grepl("^[0-9.]+", sampled_area_desc),
                                      as.numeric(sub("^([0-9.]+).*", "\\1", sampled_area_desc)), 1)]
  biom_unit[, sampled_area_sqm := ifelse(grepl("km2",sampled_area_unit),
                                         sampled_area*1e6,ifelse(grepl("ha",sampled_area_unit),
                                                                 sampled_area*10000,ifelse(grepl("m2",sampled_area_unit),
                                                                                           sampled_area,NA_integer_)
                                         ))]
  # The DK dataset has sampled area in the Station info
  unlist(lapply(Stn_lst, function(x)names(x)[grepl("area", names(x))]))
  biom_unit[.id==names(unlist(lapply(Stn_lst, function(x)names(x)[grepl("area", names(x))]))), 
            `:=` (sampled_area = unique(Stn_lst[[names(unlist(lapply(Stn_lst, function(x)names(x)[grepl("area", names(x))])))]]$sample_area_m2),
                  sampled_area_sqm = unique(Stn_lst[[names(unlist(lapply(Stn_lst, function(x)names(x)[grepl("area", names(x))])))]]$sample_area_m2)
            )]
  
  selcols <- c(".id","cl_description")
  to_table4 <- biom_unit[,..selcols]
  
  B_dt[, biomass_unit := biom_unit$cl_description[match(B_dt$.id, biom_unit$.id, nomatch=NA_integer_)]]
  B_dt[, sampled_area_sqm := biom_unit$sampled_area_sqm[match(B_dt$.id, biom_unit$.id, nomatch=NA_integer_)]]
  B_dt[,biomass_g_per_sqm := biomass / sampled_area_sqm]
  B_dt[,biomass_g_per_sqm := format(biomass_g_per_sqm, scientific = FALSE, trim = TRUE)]
  
  
  # ASSIGN APHIA-IDs TO ALL TAXA
  # table(!is.na(B_dt[is.na(aphiaid)]$taxcode)) # When aphiaid is lacking, there's a taxcode
  tax_without_id <- sort(unique(B_dt[!is.na(taxcode) & is.na(aphiaid)]$taxcode))
  # tax_aphia_id <- wm_name2id_(name = tax_without_id) # Warnings for some records
  rank_order <- c("Kingdom", "Phylum", "Class", "Order", "Infraorder", "Family", "Genus", "Species", "Subspecies", "Variety", "Form")
  get_valid_aphia <- function(name) {
    rec <- tryCatch(worrms::wm_records_name(name),
                    error = function(e) NULL,
                    warning = function(w) NULL)
    if (is.null(rec) || length(rec) == 0) return(NA_integer_)
    rec <- rec[rec$status == "accepted" & !is.null(rec$status == "accepted"),]
    if (length(rec) == 0) return(NA_integer_)
    ranks_in_order <- intersect(rank_order, rec$rank)
    if(length(ranks_in_order) == 0) return(rec) # fallback
    return(rec[rec$rank == ranks_in_order[1],])  # Return the AphiaID of the first record with the highest rank
  }
  tax_aphia_id_lst <- pblapply(tax_without_id, get_valid_aphia)
  tax_aphia_id_dt <- rbindlist(lapply(tax_aphia_id_lst, as.data.table), fill = TRUE)
  selcols <- c("AphiaID","scientificname","status","rank","valid_name")
  
  tax_without_id[!tax_without_id %in% tax_aphia_id_dt$valid_name] # Missing taxa
  wm_records_name("Asthenognathus atlanticus")
  # "Asthenognathus atlanticus" => Correct name: "Dudekemus atlanticus"
  B_dt[is.na(aphiaid) & taxcode=="Asthenognathus atlanticus"]$aphiaid <- wm_records_name(wm_records_name("Asthenognathus atlanticus")$valid_name) %>% filter(status=="accepted") %>% select(AphiaID)
  # tax_aphia_id_dt[valid_name %in% tax_without_id,..selcols]
  # tax_aphia_id_dt[!valid_name %in% tax_without_id,..selcols]
  
  B_dt[is.na(aphiaid)]$aphiaid <- tax_aphia_id_dt$AphiaID[match(B_dt[is.na(aphiaid)]$taxcode, tax_aphia_id_dt$valid_name, nomatch=NA_integer_)]
  
  selcols <- c(".id","station","year","month","aphiaid","biomass_g_per_sqm")
  dim(B_dt); dim(B_dt[aphiaid!="NA" & !is.na(aphiaid),..selcols])
  B_dt <- B_dt[aphiaid!="NA" & !is.na(aphiaid),..selcols]
  
  saveRDS(B_dt, file="./indicator_methods/LonglivedBiomass/Type3_biomass_dt.RDS")
  
}else{
  B_dt <- readRDS(file="./indicator_methods/LonglivedBiomass/Type3_biomass_dt.RDS")
} # end ifelse-conditions prep biomass data



# ADD LONGEVITY TRAIT ----
## Load traits ====
if(!file.exists("./indicator_methods/LonglivedBiomass/long_dt.RDS")){
  # COMPILE COMBINED biological trait database
  # Load biological trait databases
  trt_files <- list.files(traits_folder,  pattern = "\\.xlsx$|\\.xls$|\\.csv$", full.names = T, recursive = T)
  beauchard_legend <- read_excel(trt_files[6])
  trt_files <- trt_files[grepl("Foveau|Vaz|11877|Beauchard_SupplMat\\.xlsx$", trt_files)] # 4 trait databases
  
  read_csv_auto <- function(file) {read_delim(file, delim = NULL, show_col_types = FALSE)}
  read_any_file <- function(file) {
    ext <- tolower(file_ext(file))
    switch(ext,
           "csv"  = read_csv_auto(file),
           "xlsx" = read_excel(file),
           "xls"  = read_excel(file),
           stop("Unsupported file type: ", ext))}
  trt_sheets <- lapply(trt_files, read_any_file)
  names(trt_sheets) <- basename(trt_files)
  
  # Manually select all longevity traits => exclude Foveau
  lapply(trt_sheets, names)

  # Longevity traits when aphia-id is available (Vaz and Clare):
  long <- list()
  long[["vaz"]] <- trt_sheets[[2]][,c("aphiaID","meanL1", "meanL1.L3", "meanL3.L10", "meanL10","phylum","class","order","family","genus")]
  vaz_NAs <- long[["vaz"]][is.na(long[["vaz"]]$aphiaID),]
  vaz_NAs_aphiaids <- rbindlist((pblapply(c("Rissoidae","Hoplostethus","Terebellidae"), get_valid_aphia)))
  long[["vaz"]][is.na(long[["vaz"]]$aphiaID),]$aphiaID <- vaz_NAs_aphiaids[status=="accepted"]$AphiaID
  
  long[["clare"]] <- trt_sheets[[3]][,c("AphiaID","l_less_than_1","l1_to_3","l3_to_10","l_more_than_10","Phylum","Class","_Order","Family","Genus")]
  long[["clare"]][is.na(long[["clare"]]$AphiaID),]
  
  # Add aphia-id to beauchard's database:
  beauchard <- setDT(trt_sheets[[4]][,c("genus","species","LS")])
  stan_score <- beauchard_legend[beauchard_legend$trait=="LS",]$standardised_score
  beauchard[, longevity := stan_score[LS]]
  beauchard <- dcast(beauchard, genus+species~longevity, value.var = "longevity")
  beauchard[, sp := paste(genus, species,sep=" ")]
  beauchard[, sp := gsub(" sp.","",sp)]
  setnames(beauchard, c("0", "0.33", "0.67", "1"),
           c("L1","L3","L10","L10plus"))
  beauchard_aphia_id_lst <- pblapply(beauchard$sp, get_valid_aphia)
  beauchard_aphia_id_dt <- rbindlist(lapply(beauchard_aphia_id_lst, as.data.table), fill = TRUE)
  # names(beauchard_aphia_id_dt)
  selcols <- c("valid_name","valid_AphiaID","phylum","class","order","family","genus","scientificname")
  beauchard_aphia_id_dt[!is.na(valid_name),..selcols]
  missing_taxa <- beauchard$sp[!beauchard$sp %in% beauchard_aphia_id_dt$valid_name] # Missing taxa
  # lapply(missing_taxa[!missing_taxa %in% c("Ampeliscanipes","Aporrhais serresianus","Gastrosaccusnifer","Munidaciosa",
  #                                          "Pontophilusnosus","Porania pulvillus","Pseudocuma longicorne")], 
  #        function(x)wm_records_name(x))
  # beauchard$sp[beauchard$sp %in% beauchard_aphia_id_dt$scientificname]
  
  setkey(beauchard, genus, sp)
  setkey(beauchard_aphia_id_dt, genus, scientificname)
  beauchard <- beauchard_aphia_id_dt[!is.na(valid_name),..selcols][beauchard]
  nrow(beauchard[is.na(valid_AphiaID)]) # 26 missing taxa 
  beauchard <- beauchard[!is.na(valid_AphiaID)]
  selcols <- c("valid_AphiaID","L1","L3","L10","L10plus","phylum","class","order","family","genus","scientificname")
  long[["beauchard"]] <- beauchard[,..selcols]
  setnafill(long[["beauchard"]],
            type = "const", fill = 0,
            cols = c("L1", "L3", "L10", "L10plus"))
  
  long[["vaz"]]$scientificname <- NA
  long[["clare"]]$scientificname <- NA
  
  long <- lapply(long, function(x){
    names(x) <- c("aphiaid","L1","L3","L10","L10plus","phylum","class","order","family","genus","scientificname")
    as.data.table(x)})
  long_dt <- rbindlist(long, idcol=T)
  long_dt <- long_dt[!is.na(aphiaid)] # redundant
  
  unique_aphias_orig <- length(unique(long_dt$aphiaid))
  common_aphiaid <- long_dt[, 
                            uniqueN(.id), by = aphiaid]
  overlap_aphias_orig <- table(common_aphiaid$V1) #
  # to_plot <- long_dt[aphiaid %in% common_aphiaid[V1 %in% c(2,3), aphiaid]] # To enable comparison of the longevity classes (not done)
  
  round(table(duplicated(long_dt[,-1]))/nrow(long_dt),2)*100 # 10 %  duplicated records
  selcols <- c("aphiaid")
  round(table(duplicated(long_dt[,..selcols]))/nrow(long_dt[,..selcols]),2)*100 # 35 % duplicated taxa, 
  # implying that trait modalities differed between 65 % of overlapping taxa
  long_spp_dt <- long_dt[!is.na(scientificname),
                         lapply(.SD,function(x)mean(x,na.rm=T)), # Take the mean over duplicated taxa
                         by=.(aphiaid, phylum, class, order, family, genus, scientificname),
                         .SDcols=c("L1","L3","L10","L10plus")]
  long_genus_dt <- long_dt[,
                           lapply(.SD,function(x)mean(x,na.rm=T)), # Take the mean over duplicated taxa
                           by=.(aphiaid, phylum, class, order, family, genus),
                           .SDcols=c("L1","L3","L10","L10plus")]
  long_genus_dt[,scientificname := NA_character_]
  
  # Note that some taxonomic ranks differ even though the aphia-ids are the same ! => they have different longevities
  dbls <- long_genus_dt[duplicated(long_genus_dt$aphiaid) | duplicated(long_genus_dt$aphiaid,fromLast = T)]
  num_cols <- names(dbls)[sapply(dbls, is.numeric)]
  char_cols <- names(dbls)[sapply(dbls, is.character)]
  char_cols <- char_cols[!char_cols %in% "aphiaid"]
  char_agg_fun <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    x_unique <- unique(x)
    if (length(x_unique) == 1) return(x_unique)
    paste(x_unique, collapse = "%%")
  }
  dbls_toadd <- dbls[,
                     c(lapply(.SD[, num_cols, with = FALSE], function(x) mean(x, na.rm = TRUE)),
                       lapply(.SD[, char_cols, with = FALSE], char_agg_fun)), 
                     by = .(aphiaid),
                     .SDcols = c(num_cols, char_cols)]
  setcolorder(dbls_toadd, names(long_genus_dt))
  # Replace duplicates:
  long_genus_dt <- rbind(long_genus_dt[!(duplicated(long_genus_dt$aphiaid) | duplicated(long_genus_dt$aphiaid,fromLast = T))],
                         dbls_toadd)
  
  long_dt <- rbind(long_genus_dt, long_spp_dt)
  
  saveRDS(long_dt, "./indicator_methods/LonglivedBiomass/long_dt.RDS")
  
}else{
  long_dt <- readRDS("./indicator_methods/LonglivedBiomass/long_dt.RDS")
} # end ifelse-conditions longevity traits database

## Add traits to biomass data ----
selcols <- c(".id","station","year","month","aphiaid","biomass_g_per_sqm")
B_dt <- B_dt[aphiaid!="NA" & !is.na(aphiaid),..selcols]
if(any(duplicated(B_dt))){
  B_dt[duplicated(B_dt) | duplicated(B_dt, fromLast = TRUE)] # See the duplicates
  B_dt <- unique(B_dt) # remove duplicates
}
if(any(duplicated(long_dt))){
  long_dt[duplicated(long_dt) | duplicated(long_dt, fromLast = TRUE)]
  long_dt <- unique(long_dt) # remove duplicates
}

# Add traits using species' scientific names
setkey(B_dt, aphiaid)
setkey(long_dt, aphiaid)

temp_dt <- long_dt[!is.na(scientificname)][B_dt]
dim(B_dt)
dim(temp_dt)
B_spp_dt <- temp_dt[!is.na(scientificname)] # scientific names are given

# Add traits using genus names
B_gen_dt <- temp_dt[is.na(scientificname)]
selcols <- names(B_dt)
B_gen_dt <- B_gen_dt[,..selcols]

setkey(B_gen_dt, aphiaid)
setkey(long_dt, aphiaid)

B_gen_dt <- long_dt[is.na(scientificname)][B_gen_dt]

# Combine datasets with traits, given by spp or genus
B_dt <- rbind(B_spp_dt, B_gen_dt)
names(B_dt)
setcolorder(B_dt, c(".id","station","year","month","aphiaid","biomass_g_per_sqm","L1","L3","L10","L10plus","phylum","class","order","family","genus","scientificname"))

saveRDS(B_dt, "./indicator_methods/LonglivedBiomass/Type3_biomass_long_dt.RDS")


