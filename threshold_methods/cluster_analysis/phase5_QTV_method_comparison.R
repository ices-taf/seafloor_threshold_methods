#########################################################################################
#
#                              Phase 5: QTV method comparison
#                                   Type 1 vs Type 3 QTV 
#
##########################################################################################
#ICES WKBENTH4
#Script Author: Gabriele Di Bona
#February 2026


library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)
library(fs)
library(MASS)
library(emmeans)
library(glmmTMB)

rm(list = ls())
options(scipen = 999)
set.seed(666)


#Load type1 threshold values
type1_qtv<-readRDS("./WKBENTH4/data/all_type1_thresholds.RDS")
type1_qtv<-filter(type1_qtv,data_type=="whole_community")
type1_qtv$indicator<-ifelse(type1_qtv$indicator=="biomass","total_biomass",
                            ifelse(type1_qtv$indicator=="Rel Margalef div (dens)","relM_abundance",
                                   ifelse(type1_qtv$indicator=="abundance","total_abundance",
                                          ifelse(type1_qtv$indicator=="SoS_2026","SoS",
                                                 ifelse(type1_qtv$indicator=="Rel Margalef div (biom)","relM_biomass",type1_qtv$indicator)))))
#remove lf
type1_qtv<-filter(type1_qtv,indicator!="Lf")

#Load type 3 threshold values
type3_qtv<-as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group2.csv"))
type3_qtv$thr<-factor(type3_qtv$thr,levels=sort(unique(type3_qtv$thr)))
type3_qtv<-filter(type3_qtv,thr==0.1)

#keep only useful columns
type1_qtv<-type1_qtv[,c("station","indicator","SDC","RNV")]
type1_qtv$Type<-rep("type1",nrow(type1_qtv))

type3_qtv<-type3_qtv[,c("dataset_gear","indicator","dect_change","nv_15")]
type3_qtv$Type<-rep("type3",nrow(type3_qtv))


colnames(type3_qtv)<-c("dataset","indicator","SDC","RNV","Type")
colnames(type1_qtv)<-c("dataset","indicator","SDC","RNV","Type")

Type_df<-bind_rows(type1_qtv,type3_qtv)
Type_df$Type<-factor(Type_df$Type,levels=c("type1","type3"))

indicators<-unique(Type_df$indicator)


#Run beta models for SDC and RNV for each indicator
SDC_Type_model<-list()
RNV_Type_model<-list()
for (i in 1:length(indicators)) {
  SDC_Type_model[[i]]<- glmmTMB(
    SDC ~Type + (1|dataset),
    data = filter(Type_df,indicator==indicators[i]),
    family = beta_family(link="logit")
  )  
  
  RNV_Type_model[[i]]<- glmmTMB(
    RNV ~Type + (1|dataset),
    data = filter(Type_df,indicator==indicators[i]),
    family = beta_family(link="logit")
  )  
}

names(SDC_Type_model)<-indicators
names(RNV_Type_model)<-indicators

#check results
summary(SDC_Type_model[["total_biomass"]])
summary(SDC_Type_model[["total_abundance"]])
summary(SDC_Type_model[["richness"]])
summary(SDC_Type_model[["SoS"]])
summary(SDC_Type_model[["relM_biomass"]])
summary(SDC_Type_model[["relM_abundance"]])

summary(RNV_Type_model[["total_biomass"]])
summary(RNV_Type_model[["total_abundance"]])
summary(RNV_Type_model[["richness"]])
summary(RNV_Type_model[["SoS"]])
summary(RNV_Type_model[["relM_biomass"]])
summary(RNV_Type_model[["relM_abundance"]])

#Extract mean and CI from model outputs and evaluate eff.size as difference with associated significance
emm_type_RNV<-list()
emm_type_RNV_tbl<-list()
emm_type_SDC<-list()
emm_type_SDC_tbl<-list()
for (i in 1:length(indicators)) {
  emm_type_RNV[[i]] <- emmeans(RNV_Type_model[[i]], ~ Type, type = "response")
  PRS_RNV <- as.data.frame(contrast(emm_type_RNV[[i]], "revpairwise"))
  emm_type_RNV_tbl[[i]] <- as.data.frame(confint(emm_type_RNV[[i]])) %>%
    transmute(Type, mean = response, lcl = asymp.LCL, ucl = asymp.UCL)%>%
    summarise(method="RNV",indicator=indicators[i],delta_mean=diff(mean),delta_lcl=diff(lcl),delta_ucl=diff(ucl),p_value=PRS_RNV$p.value)
  
  emm_type_SDC[[i]] <- emmeans(SDC_Type_model[[i]], ~ Type, type = "response")
  PRS_SDC <- as.data.frame(contrast(emm_type_SDC[[i]], "revpairwise"))
  emm_type_SDC_tbl[[i]] <- as.data.frame(confint(emm_type_SDC[[i]])) %>%
    transmute(Type, mean = response, lcl = asymp.LCL, ucl = asymp.UCL)%>%
    summarise(method="SDC",indicator=indicators[i],delta_mean=diff(mean),delta_lcl=diff(lcl),delta_ucl=diff(ucl),p_value=PRS_SDC$p.value)
  
}

#Summary table for RNV and SDC
emm_type_RNV_tbl_df<-rbindlist(emm_type_RNV_tbl)
emm_type_SDC_tbl_df<-rbindlist(emm_type_SDC_tbl)



#####Extract QTV summaries for comparison with other methods#####

# Load data with finer k (group2)
thr_group2 <- as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group2.csv"))


#Filter data by most conservative pre-filtering SAR value
df_long_01 <- filter(thr_group2,thr==.1) %>%
  dplyr::select(indicator, nv_15, dect_change) %>%
  pivot_longer(c(nv_15, dect_change), names_to = "method", values_to = "qtv") %>%
  filter(!is.na(qtv), !is.na(indicator)) %>%
  mutate(
    method = factor(method, levels = c("nv_15", "dect_change")),
    qtv = qtv
  )

range_tab <- df_long_01 %>%
  group_by(indicator, method) %>%
  summarise(
    n = sum(!is.na(qtv)),
    qtv_min = min(qtv, na.rm = TRUE),
    qtv_q25=quantile(qtv,prob=.25,na.rm=T),
    qtv_median = median(qtv, na.rm = TRUE),
    qtv_q75=quantile(qtv,prob=.75,na.rm=T),
    qtv_max = max(qtv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(indicator, method)
fwrite(range_tab,"./WKBENTH4/sharepoint/WKBENTH4_Type3_kmeans_cluster_QTV_range_summary_table.csv")



#####Boxplot summaries####
# Load data
thr_group2 <- as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group2.csv"))

thr_group2$cluster_ID <- paste("group2", thr_group2$kmean_cluster_group2, sep = "_")

# make thr discrete for boxplots
thr_group2 <- thr_group2 %>%
  mutate(thr_f = factor(thr, levels = sort(unique(thr))))

df_long <- thr_group2 %>%
  mutate(thr_f = factor(thr, levels = sort(unique(thr)))) %>%
  dplyr::select(indicator, thr_f, nv_15, dect_change) %>%
  pivot_longer(cols = c(nv_15, dect_change),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value), !is.na(indicator), !is.na(thr_f)) %>%
  mutate(metric = factor(metric, levels = c("nv_15", "dect_change")))

df_long$indicator<-factor(df_long$indicator,levels=c("total_abundance","total_biomass","SoS","richness","relM_abundance","relM_biomass"))

#set colors for the boxplot
cols_fill  <- c(nv_15 = "#F4A582", dect_change = "#92C5DE")  # pastel orange / light blue
cols_line  <- c(nv_15 = "#B35806", dect_change = "#2166AC")  # darker outline tones

pd <- position_dodge(width = 0.75)

#filter by SAR value = 0.1
df_long_01<-filter(df_long, thr_f == 0.1)

type3_kmeans_boxplot<-ggplot(df_long_01,
                             aes(x = indicator, y = value, fill = metric, colour = metric)) +
  
  # dashed whiskers only (draw whiskers separately)
  stat_boxplot(geom = "errorbar", width = 0.35, position = pd,
               linewidth = 0.8) +
  
  # box (solid)
  geom_boxplot(width = 0.6, position = pd,
               linewidth = 0.8, outlier.alpha = 0.35, outlier.shape = 1) +
  
  # black mean point
  stat_summary(fun = median, geom = "point",
               position = pd, colour = "black", size = 2) +
  
  # your median labels at the bottom
  geom_text(data = filter(med_df2, thr_f == 0.1),
            aes(x = indicator, y = y_lab, label = lab, colour = metric),
            inherit.aes = FALSE, position = pd, size = 5, show.legend = FALSE) +
  
  scale_fill_manual(values = cols_fill,
                    labels = c(nv_15 = "Range of natural variation",
                               dect_change = "Statistically detectable change"),
                    name = NULL) +
  scale_colour_manual(values = cols_line, guide = "none") +
  
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0.00, 0.02))) +
  coord_cartesian(clip = "off") +
  
  labs(x = NULL, y = "QTV") +
  
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.margin = margin(5.5, 5.5, 28, 5.5) 
  )
type3_kmeans_boxplot
ggsave(plot=type3_kmeans_boxplot,filename = "./WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_QTV_boxplot_distribution.png",dpi=300,width=16,height=9)
