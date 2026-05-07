#########################################################################################
#
#                     Phase 3: Quality threshold evaluation
#                         QTV Summary - QTV at BHT level
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
library(broom.mixed)
library(emmeans)
library(glmmTMB)

rm(list = ls())
options(scipen = 999)
set.seed(666)


thr_group1<-as.data.frame(fread("G:/Il mio Drive/Ricerca/ICES/WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group1.csv"))
thr_group1$thr<-factor(thr_group1$thr)
thr_group1$dataset_gear<-factor(thr_group1$dataset_gear)
thr_group2<-as.data.frame(fread("G:/Il mio Drive/Ricerca/ICES/WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group2.csv"))
thr_group2$thr<-factor(thr_group2$thr)
thr_group2$dataset_gear<-factor(thr_group2$dataset_gear)


okabe_ito <- c(
"BoBIC_IberianChabitats_otter_trawl"  = "#E69F00", 
"CS_EVHOE_otter_trawl" = "#56B4E9", 
"CS_NS_IBTSFR_otter_trawl" = "#009E73",
"FR_ORHAGO_beam_trawl" = "#0072B2", 
"NS_DKhabitats_haps_corer" = "#D55E00", 
"WMS_EShabitats_otter_trawl" = "#CC79A7", 
"WMS_ISCMS_IRBIMCNR_otter_trawl" = "#000000"
)

pd <- position_dodge2(width = 0.7, preserve = "single")

# positions between indicator levels (works when indicator is a discrete axis)
n_thr <- nlevels(factor(thr_group1$thr))
sep_x <- seq(1.5, n_thr - 0.5, by = 1)


nv15_sum <- thr_group1 %>%
  group_by(indicator, thr, dataset_gear) %>%
  summarise(
    q25 = quantile(nv_15, 0.25, na.rm = TRUE),
    med = median(nv_15, na.rm = TRUE),
    q75 = quantile(nv_15, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

nv15_group <- ggplot(nv15_sum,
                     aes(x = thr, y = med, colour = dataset_gear, group = dataset_gear)) +
  geom_linerange(aes(ymin = q25, ymax = q75), position = pd, linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(y = q25), position = pd, size = 2.2) +
  geom_point(aes(y = med), position = pd, size = 2.2) +
  geom_point(aes(y = q75), position = pd, size = 2.2) +
  scale_colour_manual(values = okabe_ito) +
  facet_wrap(~ indicator, ncol = 3) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "SAR-filtering value", y = "RNV (q = 0.15)", colour = "Dataset / gear") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey95", colour = "grey20", linewidth = 0.6),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

nv15_group
ggsave(plot=nv15_group,filename ="G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_group1_nv15_barplot.png",dpi=300,width=16,height=9 )

dect_change_sum <- thr_group1 %>%
  group_by(indicator, thr, dataset_gear) %>%
  summarise(
    q25 = quantile(dect_change, 0.25, na.rm = TRUE),
    med = median(dect_change, na.rm = TRUE),
    q75 = quantile(dect_change, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

dect_change_group <- ggplot(dect_change_sum,
                     aes(x = thr, y = med, colour = dataset_gear, group = dataset_gear)) +
  geom_linerange(aes(ymin = q25, ymax = q75), position = pd, linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(y = q25), position = pd, size = 2.2) +
  geom_point(aes(y = med), position = pd, size = 2.2) +
  geom_point(aes(y = q75), position = pd, size = 2.2) +
  scale_colour_manual(values = okabe_ito) +
  facet_wrap(~ indicator, ncol = 3) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "SAR-filtering value", y = "SDC", colour = "Dataset / gear") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey95", colour = "grey20", linewidth = 0.6),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

dect_change_group
ggsave(plot=dect_change_group,filename ="G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_group1_dect_change_barplot.png",dpi=300,width=16,height=9 )



nv15_sum <- thr_group2 %>%
  group_by(indicator, thr, dataset_gear) %>%
  summarise(
    q25 = quantile(nv_15, 0.25, na.rm = TRUE),
    med = median(nv_15, na.rm = TRUE),
    q75 = quantile(nv_15, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

nv15_group <- ggplot(nv15_sum,
                     aes(x = thr, y = med, colour = dataset_gear, group = dataset_gear)) +
  geom_linerange(aes(ymin = q25, ymax = q75), position = pd, linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(y = q25), position = pd, size = 2.2) +
  geom_point(aes(y = med), position = pd, size = 2.2) +
  geom_point(aes(y = q75), position = pd, size = 2.2) +
  scale_colour_manual(values = okabe_ito) +
  facet_wrap(~ indicator, ncol = 3) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "SAR-filtering value", y = "RNV (q = 0.15)", colour = "Dataset / gear") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey95", colour = "grey20", linewidth = 0.6),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

nv15_group
ggsave(plot=nv15_group,filename ="G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_group2_nv15_barplot.png",dpi=300,width=16,height=9 )

dect_change_sum <- thr_group2 %>%
  group_by(indicator, thr, dataset_gear) %>%
  summarise(
    q25 = quantile(dect_change, 0.25, na.rm = TRUE),
    med = median(dect_change, na.rm = TRUE),
    q75 = quantile(dect_change, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

dect_change_group <- ggplot(dect_change_sum,
                            aes(x = thr, y = med, colour = dataset_gear, group = dataset_gear)) +
  geom_linerange(aes(ymin = q25, ymax = q75), position = pd, linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(y = q25), position = pd, size = 2.2) +
  geom_point(aes(y = med), position = pd, size = 2.2) +
  geom_point(aes(y = q75), position = pd, size = 2.2) +
  scale_colour_manual(values = okabe_ito) +
  facet_wrap(~ indicator, ncol = 3) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "SAR-filtering value", y = "SDC", colour = "Dataset / gear") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey95", colour = "grey20", linewidth = 0.6),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

dect_change_group
ggsave(plot=dect_change_group,filename ="G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_group2_dect_change_barplot.png",dpi=300,width=16,height=9 )


#####BHT QTV summaries####

habitat_qtv<-as.data.frame(fread("G:/Il mio Drive/Ricerca/ICES/WKBENTH4/results/WKBENTH4_Type3_reference_condition_threshold_kmean_cluster_group2_full_dataset.csv"))
habitat_qtv$thr<-factor(habitat_qtv$thr,levels=sort(unique(habitat_qtv$thr)))

habitat_qtv<-filter(habitat_qtv,thr==0.1)


indicators <- sort(unique(habitat_qtv$indicator))

qtab <- map_dfr(indicators, function(ind){
  
  dat <- habitat_qtv %>% 
    filter(indicator == ind) %>%
    filter(!is.na(habitat_type), !is.na(dataset_gear)) %>%
    filter(is.finite(nv_15)) %>%
    filter(nv_15 > 0, nv_15 < 1)  # beta family requires (0,1)
  
  m <- glmmTMB(
    nv_15 ~ habitat_type + (1|dataset_gear),
    data = dat,
    family = beta_family(link = "logit")
  )
  
  # habitat means on response scale -> mu_h
  emm_df <- as.data.frame(emmeans(m, ~ habitat_type, type = "response")) %>%
    rename(mu = response)
  
  # precision phi (common across habitats unless dispformula changes)
  phi <- exp(fixef(m)$disp[1])
  
  emm_df %>%
    mutate(
      indicator = ind,
      phi = phi,
      alpha = mu * phi,
      beta  = (1 - mu) * phi,
      q50 = qbeta(0.5, shape1 = alpha, shape2 = beta),
      q90 = qbeta(0.9, shape1 = alpha, shape2 = beta)
    ) %>%
    dplyr::select(indicator, habitat_type, mu, phi, q50, q90)
})

#Remove habitat that are not with high confidence

qtab<-filter(qtab,!habitat_type%in%c("Infralittoral rock and biogenic reef","Upper bathyal rock and biogenic reef",
                                     "Lower bathyal sediment/Lower bathyal rock and biogenic reef","Circalittoral rock and biogenic reef",
                                     "Na","Offshore circalittoral rock and biogenic reef","Upper bathyal rock and biogenic reef"))


qtab$method<-"nv_15"


qtab2 <- map_dfr(indicators, function(ind){
  
  dat <- habitat_qtv %>% 
    filter(indicator == ind) %>%
    filter(!is.na(habitat_type), !is.na(dataset_gear)) %>%
    filter(is.finite(dect_change)) %>%
    filter(dect_change > 0, dect_change < 1)  # beta family requires (0,1)
  
  m <- glmmTMB(
    dect_change ~ habitat_type + (1|dataset_gear),
    data = dat,
    family = beta_family(link = "logit")
  )
  
  # habitat means on response scale -> mu_h
  emm_df <- as.data.frame(emmeans(m, ~ habitat_type, type = "response")) %>%
    rename(mu = response)
  
  # precision phi (common across habitats unless dispformula changes)
  phi <- exp(fixef(m)$disp[1])
  
  emm_df %>%
    mutate(
      indicator = ind,
      phi = phi,
      alpha = mu * phi,
      beta  = (1 - mu) * phi,
      q50 = qbeta(0.5, shape1 = alpha, shape2 = beta),
      q90 = qbeta(0.9, shape1 = alpha, shape2 = beta)
    ) %>%
    dplyr::select(indicator, habitat_type, mu, phi, q50, q90)
})

#Remove habitat that are not with high confidence

qtab2<-filter(qtab2,!habitat_type%in%c("Infralittoral rock and biogenic reef","Upper bathyal rock and biogenic reef",
                                       "Lower bathyal sediment/Lower bathyal rock and biogenic reef","Circalittoral rock and biogenic reef",
                                       "Na","Offshore circalittoral rock and biogenic reef","Upper bathyal rock and biogenic reef"))


qtab2$method<-"dect_change"

qtab_final<-rbind(qtab,qtab2)

qtab_wide <- qtab_final %>%
  mutate(method = recode(method,
                         nv_15 = "nv15",
                         dect_change = "dectchange")) %>%
  dplyr::select(indicator, habitat_type, method, q50, q90) %>%
  pivot_wider(
    names_from = method,
    values_from = c(q50, q90),
    names_glue = "{.value}_{method}"
  ) %>%
  arrange(indicator, habitat_type)

fwrite(qtab_wide,"G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/WKBENTH_Type3_kmeans_cluster_habitat_q50_q90_summary_table.csv")


qtab_wide$habitat_type<-factor(qtab_wide$habitat_type,levels = c("Infralittoral mud","Infralittoral mixed sediment","Infralittoral rock and biogenic reef","Circalittoral mud","Circalittoral sand","Circalittoral mixed sediment","Circalittoral coarse sediment","Circalittoral rock and biogenic reef","Offshore circalittoral mud","Offshore circalittoral sand","Offshore circalittoral mud/Offshore circalittoral sand","Offshore circalittoral mixed sediment","Offshore circalittoral coarse sediment","Offshore circalittoral rock and biogenic reef","Upper bathyal sediment","Lower bathyal sediment","Lower bathyal sediment/Upper bathyal sediment","Lower bathyal sediment/Lower bathyal rock and biogenic reef","Upper bathyal rock and biogenic reef","Na"))
qtab_wide$indicator<-factor(qtab_wide$indicator,levels = c("total_abundance","total_biomass","SoS","richness","relM_abundance","relM_biomass"))


okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

pd <- position_dodge2(width = 0.7, preserve = "single")

# positions between indicator levels (works when indicator is a discrete axis)
n_ind <- nlevels(factor(qtab_wide$indicator))
sep_x <- seq(1.5, n_ind - 0.5, by = 1)

nv15_habitat<-ggplot(qtab_wide, aes(x = indicator, colour = habitat_type, group = habitat_type)) +
  # separator lines between indicator rows (will become horizontal after coord_flip)
  geom_vline(xintercept = sep_x, colour = "black", linewidth = 0.4) +
  geom_linerange(aes(ymin = q50_nv15, ymax = q90_nv15),
                 position = pd, linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(y = q50_nv15), position = pd, size = 2.3) +
  geom_point(aes(y = q90_nv15), position = pd, size = 2.3) +
  scale_colour_manual(values = okabe_ito) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Indicator", y = "RNV (Q50 - Q90)", colour = "Habitat type") +
  coord_flip() +
  theme_minimal()
nv15_habitat <- nv15_habitat +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14)
  )
ggsave(plot=nv15_habitat,filename ="G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_habitat_nv15_barplot.png",dpi=300,width=16,height=9 )

dect_habitat<-ggplot(qtab_wide, aes(x = indicator, colour = habitat_type, group = habitat_type)) +
  # separator lines between indicator rows (will become horizontal after coord_flip)
  geom_vline(xintercept = sep_x, colour = "black", linewidth = 0.4) +
  geom_linerange(aes(ymin = q50_dectchange, ymax = q90_dectchange),
                 position = pd, linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(y = q50_dectchange), position = pd, size = 2.3) +
  geom_point(aes(y = q90_dectchange), position = pd, size = 2.3) +
  scale_colour_manual(values = okabe_ito) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Indicator", y = "SDC (Q50 - Q90)", colour = "Habitat type") +
  coord_flip() +
  theme_minimal()

dect_habitat <- dect_habitat +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14)
  )
ggsave(plot=dect_habitat,filename ="G:/Il mio Drive/Ricerca/ICES/WKBENTH4/sharepoint/Figure/WKBENTH4_Type3_kmean_cluster_habitat_dectchange_barplot.png",dpi=300,width=16,height=9 )
