#########################################################################################
#
#                     Phase 4: Sensitivity analysis on clustering and threshold evaluation 
#                           QTV Sensitivity - fixed prefiltering technique
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


#Function for saving plots
save_plot <- function(p, outdir, fname, w = 12, h = 7, dpi = 300) {
  dir_create(outdir, recurse = TRUE)
  ggsave(filename = file.path(outdir, fname), plot = p, width = w, height = h, dpi = dpi)
}

# Load data
time_group1 <- as.data.frame(fread("./WKBENTH4/results/Type3_group1_Indices_time_series_predictions_matrices.csv"))
time_group2 <- as.data.frame(fread("./WKBENTH4/results/Type3_group2_Indices_time_series_predictions_matrices.csv"))

thr_group1 <- as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group1.csv"))
thr_group2 <- as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group2.csv"))

thr_group1$cluster_ID <- paste("group1", thr_group1$kmean_cluster_group1, sep = "_")
thr_group2$cluster_ID <- paste("group2", thr_group2$kmean_cluster_group2, sep = "_")

thr_group <- rbindlist(list(thr_group1, thr_group2), use.names = TRUE, fill = TRUE) %>%
  mutate(
    thr = as.numeric(thr),
    dataset_gear = as.factor(dataset_gear),
    indicator = as.factor(indicator),
    cluster_ID = as.factor(cluster_ID),
    dect_change_orig=dect_change*baseline,
    nv_025_orig=nv_025*baseline,
    nv_050_orig=nv_050*baseline,
    nv_15_orig=nv_15*baseline
  )

thr_group <- thr_group %>%
  mutate(
    group = case_when(
      str_detect(as.character(cluster_ID), "^group1_") ~ "group1",
      str_detect(as.character(cluster_ID), "^group2_") ~ "group2",
      TRUE ~ NA_character_
    ),
    group = factor(group, levels = c("group1", "group2"))
  )

thr_breaks <- sort(unique(thr_group$thr))

thr_group$thr<-factor(thr_group$thr,levels=sort(unique(thr_group$thr)))

qtv_long <- thr_group %>%
  dplyr::select(dataset_gear, indicator, thr, group, nv_15, dect_change) %>%
  pivot_longer(
    cols = c(nv_15, dect_change),
    names_to = "method",
    values_to = "qtv"
  ) %>%
  mutate(
    method = factor(method, levels = c("nv_15", "dect_change")),
    thr = factor(thr, levels = sort(unique(as.numeric(as.character(thr))))),
    group = factor(group, levels = c("group1","group2"))
  ) %>%
  filter(!is.na(qtv))

fit_one <- function(dat){
  
  
  mod <- glmmTMB(
    qtv ~ thr * group + (1|dataset_gear),
    data = dat,
    family = beta_family(link="logit")
  )
  
  # EMMs you likely want for reporting/plotting:
  # thr × group marginal means (response scale)
  emm_thr_grp <- emmeans(mod, ~ thr * group, type = "response")
  emm_tbl <- as.data.frame(confint(emm_thr_grp)) %>%
    transmute(thr, group, mean = response, lcl = asymp.LCL, ucl = asymp.UCL)
  
  # group differences within each thr (optional)
  grp_contr <- pairs(emmeans(mod, ~ thr | group, type="response"), adjust="BH") %>%
    as.data.frame()
  
  list(model = mod, emm = emm_tbl, pairs = grp_contr)
}

fits <- qtv_long %>%
  group_by(indicator, method) %>%
  group_split() %>%
  set_names(map_chr(., ~ paste(unique(.x$indicator), unique(.x$method), sep="_"))) %>%
  map(fit_one)



s<-list()
coef_tab<-list()
coef_tab2<-list()
p_range_wide<-list()
for (i in 1:length(fits)) {
  s[[i]] <- summary(fits[[i]]$model) 
  coef_tab[[i]] <- as.data.frame(s[[i]]$coefficients$cond)
  coef_tab[[i]]$names<-rownames(coef_tab[[i]])
  coef_tab2[[i]] <- coef_tab[[i]] %>%
    mutate(
      term = case_when(
        str_detect(names, "^thr") & str_detect(names, "group") ~ "interaction",
        str_detect(names, "^thr") ~ "thr",
        str_detect(names, "group") ~ "group",
        TRUE ~ "other"   # e.g. (Intercept)
      )
    )
  p_range_wide[[i]] <- coef_tab2[[i]] %>%
    filter(term %in% c("thr", "group", "interaction")) %>%
    group_by(term) %>%
    summarise(
      p_min = round(min(`Pr(>|z|)`, na.rm = TRUE),2),
      p_max = round(max(`Pr(>|z|)`, na.rm = TRUE),2),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = term,
      values_from = c(p_min, p_max),
      names_glue = "{term}_{.value}"
    )
}


p_range_wide_df<-rbindlist(p_range_wide)
p_range_wide_df$names<-names(fits)

fwrite(p_range_wide_df,"./WKBENTH4/sharepoint/WKBENTH4_Type3_kmean_cluster_QTV_thr_group_comparison_test_summary.csv")
