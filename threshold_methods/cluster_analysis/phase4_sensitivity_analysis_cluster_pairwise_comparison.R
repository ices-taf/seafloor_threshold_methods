#########################################################################################
#
#               Phase 4: Sensitivity analysis on clustering and threshold evaluation 
#             Check selected kmean cluster  indicator distribution vs removed observations 
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
library(rstatix)

rm(list = ls())
options(scipen = 999)
set.seed(666)


# ---- load data ----
cluster_group1 <- as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_pressuretype_fixedpthr_kmeans_clusters_group1.csv"))
cluster_group2 <- as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_pressuretype_fixedpthr_kmeans_clusters_group2.csv"))

pairwise_mean_spat_success_group1<-as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_reference_condition_kmean_clusters_group1_spatial_metrics.csv"))
pairwise_mean_spat_success_group2<-as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_reference_condition_kmean_clusters_group2_spatial_metrics.csv"))




#Load dataset
dataset.merged<-as.data.frame(fread("./WKBENTH4/data/WKBENTH4_datacall_merged.csv"))

dataset.merged$dataset_gear<-paste(dataset.merged$dataset,dataset.merged$gear,sep="_")
dataset.merged$sample_ID

#Load spatial metrics of indicator
spatial.metrics<-as.data.frame(fread("./WKBENTH4/results/Type3_Indices_variogram_models_matrices.csv"))

#Remove dataset_gear not relevant in the threshold analysis
dataset.merged<-filter(dataset.merged,dataset_gear%in%c("BoBIC_IberianChabitats_otter_trawl","CS_EVHOE_otter_trawl",
                                                        "CS_NS_IBTSFR_otter_trawl","FR_ORHAGO_beam_trawl",
                                                        "WMS_EShabitats_otter_trawl","WMS_ISCMS_IRBIMCNR_otter_trawl"))

#Fix unrealistic relM_abundance and relM_biomass 
dataset.merged$relM_biomass<-ifelse(dataset.merged$relM_biomass<=0,0.01,dataset.merged$relM_biomass)
dataset.merged$relM_abundance<-ifelse(dataset.merged$relM_abundance<=0,0.01,dataset.merged$relM_abundance)


#select the thr value to analyze
thr_value<-c(0.1,0.25,0.5,0.65,0.75,1)
ind <- c("SoS", "richness", "total_biomass", "total_abundance",
         "relM_biomass", "relM_abundance")


#####SoS####
cluster_pairwise_SoS <- function(cluster_group, dataset, thr_value, min_n = 2) {
  
  cluster_group <- cluster_group %>%
    filter(thr == thr_value) %>%
    select(sample_ID, kmean_cluster, thr)
  
  dataset.merged.clusters <- dataset %>%
    left_join(cluster_group, by = "sample_ID") %>%
    mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster)),
      kmean_cluster = factor(kmean_cluster)
    ) %>%
    filter(is.finite(SoS), !is.na(SoS))
  
  # (optional) drop tiny cluster levels to prevent "not enough obs"
  if (!is.null(min_n) && min_n > 1) {
    dataset.merged.clusters <- dataset.merged.clusters %>%
      group_by(dataset_gear, kmean_cluster) %>%
      filter(dplyr::n() >= min_n) %>%
      ungroup()
  }
  
  # run per dataset_gear using base pairwise.wilcox.test (robust), then tidy
  res <- lapply(split(dataset.merged.clusters, dataset.merged.clusters$dataset_gear), function(df) {
    
    # need at least two groups present
    if (n_distinct(df$kmean_cluster) < 2) return(NULL)
    if (!("removed" %in% levels(df$kmean_cluster))) return(NULL)
    
    # run pairwise test (BH within this dataset_gear)
    pw <- tryCatch(
      pairwise.wilcox.test(df$SoS, df$kmean_cluster, p.adjust.method = "BH"),
      error = function(e) NULL
    )
    if (is.null(pw)) return(NULL)
    
    # pw$p.value is a lower-tri matrix; convert to long
    pmat <- pw$p.value
    if (is.null(pmat) || length(pmat) == 0) return(NULL)
    
    out <- as.data.frame(as.table(pmat), stringsAsFactors = FALSE) %>%
      rename(group1 = Var1, group2 = Var2, p.adj = Freq) %>%
      filter(!is.na(p.adj)) %>%
      # keep only comparisons involving "removed"
      filter(group1 == "removed" | group2 == "removed") %>%
      mutate(
        cluster = ifelse(group1 == "removed", group2, group1)
      )
    
    out
  })
  
  out <- bind_rows(res, .id = "dataset_gear") %>%
    mutate(thr_value = thr_value)
  
  out
}



SoS_pairwise_group1<-list()
for (i in 1:length(thr_value)) {
  SoS_pairwise_group1[[i]]<-cluster_pairwise_SoS(cluster_group1,dataset.merged,thr_value[i]) 
}

SoS_pairwise_group1_df<-rbindlist(SoS_pairwise_group1)
SoS_pairwise_group1_df<-filter(SoS_pairwise_group1_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group1)
  )

SoS_pairwise_group1_df <- SoS_pairwise_group1_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster"))



SoS_group1_ns_summary <- SoS_pairwise_group1_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )


SoS_group1_ns_summary_plot<-ggplot(SoS_group1_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(SoS_group1_ns_summary$thr_value))) +
  labs(title="SoS coarser k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
SoS_group1_ns_summary_plot

ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_SoS_group1_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = SoS_group1_ns_summary_plot, width = 16, height = 9, dpi = 300)

SoS_pairwise_group2<-list()
for (i in 1:length(thr_value)) {
  SoS_pairwise_group2[[i]]<-cluster_pairwise_SoS(cluster_group2,dataset.merged,thr_value[i]) 
}

SoS_pairwise_group2_df<-rbindlist(SoS_pairwise_group2)
SoS_pairwise_group2_df<-filter(SoS_pairwise_group2_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group2)
  )

SoS_pairwise_group2_df <- SoS_pairwise_group2_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster"))


SoS_group2_ns_summary <- SoS_pairwise_group2_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

SoS_group2_ns_summary_plot<-ggplot(SoS_group2_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(SoS_group2_ns_summary$thr_value))) +
  labs(title="SoS finer k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
SoS_group2_ns_summary_plot
ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_SoS_group2_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = SoS_group2_ns_summary_plot, width = 16, height = 9, dpi = 300)


make_cluster_boxplot_highlight_removed <- function(cluster_group, dataset, thr_value,
                                                   title = "",
                                                   top_n_clusters = NULL,
                                                   drop_rare = TRUE,
                                                   min_n = 2) {
  
  cg <- cluster_group %>%
    dplyr::filter(thr == thr_value) %>%
    dplyr::select(sample_ID, kmean_cluster, thr)
  
  dat_plot <- dataset %>%
    dplyr::left_join(cg, by = "sample_ID") %>%
    dplyr::mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster))
    ) %>%
    dplyr::filter(is.finite(SoS), !is.na(SoS))
  
  if (drop_rare) {
    dat_plot <- dat_plot %>%
      dplyr::group_by(dataset_gear, kmean_cluster) %>%
      dplyr::filter(dplyr::n() >= min_n) %>%
      dplyr::ungroup()
  }
  
  if (!is.null(top_n_clusters)) {
    keep_levels <- dat_plot %>%
      dplyr::filter(kmean_cluster != "removed") %>%
      dplyr::count(dataset_gear, kmean_cluster, name = "n") %>%
      dplyr::group_by(dataset_gear) %>%
      dplyr::slice_max(order_by = n, n = top_n_clusters, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset_gear, kmean_cluster)
    
    dat_plot <- dat_plot %>%
      dplyr::left_join(keep_levels %>% dplyr::mutate(keep = TRUE),
                       by = c("dataset_gear", "kmean_cluster")) %>%
      dplyr::filter(kmean_cluster == "removed" | keep) %>%
      dplyr::select(-keep)
  }
  
  # ---- TRUE ordering per dataset_gear using a facet-specific key ----
  ord_tbl <- dat_plot %>%
    dplyr::group_by(dataset_gear, kmean_cluster) %>%
    dplyr::summarise(med = stats::median(SoS, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dataset_gear) %>%
    dplyr::arrange(dplyr::desc(kmean_cluster == "removed"), med, .by_group = TRUE) %>%  # removed first, then median
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"))
  
  # levels ordered within each dataset_gear (and concatenated across dataset_gears)
  key_levels <- ord_tbl %>% dplyr::pull(cluster_key)
  
  dat_plot <- dat_plot %>%
    dplyr::mutate(
      cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"),
      cluster_key = factor(cluster_key, levels = key_levels)
    )
  
  dat_other <- dat_plot %>% dplyr::filter(kmean_cluster != "removed") %>% droplevels()
  dat_rem   <- dat_plot %>% dplyr::filter(kmean_cluster == "removed") %>% droplevels()
  
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = dat_other,
      ggplot2::aes(x = cluster_key, y = SoS),
      width = 0.55, outlier.alpha = 0.12, linewidth = 0.25
    ) +
    ggplot2::geom_boxplot(
      data = dat_rem,
      ggplot2::aes(x = cluster_key, y = SoS),
      width = 0.60, fill = NA, colour = "black", linewidth = 0.7,
      outlier.alpha = 0.25
    ) +
    ggplot2::facet_wrap(~dataset_gear, ncol = 3, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("thr = ", thr_value, " | clusters ordered by median"),
      x = "kmean_cluster",
      y = "SoS"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )
}

# loop over all thr values and save plots for both group1 and group2

outdir <- "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/boxplots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

for (th in thr_value) {
  
  # ---- Group 1 (coarser k) ----
  p_box1 <- make_cluster_boxplot_highlight_removed(
    cluster_group1, dataset.merged, th,
    title = "SoS boxplots — Coarser K optimum"
  )
  
  out_file1 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_SoS_group1_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file1, plot = p_box1, width = 16, height = 9, dpi = 300)
  
  
  # ---- Group 2 (finer k) ----
  p_box2 <- make_cluster_boxplot_highlight_removed(
    cluster_group2, dataset.merged, th,
    title = "SoS boxplots — Finer K optimum"
  )
  
  out_file2 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_SoS_group2_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file2, plot = p_box2, width = 16, height = 9, dpi = 300)
}

#####richness####
cluster_pairwise_richness <- function(cluster_group, dataset, thr_value, min_n = 2) {
  
  cluster_group <- cluster_group %>%
    filter(thr == thr_value) %>%
    select(sample_ID, kmean_cluster, thr)
  
  dataset.merged.clusters <- dataset %>%
    left_join(cluster_group, by = "sample_ID") %>%
    mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster)),
      kmean_cluster = factor(kmean_cluster)
    ) %>%
    filter(is.finite(richness), !is.na(richness))
  
  # (optional) drop tiny cluster levels to prevent "not enough obs"
  if (!is.null(min_n) && min_n > 1) {
    dataset.merged.clusters <- dataset.merged.clusters %>%
      group_by(dataset_gear, kmean_cluster) %>%
      filter(dplyr::n() >= min_n) %>%
      ungroup()
  }
  
  # run per dataset_gear using base pairwise.wilcox.test (robust), then tidy
  res <- lapply(split(dataset.merged.clusters, dataset.merged.clusters$dataset_gear), function(df) {
    
    # need at least two groups present
    if (n_distinct(df$kmean_cluster) < 2) return(NULL)
    if (!("removed" %in% levels(df$kmean_cluster))) return(NULL)
    
    # run pairwise test (BH within this dataset_gear)
    pw <- tryCatch(
      pairwise.wilcox.test(df$richness, df$kmean_cluster, p.adjust.method = "BH"),
      error = function(e) NULL
    )
    if (is.null(pw)) return(NULL)
    
    # pw$p.value is a lower-tri matrix; convert to long
    pmat <- pw$p.value
    if (is.null(pmat) || length(pmat) == 0) return(NULL)
    
    out <- as.data.frame(as.table(pmat), stringsAsFactors = FALSE) %>%
      rename(group1 = Var1, group2 = Var2, p.adj = Freq) %>%
      filter(!is.na(p.adj)) %>%
      # keep only comparisons involving "removed"
      filter(group1 == "removed" | group2 == "removed") %>%
      mutate(
        cluster = ifelse(group1 == "removed", group2, group1)
      )
    
    out
  })
  
  out <- bind_rows(res, .id = "dataset_gear") %>%
    mutate(thr_value = thr_value)
  
  out
}



richness_pairwise_group1<-list()
for (i in 1:length(thr_value)) {
  richness_pairwise_group1[[i]]<-cluster_pairwise_richness(cluster_group1,dataset.merged,thr_value[i]) 
}

richness_pairwise_group1_df<-rbindlist(richness_pairwise_group1)
richness_pairwise_group1_df<-filter(richness_pairwise_group1_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group1)
  )

richness_pairwise_group1_df <- richness_pairwise_group1_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster"))



richness_group1_ns_summary <- richness_pairwise_group1_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

richness_group1_ns_summary_plot<-ggplot(richness_group1_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(richness_group1_ns_summary$thr_value))) +
  labs(title="richness coarser k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
richness_group1_ns_summary_plot

ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_richness_group1_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = richness_group1_ns_summary_plot, width = 16, height = 9, dpi = 300)

richness_pairwise_group2<-list()
for (i in 1:length(thr_value)) {
  richness_pairwise_group2[[i]]<-cluster_pairwise_richness(cluster_group2,dataset.merged,thr_value[i]) 
}

richness_pairwise_group2_df<-rbindlist(richness_pairwise_group2)
richness_pairwise_group2_df<-filter(richness_pairwise_group2_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group2)
  )

richness_pairwise_group2_df <- richness_pairwise_group2_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster"))


richness_group2_ns_summary <- richness_pairwise_group2_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

richness_group2_ns_summary_plot<-ggplot(richness_group2_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(richness_group2_ns_summary$thr_value))) +
  labs(title="richness finer k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
richness_group2_ns_summary_plot
ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_richness_group2_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = richness_group2_ns_summary_plot, width = 16, height = 9, dpi = 300)


make_cluster_boxplot_highlight_removed <- function(cluster_group, dataset, thr_value,
                                                   title = "",
                                                   top_n_clusters = NULL,
                                                   drop_rare = TRUE,
                                                   min_n = 2) {
  
  cg <- cluster_group %>%
    dplyr::filter(thr == thr_value) %>%
    dplyr::select(sample_ID, kmean_cluster, thr)
  
  dat_plot <- dataset %>%
    dplyr::left_join(cg, by = "sample_ID") %>%
    dplyr::mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster))
    ) %>%
    dplyr::filter(is.finite(richness), !is.na(richness))
  
  if (drop_rare) {
    dat_plot <- dat_plot %>%
      dplyr::group_by(dataset_gear, kmean_cluster) %>%
      dplyr::filter(dplyr::n() >= min_n) %>%
      dplyr::ungroup()
  }
  
  if (!is.null(top_n_clusters)) {
    keep_levels <- dat_plot %>%
      dplyr::filter(kmean_cluster != "removed") %>%
      dplyr::count(dataset_gear, kmean_cluster, name = "n") %>%
      dplyr::group_by(dataset_gear) %>%
      dplyr::slice_max(order_by = n, n = top_n_clusters, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset_gear, kmean_cluster)
    
    dat_plot <- dat_plot %>%
      dplyr::left_join(keep_levels %>% dplyr::mutate(keep = TRUE),
                       by = c("dataset_gear", "kmean_cluster")) %>%
      dplyr::filter(kmean_cluster == "removed" | keep) %>%
      dplyr::select(-keep)
  }
  
  # ---- TRUE ordering per dataset_gear using a facet-specific key ----
  ord_tbl <- dat_plot %>%
    dplyr::group_by(dataset_gear, kmean_cluster) %>%
    dplyr::summarise(med = stats::median(richness, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dataset_gear) %>%
    dplyr::arrange(dplyr::desc(kmean_cluster == "removed"), med, .by_group = TRUE) %>%  # removed first, then median
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"))
  
  # levels ordered within each dataset_gear (and concatenated across dataset_gears)
  key_levels <- ord_tbl %>% dplyr::pull(cluster_key)
  
  dat_plot <- dat_plot %>%
    dplyr::mutate(
      cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"),
      cluster_key = factor(cluster_key, levels = key_levels)
    )
  
  dat_other <- dat_plot %>% dplyr::filter(kmean_cluster != "removed") %>% droplevels()
  dat_rem   <- dat_plot %>% dplyr::filter(kmean_cluster == "removed") %>% droplevels()
  
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = dat_other,
      ggplot2::aes(x = cluster_key, y = richness),
      width = 0.55, outlier.alpha = 0.12, linewidth = 0.25
    ) +
    ggplot2::geom_boxplot(
      data = dat_rem,
      ggplot2::aes(x = cluster_key, y = richness),
      width = 0.60, fill = NA, colour = "black", linewidth = 0.7,
      outlier.alpha = 0.25
    ) +
    ggplot2::facet_wrap(~dataset_gear, ncol = 3, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("thr = ", thr_value, " | clusters ordered by median"),
      x = "kmean_cluster",
      y = "richness"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )
}

# loop over all thr values and save plots for both group1 and group2

outdir <- "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/boxplots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

for (th in thr_value) {
  
  # ---- Group 1 (coarser k) ----
  p_box1 <- make_cluster_boxplot_highlight_removed(
    cluster_group1, dataset.merged, th,
    title = "richness boxplots — Coarser K optimum"
  )
  
  out_file1 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_richness_group1_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file1, plot = p_box1, width = 16, height = 9, dpi = 300)
  
  
  # ---- Group 2 (finer k) ----
  p_box2 <- make_cluster_boxplot_highlight_removed(
    cluster_group2, dataset.merged, th,
    title = "richness boxplots — Finer K optimum"
  )
  
  out_file2 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_richness_group2_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file2, plot = p_box2, width = 16, height = 9, dpi = 300)
}

#####total_biomass####
cluster_pairwise_total_biomass <- function(cluster_group, dataset, thr_value, min_n = 2) {
  
  cluster_group <- cluster_group %>%
    filter(thr == thr_value) %>%
    select(sample_ID, kmean_cluster, thr)
  
  dataset.merged.clusters <- dataset %>%
    left_join(cluster_group, by = "sample_ID") %>%
    mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster)),
      kmean_cluster = factor(kmean_cluster)
    ) %>%
    filter(is.finite(total_biomass), !is.na(total_biomass))
  
  # (optional) drop tiny cluster levels to prevent "not enough obs"
  if (!is.null(min_n) && min_n > 1) {
    dataset.merged.clusters <- dataset.merged.clusters %>%
      group_by(dataset_gear, kmean_cluster) %>%
      filter(dplyr::n() >= min_n) %>%
      ungroup()
  }
  
  # run per dataset_gear using base pairwise.wilcox.test (robust), then tidy
  res <- lapply(split(dataset.merged.clusters, dataset.merged.clusters$dataset_gear), function(df) {
    
    # need at least two groups present
    if (n_distinct(df$kmean_cluster) < 2) return(NULL)
    if (!("removed" %in% levels(df$kmean_cluster))) return(NULL)
    
    # run pairwise test (BH within this dataset_gear)
    pw <- tryCatch(
      pairwise.wilcox.test(df$total_biomass, df$kmean_cluster, p.adjust.method = "BH"),
      error = function(e) NULL
    )
    if (is.null(pw)) return(NULL)
    
    # pw$p.value is a lower-tri matrix; convert to long
    pmat <- pw$p.value
    if (is.null(pmat) || length(pmat) == 0) return(NULL)
    
    out <- as.data.frame(as.table(pmat), stringsAsFactors = FALSE) %>%
      rename(group1 = Var1, group2 = Var2, p.adj = Freq) %>%
      filter(!is.na(p.adj)) %>%
      # keep only comparisons involving "removed"
      filter(group1 == "removed" | group2 == "removed") %>%
      mutate(
        cluster = ifelse(group1 == "removed", group2, group1)
      )
    
    out
  })
  
  out <- bind_rows(res, .id = "dataset_gear") %>%
    mutate(thr_value = thr_value)
  
  out
}



total_biomass_pairwise_group1<-list()
for (i in 1:length(thr_value)) {
  total_biomass_pairwise_group1[[i]]<-cluster_pairwise_total_biomass(cluster_group1,dataset.merged,thr_value[i]) 
}

total_biomass_pairwise_group1_df<-rbindlist(total_biomass_pairwise_group1)
total_biomass_pairwise_group1_df<-filter(total_biomass_pairwise_group1_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group1)
  )

total_biomass_pairwise_group1_df <- total_biomass_pairwise_group1_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster"))



total_biomass_group1_ns_summary <- total_biomass_pairwise_group1_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

total_biomass_group1_ns_summary_plot<-ggplot(total_biomass_group1_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(total_biomass_group1_ns_summary$thr_value))) +
  labs(title="total_biomass coarser k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
total_biomass_group1_ns_summary_plot

ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_total_biomass_group1_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = total_biomass_group1_ns_summary_plot, width = 16, height = 9, dpi = 300)

total_biomass_pairwise_group2<-list()
for (i in 1:length(thr_value)) {
  total_biomass_pairwise_group2[[i]]<-cluster_pairwise_total_biomass(cluster_group2,dataset.merged,thr_value[i]) 
}

total_biomass_pairwise_group2_df<-rbindlist(total_biomass_pairwise_group2)
total_biomass_pairwise_group2_df<-filter(total_biomass_pairwise_group2_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group2)
  )

total_biomass_pairwise_group2_df <- total_biomass_pairwise_group2_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster"))


total_biomass_group2_ns_summary <- total_biomass_pairwise_group2_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

total_biomass_group2_ns_summary_plot<-ggplot(total_biomass_group2_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(total_biomass_group2_ns_summary$thr_value))) +
  labs(title="total_biomass finer k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
total_biomass_group2_ns_summary_plot
ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_total_biomass_group2_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = total_biomass_group2_ns_summary_plot, width = 16, height = 9, dpi = 300)


make_cluster_boxplot_highlight_removed <- function(cluster_group, dataset, thr_value,
                                                   title = "",
                                                   top_n_clusters = NULL,
                                                   drop_rare = TRUE,
                                                   min_n = 2) {
  
  cg <- cluster_group %>%
    dplyr::filter(thr == thr_value) %>%
    dplyr::select(sample_ID, kmean_cluster, thr)
  
  dat_plot <- dataset %>%
    dplyr::left_join(cg, by = "sample_ID") %>%
    dplyr::mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster))
    ) %>%
    dplyr::filter(is.finite(total_biomass), !is.na(total_biomass))
  
  if (drop_rare) {
    dat_plot <- dat_plot %>%
      dplyr::group_by(dataset_gear, kmean_cluster) %>%
      dplyr::filter(dplyr::n() >= min_n) %>%
      dplyr::ungroup()
  }
  
  if (!is.null(top_n_clusters)) {
    keep_levels <- dat_plot %>%
      dplyr::filter(kmean_cluster != "removed") %>%
      dplyr::count(dataset_gear, kmean_cluster, name = "n") %>%
      dplyr::group_by(dataset_gear) %>%
      dplyr::slice_max(order_by = n, n = top_n_clusters, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset_gear, kmean_cluster)
    
    dat_plot <- dat_plot %>%
      dplyr::left_join(keep_levels %>% dplyr::mutate(keep = TRUE),
                       by = c("dataset_gear", "kmean_cluster")) %>%
      dplyr::filter(kmean_cluster == "removed" | keep) %>%
      dplyr::select(-keep)
  }
  
  # ---- TRUE ordering per dataset_gear using a facet-specific key ----
  ord_tbl <- dat_plot %>%
    dplyr::group_by(dataset_gear, kmean_cluster) %>%
    dplyr::summarise(med = stats::median(total_biomass, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dataset_gear) %>%
    dplyr::arrange(dplyr::desc(kmean_cluster == "removed"), med, .by_group = TRUE) %>%  # removed first, then median
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"))
  
  # levels ordered within each dataset_gear (and concatenated across dataset_gears)
  key_levels <- ord_tbl %>% dplyr::pull(cluster_key)
  
  dat_plot <- dat_plot %>%
    dplyr::mutate(
      cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"),
      cluster_key = factor(cluster_key, levels = key_levels)
    )
  
  dat_other <- dat_plot %>% dplyr::filter(kmean_cluster != "removed") %>% droplevels()
  dat_rem   <- dat_plot %>% dplyr::filter(kmean_cluster == "removed") %>% droplevels()
  
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = dat_other,
      ggplot2::aes(x = cluster_key, y = total_biomass),
      width = 0.55, outlier.alpha = 0.12, linewidth = 0.25
    ) +
    ggplot2::geom_boxplot(
      data = dat_rem,
      ggplot2::aes(x = cluster_key, y = total_biomass),
      width = 0.60, fill = NA, colour = "black", linewidth = 0.7,
      outlier.alpha = 0.25
    ) +
    ggplot2::facet_wrap(~dataset_gear, ncol = 3, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("thr = ", thr_value, " | clusters ordered by median"),
      x = "kmean_cluster",
      y = "total_biomass"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )
}

# loop over all thr values and save plots for both group1 and group2

outdir <- "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/boxplots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

for (th in thr_value) {
  
  # ---- Group 1 (coarser k) ----
  p_box1 <- make_cluster_boxplot_highlight_removed(
    cluster_group1, dataset.merged, th,
    title = "total_biomass boxplots — Coarser K optimum"
  )
  
  out_file1 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_total_biomass_group1_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file1, plot = p_box1, width = 16, height = 9, dpi = 300)
  
  
  # ---- Group 2 (finer k) ----
  p_box2 <- make_cluster_boxplot_highlight_removed(
    cluster_group2, dataset.merged, th,
    title = "total_biomass boxplots — Finer K optimum"
  )
  
  out_file2 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_total_biomass_group2_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file2, plot = p_box2, width = 16, height = 9, dpi = 300)
}

#####total_abundance####
cluster_pairwise_total_abundance <- function(cluster_group, dataset, thr_value, min_n = 2) {
  
  cluster_group <- cluster_group %>%
    filter(thr == thr_value) %>%
    select(sample_ID, kmean_cluster, thr)
  
  dataset.merged.clusters <- dataset %>%
    left_join(cluster_group, by = "sample_ID") %>%
    mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster)),
      kmean_cluster = factor(kmean_cluster)
    ) %>%
    filter(is.finite(total_abundance), !is.na(total_abundance))
  
  # (optional) drop tiny cluster levels to prevent "not enough obs"
  if (!is.null(min_n) && min_n > 1) {
    dataset.merged.clusters <- dataset.merged.clusters %>%
      group_by(dataset_gear, kmean_cluster) %>%
      filter(dplyr::n() >= min_n) %>%
      ungroup()
  }
  
  # run per dataset_gear using base pairwise.wilcox.test (robust), then tidy
  res <- lapply(split(dataset.merged.clusters, dataset.merged.clusters$dataset_gear), function(df) {
    
    # need at least two groups present
    if (n_distinct(df$kmean_cluster) < 2) return(NULL)
    if (!("removed" %in% levels(df$kmean_cluster))) return(NULL)
    
    # run pairwise test (BH within this dataset_gear)
    pw <- tryCatch(
      pairwise.wilcox.test(df$total_abundance, df$kmean_cluster, p.adjust.method = "BH"),
      error = function(e) NULL
    )
    if (is.null(pw)) return(NULL)
    
    # pw$p.value is a lower-tri matrix; convert to long
    pmat <- pw$p.value
    if (is.null(pmat) || length(pmat) == 0) return(NULL)
    
    out <- as.data.frame(as.table(pmat), stringsAsFactors = FALSE) %>%
      rename(group1 = Var1, group2 = Var2, p.adj = Freq) %>%
      filter(!is.na(p.adj)) %>%
      # keep only comparisons involving "removed"
      filter(group1 == "removed" | group2 == "removed") %>%
      mutate(
        cluster = ifelse(group1 == "removed", group2, group1)
      )
    
    out
  })
  
  out <- bind_rows(res, .id = "dataset_gear") %>%
    mutate(thr_value = thr_value)
  
  out
}



total_abundance_pairwise_group1<-list()
for (i in 1:length(thr_value)) {
  total_abundance_pairwise_group1[[i]]<-cluster_pairwise_total_abundance(cluster_group1,dataset.merged,thr_value[i]) 
}

total_abundance_pairwise_group1_df<-rbindlist(total_abundance_pairwise_group1)
total_abundance_pairwise_group1_df<-filter(total_abundance_pairwise_group1_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group1)
  )

total_abundance_pairwise_group1_df <- total_abundance_pairwise_group1_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster"))



total_abundance_group1_ns_summary <- total_abundance_pairwise_group1_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

total_abundance_group1_ns_summary_plot<-ggplot(total_abundance_group1_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(total_abundance_group1_ns_summary$thr_value))) +
  labs(title="total_abundance coarser k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
total_abundance_group1_ns_summary_plot

ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_total_abundance_group1_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = total_abundance_group1_ns_summary_plot, width = 16, height = 9, dpi = 300)

total_abundance_pairwise_group2<-list()
for (i in 1:length(thr_value)) {
  total_abundance_pairwise_group2[[i]]<-cluster_pairwise_total_abundance(cluster_group2,dataset.merged,thr_value[i]) 
}

total_abundance_pairwise_group2_df<-rbindlist(total_abundance_pairwise_group2)
total_abundance_pairwise_group2_df<-filter(total_abundance_pairwise_group2_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group2)
  )

total_abundance_pairwise_group2_df <- total_abundance_pairwise_group2_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster"))


total_abundance_group2_ns_summary <- total_abundance_pairwise_group2_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

total_abundance_group2_ns_summary_plot<-ggplot(total_abundance_group2_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(total_abundance_group2_ns_summary$thr_value))) +
  labs(title="total_abundance finer k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
total_abundance_group2_ns_summary_plot
ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_total_abundance_group2_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = total_abundance_group2_ns_summary_plot, width = 16, height = 9, dpi = 300)


make_cluster_boxplot_highlight_removed <- function(cluster_group, dataset, thr_value,
                                                   title = "",
                                                   top_n_clusters = NULL,
                                                   drop_rare = TRUE,
                                                   min_n = 2) {
  
  cg <- cluster_group %>%
    dplyr::filter(thr == thr_value) %>%
    dplyr::select(sample_ID, kmean_cluster, thr)
  
  dat_plot <- dataset %>%
    dplyr::left_join(cg, by = "sample_ID") %>%
    dplyr::mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster))
    ) %>%
    dplyr::filter(is.finite(total_abundance), !is.na(total_abundance))
  
  if (drop_rare) {
    dat_plot <- dat_plot %>%
      dplyr::group_by(dataset_gear, kmean_cluster) %>%
      dplyr::filter(dplyr::n() >= min_n) %>%
      dplyr::ungroup()
  }
  
  if (!is.null(top_n_clusters)) {
    keep_levels <- dat_plot %>%
      dplyr::filter(kmean_cluster != "removed") %>%
      dplyr::count(dataset_gear, kmean_cluster, name = "n") %>%
      dplyr::group_by(dataset_gear) %>%
      dplyr::slice_max(order_by = n, n = top_n_clusters, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset_gear, kmean_cluster)
    
    dat_plot <- dat_plot %>%
      dplyr::left_join(keep_levels %>% dplyr::mutate(keep = TRUE),
                       by = c("dataset_gear", "kmean_cluster")) %>%
      dplyr::filter(kmean_cluster == "removed" | keep) %>%
      dplyr::select(-keep)
  }
  
  # ---- TRUE ordering per dataset_gear using a facet-specific key ----
  ord_tbl <- dat_plot %>%
    dplyr::group_by(dataset_gear, kmean_cluster) %>%
    dplyr::summarise(med = stats::median(total_abundance, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dataset_gear) %>%
    dplyr::arrange(dplyr::desc(kmean_cluster == "removed"), med, .by_group = TRUE) %>%  # removed first, then median
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"))
  
  # levels ordered within each dataset_gear (and concatenated across dataset_gears)
  key_levels <- ord_tbl %>% dplyr::pull(cluster_key)
  
  dat_plot <- dat_plot %>%
    dplyr::mutate(
      cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"),
      cluster_key = factor(cluster_key, levels = key_levels)
    )
  
  dat_other <- dat_plot %>% dplyr::filter(kmean_cluster != "removed") %>% droplevels()
  dat_rem   <- dat_plot %>% dplyr::filter(kmean_cluster == "removed") %>% droplevels()
  
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = dat_other,
      ggplot2::aes(x = cluster_key, y = total_abundance),
      width = 0.55, outlier.alpha = 0.12, linewidth = 0.25
    ) +
    ggplot2::geom_boxplot(
      data = dat_rem,
      ggplot2::aes(x = cluster_key, y = total_abundance),
      width = 0.60, fill = NA, colour = "black", linewidth = 0.7,
      outlier.alpha = 0.25
    ) +
    ggplot2::facet_wrap(~dataset_gear, ncol = 3, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("thr = ", thr_value, " | clusters ordered by median"),
      x = "kmean_cluster",
      y = "total_abundance"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )
}

# loop over all thr values and save plots for both group1 and group2

outdir <- "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/boxplots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

for (th in thr_value) {
  
  # ---- Group 1 (coarser k) ----
  p_box1 <- make_cluster_boxplot_highlight_removed(
    cluster_group1, dataset.merged, th,
    title = "total_abundance boxplots — Coarser K optimum"
  )
  
  out_file1 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_total_abundance_group1_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file1, plot = p_box1, width = 16, height = 9, dpi = 300)
  
  
  # ---- Group 2 (finer k) ----
  p_box2 <- make_cluster_boxplot_highlight_removed(
    cluster_group2, dataset.merged, th,
    title = "total_abundance boxplots — Finer K optimum"
  )
  
  out_file2 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_total_abundance_group2_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file2, plot = p_box2, width = 16, height = 9, dpi = 300)
}

#####relM_biomass####
cluster_pairwise_relM_biomass <- function(cluster_group, dataset, thr_value, min_n = 2) {
  
  cluster_group <- cluster_group %>%
    filter(thr == thr_value) %>%
    select(sample_ID, kmean_cluster, thr)
  
  dataset.merged.clusters <- dataset %>%
    left_join(cluster_group, by = "sample_ID") %>%
    mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster)),
      kmean_cluster = factor(kmean_cluster)
    ) %>%
    filter(is.finite(relM_biomass), !is.na(relM_biomass))
  
  # (optional) drop tiny cluster levels to prevent "not enough obs"
  if (!is.null(min_n) && min_n > 1) {
    dataset.merged.clusters <- dataset.merged.clusters %>%
      group_by(dataset_gear, kmean_cluster) %>%
      filter(dplyr::n() >= min_n) %>%
      ungroup()
  }
  
  # run per dataset_gear using base pairwise.wilcox.test (robust), then tidy
  res <- lapply(split(dataset.merged.clusters, dataset.merged.clusters$dataset_gear), function(df) {
    
    # need at least two groups present
    if (n_distinct(df$kmean_cluster) < 2) return(NULL)
    if (!("removed" %in% levels(df$kmean_cluster))) return(NULL)
    
    # run pairwise test (BH within this dataset_gear)
    pw <- tryCatch(
      pairwise.wilcox.test(df$relM_biomass, df$kmean_cluster, p.adjust.method = "BH"),
      error = function(e) NULL
    )
    if (is.null(pw)) return(NULL)
    
    # pw$p.value is a lower-tri matrix; convert to long
    pmat <- pw$p.value
    if (is.null(pmat) || length(pmat) == 0) return(NULL)
    
    out <- as.data.frame(as.table(pmat), stringsAsFactors = FALSE) %>%
      rename(group1 = Var1, group2 = Var2, p.adj = Freq) %>%
      filter(!is.na(p.adj)) %>%
      # keep only comparisons involving "removed"
      filter(group1 == "removed" | group2 == "removed") %>%
      mutate(
        cluster = ifelse(group1 == "removed", group2, group1)
      )
    
    out
  })
  
  out <- bind_rows(res, .id = "dataset_gear") %>%
    mutate(thr_value = thr_value)
  
  out
}



relM_biomass_pairwise_group1<-list()
for (i in 1:length(thr_value)) {
  relM_biomass_pairwise_group1[[i]]<-cluster_pairwise_relM_biomass(cluster_group1,dataset.merged,thr_value[i]) 
}

relM_biomass_pairwise_group1_df<-rbindlist(relM_biomass_pairwise_group1)
relM_biomass_pairwise_group1_df<-filter(relM_biomass_pairwise_group1_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group1)
  )

relM_biomass_pairwise_group1_df <- relM_biomass_pairwise_group1_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster"))



relM_biomass_group1_ns_summary <- relM_biomass_pairwise_group1_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

relM_biomass_group1_ns_summary_plot<-ggplot(relM_biomass_group1_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(relM_biomass_group1_ns_summary$thr_value))) +
  labs(title="relM_biomass coarser k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
relM_biomass_group1_ns_summary_plot

ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_relM_biomass_group1_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = relM_biomass_group1_ns_summary_plot, width = 16, height = 9, dpi = 300)

relM_biomass_pairwise_group2<-list()
for (i in 1:length(thr_value)) {
  relM_biomass_pairwise_group2[[i]]<-cluster_pairwise_relM_biomass(cluster_group2,dataset.merged,thr_value[i]) 
}

relM_biomass_pairwise_group2_df<-rbindlist(relM_biomass_pairwise_group2)
relM_biomass_pairwise_group2_df<-filter(relM_biomass_pairwise_group2_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group2)
  )

relM_biomass_pairwise_group2_df <- relM_biomass_pairwise_group2_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster"))


relM_biomass_group2_ns_summary <- relM_biomass_pairwise_group2_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

relM_biomass_group2_ns_summary_plot<-ggplot(relM_biomass_group2_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(relM_biomass_group2_ns_summary$thr_value))) +
  labs(title="relM_biomass finer k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
relM_biomass_group2_ns_summary_plot
ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_relM_biomass_group2_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = relM_biomass_group2_ns_summary_plot, width = 16, height = 9, dpi = 300)


make_cluster_boxplot_highlight_removed <- function(cluster_group, dataset, thr_value,
                                                   title = "",
                                                   top_n_clusters = NULL,
                                                   drop_rare = TRUE,
                                                   min_n = 2) {
  
  cg <- cluster_group %>%
    dplyr::filter(thr == thr_value) %>%
    dplyr::select(sample_ID, kmean_cluster, thr)
  
  dat_plot <- dataset %>%
    dplyr::left_join(cg, by = "sample_ID") %>%
    dplyr::mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster))
    ) %>%
    dplyr::filter(is.finite(relM_biomass), !is.na(relM_biomass))
  
  if (drop_rare) {
    dat_plot <- dat_plot %>%
      dplyr::group_by(dataset_gear, kmean_cluster) %>%
      dplyr::filter(dplyr::n() >= min_n) %>%
      dplyr::ungroup()
  }
  
  if (!is.null(top_n_clusters)) {
    keep_levels <- dat_plot %>%
      dplyr::filter(kmean_cluster != "removed") %>%
      dplyr::count(dataset_gear, kmean_cluster, name = "n") %>%
      dplyr::group_by(dataset_gear) %>%
      dplyr::slice_max(order_by = n, n = top_n_clusters, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset_gear, kmean_cluster)
    
    dat_plot <- dat_plot %>%
      dplyr::left_join(keep_levels %>% dplyr::mutate(keep = TRUE),
                       by = c("dataset_gear", "kmean_cluster")) %>%
      dplyr::filter(kmean_cluster == "removed" | keep) %>%
      dplyr::select(-keep)
  }
  
  # ---- TRUE ordering per dataset_gear using a facet-specific key ----
  ord_tbl <- dat_plot %>%
    dplyr::group_by(dataset_gear, kmean_cluster) %>%
    dplyr::summarise(med = stats::median(relM_biomass, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dataset_gear) %>%
    dplyr::arrange(dplyr::desc(kmean_cluster == "removed"), med, .by_group = TRUE) %>%  # removed first, then median
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"))
  
  # levels ordered within each dataset_gear (and concatenated across dataset_gears)
  key_levels <- ord_tbl %>% dplyr::pull(cluster_key)
  
  dat_plot <- dat_plot %>%
    dplyr::mutate(
      cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"),
      cluster_key = factor(cluster_key, levels = key_levels)
    )
  
  dat_other <- dat_plot %>% dplyr::filter(kmean_cluster != "removed") %>% droplevels()
  dat_rem   <- dat_plot %>% dplyr::filter(kmean_cluster == "removed") %>% droplevels()
  
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = dat_other,
      ggplot2::aes(x = cluster_key, y = relM_biomass),
      width = 0.55, outlier.alpha = 0.12, linewidth = 0.25
    ) +
    ggplot2::geom_boxplot(
      data = dat_rem,
      ggplot2::aes(x = cluster_key, y = relM_biomass),
      width = 0.60, fill = NA, colour = "black", linewidth = 0.7,
      outlier.alpha = 0.25
    ) +
    ggplot2::facet_wrap(~dataset_gear, ncol = 3, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("thr = ", thr_value, " | clusters ordered by median"),
      x = "kmean_cluster",
      y = "relM_biomass"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )
}

# loop over all thr values and save plots for both group1 and group2

outdir <- "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/boxplots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

for (th in thr_value) {
  
  # ---- Group 1 (coarser k) ----
  p_box1 <- make_cluster_boxplot_highlight_removed(
    cluster_group1, dataset.merged, th,
    title = "relM_biomass boxplots — Coarser K optimum"
  )
  
  out_file1 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_relM_biomass_group1_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file1, plot = p_box1, width = 16, height = 9, dpi = 300)
  
  
  # ---- Group 2 (finer k) ----
  p_box2 <- make_cluster_boxplot_highlight_removed(
    cluster_group2, dataset.merged, th,
    title = "relM_biomass boxplots — Finer K optimum"
  )
  
  out_file2 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_relM_biomass_group2_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file2, plot = p_box2, width = 16, height = 9, dpi = 300)
}


#####relM_abundance####
cluster_pairwise_relM_abundance <- function(cluster_group, dataset, thr_value, min_n = 2) {
  
  cluster_group <- cluster_group %>%
    filter(thr == thr_value) %>%
    select(sample_ID, kmean_cluster, thr)
  
  dataset.merged.clusters <- dataset %>%
    left_join(cluster_group, by = "sample_ID") %>%
    mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster)),
      kmean_cluster = factor(kmean_cluster)
    ) %>%
    filter(is.finite(relM_abundance), !is.na(relM_abundance))
  
  # (optional) drop tiny cluster levels to prevent "not enough obs"
  if (!is.null(min_n) && min_n > 1) {
    dataset.merged.clusters <- dataset.merged.clusters %>%
      group_by(dataset_gear, kmean_cluster) %>%
      filter(dplyr::n() >= min_n) %>%
      ungroup()
  }
  
  # run per dataset_gear using base pairwise.wilcox.test (robust), then tidy
  res <- lapply(split(dataset.merged.clusters, dataset.merged.clusters$dataset_gear), function(df) {
    
    # need at least two groups present
    if (n_distinct(df$kmean_cluster) < 2) return(NULL)
    if (!("removed" %in% levels(df$kmean_cluster))) return(NULL)
    
    # run pairwise test (BH within this dataset_gear)
    pw <- tryCatch(
      pairwise.wilcox.test(df$relM_abundance, df$kmean_cluster, p.adjust.method = "BH"),
      error = function(e) NULL
    )
    if (is.null(pw)) return(NULL)
    
    # pw$p.value is a lower-tri matrix; convert to long
    pmat <- pw$p.value
    if (is.null(pmat) || length(pmat) == 0) return(NULL)
    
    out <- as.data.frame(as.table(pmat), stringsAsFactors = FALSE) %>%
      rename(group1 = Var1, group2 = Var2, p.adj = Freq) %>%
      filter(!is.na(p.adj)) %>%
      # keep only comparisons involving "removed"
      filter(group1 == "removed" | group2 == "removed") %>%
      mutate(
        cluster = ifelse(group1 == "removed", group2, group1)
      )
    
    out
  })
  
  out <- bind_rows(res, .id = "dataset_gear") %>%
    mutate(thr_value = thr_value)
  
  out
}



relM_abundance_pairwise_group1<-list()
for (i in 1:length(thr_value)) {
  relM_abundance_pairwise_group1[[i]]<-cluster_pairwise_relM_abundance(cluster_group1,dataset.merged,thr_value[i]) 
}

relM_abundance_pairwise_group1_df<-rbindlist(relM_abundance_pairwise_group1)
relM_abundance_pairwise_group1_df<-filter(relM_abundance_pairwise_group1_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group1)
  )

relM_abundance_pairwise_group1_df <- relM_abundance_pairwise_group1_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster"))



relM_abundance_group1_ns_summary <- relM_abundance_pairwise_group1_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

relM_abundance_group1_ns_summary_plot<-ggplot(relM_abundance_group1_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(relM_abundance_group1_ns_summary$thr_value))) +
  labs(title="relM_abundance coarser k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
relM_abundance_group1_ns_summary_plot

ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_relM_abundance_group1_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = relM_abundance_group1_ns_summary_plot, width = 16, height = 9, dpi = 300)

relM_abundance_pairwise_group2<-list()
for (i in 1:length(thr_value)) {
  relM_abundance_pairwise_group2[[i]]<-cluster_pairwise_relM_abundance(cluster_group2,dataset.merged,thr_value[i]) 
}

relM_abundance_pairwise_group2_df<-rbindlist(relM_abundance_pairwise_group2)
relM_abundance_pairwise_group2_df<-filter(relM_abundance_pairwise_group2_df,group1 == "removed" | group2 == "removed")

#From the starting dataset extract observation belonging to the 10years-clusters
keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster= as.integer(kmean_cluster_group2)
  )

relM_abundance_pairwise_group2_df <- relM_abundance_pairwise_group2_df %>%
  mutate(
    thr = as.numeric(thr_value),
    kmean_cluster = as.integer(cluster)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster"))


relM_abundance_group2_ns_summary <- relM_abundance_pairwise_group2_df %>%
  group_by(dataset_gear, thr_value) %>%
  summarise(
    n_tests = dplyr::n(),
    n_ns    = sum(p.adj>=0.05, na.rm = TRUE),
    perc_ns = 100 * n_ns / n_tests,
    .groups = "drop"
  )

relM_abundance_group2_ns_summary_plot<-ggplot(relM_abundance_group2_ns_summary, aes(x = thr_value, y = perc_ns)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(relM_abundance_group2_ns_summary$thr_value))) +
  labs(title="relM_abundance finer k",x = "thr_value", y = "% non-significant comparisons (BH-adjusted)") +
  ylim(0,100)+
  facet_wrap(~dataset_gear) +
  theme_bw()
relM_abundance_group2_ns_summary_plot
ggsave(filename = "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_relM_abundance_group2_dataset_kmean_clusters_pairwise_comparison_significance.png", plot = relM_abundance_group2_ns_summary_plot, width = 16, height = 9, dpi = 300)


make_cluster_boxplot_highlight_removed <- function(cluster_group, dataset, thr_value,
                                                   title = "",
                                                   top_n_clusters = NULL,
                                                   drop_rare = TRUE,
                                                   min_n = 2) {
  
  cg <- cluster_group %>%
    dplyr::filter(thr == thr_value) %>%
    dplyr::select(sample_ID, kmean_cluster, thr)
  
  dat_plot <- dataset %>%
    dplyr::left_join(cg, by = "sample_ID") %>%
    dplyr::mutate(
      kmean_cluster = ifelse(is.na(kmean_cluster), "removed", as.character(kmean_cluster))
    ) %>%
    dplyr::filter(is.finite(relM_abundance), !is.na(relM_abundance))
  
  if (drop_rare) {
    dat_plot <- dat_plot %>%
      dplyr::group_by(dataset_gear, kmean_cluster) %>%
      dplyr::filter(dplyr::n() >= min_n) %>%
      dplyr::ungroup()
  }
  
  if (!is.null(top_n_clusters)) {
    keep_levels <- dat_plot %>%
      dplyr::filter(kmean_cluster != "removed") %>%
      dplyr::count(dataset_gear, kmean_cluster, name = "n") %>%
      dplyr::group_by(dataset_gear) %>%
      dplyr::slice_max(order_by = n, n = top_n_clusters, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset_gear, kmean_cluster)
    
    dat_plot <- dat_plot %>%
      dplyr::left_join(keep_levels %>% dplyr::mutate(keep = TRUE),
                       by = c("dataset_gear", "kmean_cluster")) %>%
      dplyr::filter(kmean_cluster == "removed" | keep) %>%
      dplyr::select(-keep)
  }
  
  # ---- TRUE ordering per dataset_gear using a facet-specific key ----
  ord_tbl <- dat_plot %>%
    dplyr::group_by(dataset_gear, kmean_cluster) %>%
    dplyr::summarise(med = stats::median(relM_abundance, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dataset_gear) %>%
    dplyr::arrange(dplyr::desc(kmean_cluster == "removed"), med, .by_group = TRUE) %>%  # removed first, then median
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"))
  
  # levels ordered within each dataset_gear (and concatenated across dataset_gears)
  key_levels <- ord_tbl %>% dplyr::pull(cluster_key)
  
  dat_plot <- dat_plot %>%
    dplyr::mutate(
      cluster_key = paste(dataset_gear, kmean_cluster, sep = "__"),
      cluster_key = factor(cluster_key, levels = key_levels)
    )
  
  dat_other <- dat_plot %>% dplyr::filter(kmean_cluster != "removed") %>% droplevels()
  dat_rem   <- dat_plot %>% dplyr::filter(kmean_cluster == "removed") %>% droplevels()
  
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = dat_other,
      ggplot2::aes(x = cluster_key, y = relM_abundance),
      width = 0.55, outlier.alpha = 0.12, linewidth = 0.25
    ) +
    ggplot2::geom_boxplot(
      data = dat_rem,
      ggplot2::aes(x = cluster_key, y = relM_abundance),
      width = 0.60, fill = NA, colour = "black", linewidth = 0.7,
      outlier.alpha = 0.25
    ) +
    ggplot2::facet_wrap(~dataset_gear, ncol = 3, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("thr = ", thr_value, " | clusters ordered by median"),
      x = "kmean_cluster",
      y = "relM_abundance"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )
}

# loop over all thr values and save plots for both group1 and group2

outdir <- "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/boxplots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

for (th in thr_value) {
  
  # ---- Group 1 (coarser k) ----
  p_box1 <- make_cluster_boxplot_highlight_removed(
    cluster_group1, dataset.merged, th,
    title = "relM_abundance boxplots — Coarser K optimum"
  )
  
  out_file1 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_relM_abundance_group1_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file1, plot = p_box1, width = 16, height = 9, dpi = 300)
  
  
  # ---- Group 2 (finer k) ----
  p_box2 <- make_cluster_boxplot_highlight_removed(
    cluster_group2, dataset.merged, th,
    title = "relM_abundance boxplots — Finer K optimum"
  )
  
  out_file2 <- file.path(
    outdir,
    paste0("WKBENTH4_Type3_relM_abundance_group2_dataset_kmean_clusters_boxplot_thr_", th, ".png")
  )
  
  ggsave(filename = out_file2, plot = p_box2, width = 16, height = 9, dpi = 300)
}


make_sig_keys <- function(pairwise_df, indicator_name, group_name, alpha = 0.05){
  pairwise_df %>%
    filter(!is.na(p.adj), p.adj < alpha) %>%
    transmute(group = group_name,
              dataset_gear,
              thr = thr_value,
              indicator = indicator_name,
              kmean_cluster = as.integer(cluster)) %>%
    distinct()
}

alpha <- 0.05

sig_keys <- bind_rows(
  make_sig_keys(SoS_pairwise_group1_df,          "SoS",            "group1", alpha),
  make_sig_keys(SoS_pairwise_group2_df,          "SoS",            "group2", alpha),
  make_sig_keys(richness_pairwise_group1_df,     "richness",       "group1", alpha),
  make_sig_keys(richness_pairwise_group2_df,     "richness",       "group2", alpha),
  make_sig_keys(total_biomass_pairwise_group1_df,"total_biomass",  "group1", alpha),
  make_sig_keys(total_biomass_pairwise_group2_df,"total_biomass",  "group2", alpha),
  make_sig_keys(total_abundance_pairwise_group1_df,"total_abundance","group1", alpha),
  make_sig_keys(total_abundance_pairwise_group2_df,"total_abundance","group2", alpha),
  make_sig_keys(relM_biomass_pairwise_group1_df, "relM_biomass",   "group1", alpha),
  make_sig_keys(relM_biomass_pairwise_group2_df, "relM_biomass",   "group2", alpha),
  make_sig_keys(relM_abundance_pairwise_group1_df,"relM_abundance","group1", alpha),
  make_sig_keys(relM_abundance_pairwise_group2_df,"relM_abundance","group2", alpha)
) %>%
  distinct()


cluster_group1_sig <- cluster_group1 %>%
  mutate(thr = as.numeric(thr), kmean_cluster = as.integer(kmean_cluster)) %>%
  semi_join(
    sig_keys %>% filter(group == "group1") %>% select(dataset_gear, thr, kmean_cluster),
    by = c("dataset_gear","thr","kmean_cluster")
  )

cluster_group2_sig <- cluster_group2 %>%
  mutate(thr = as.numeric(thr), kmean_cluster = as.integer(kmean_cluster)) %>%
  semi_join(
    sig_keys %>% filter(group == "group2") %>% select(dataset_gear, thr, kmean_cluster),
    by = c("dataset_gear","thr","kmean_cluster")
  )

fwrite(cluster_group1_sig,"./WKBENTH4/results/WKBENTH4_Type3_pressuretype_fixedpthr_significant_kmeans_clusters_group1.csv")
fwrite(cluster_group2_sig,"./WKBENTH4/results/WKBENTH4_Type3_pressuretype_fixedpthr_significant_kmeans_clusters_group2.csv")


#########################################################################################
#   SUMMARY TABLE: dataset_gear x thr x indicator
#   - number of clusters tested vs "removed"
#   - % non-significant comparisons (BH-adjusted p.adj >= alpha)
#########################################################################################

alpha <- 0.05

# helper: build summary from a pairwise df (already filtered to include only the selected clusters)
summarise_removed_vs_clusters <- function(pairwise_df, indicator_name, group_name, alpha = 0.05) {
  
  if (is.null(pairwise_df) || nrow(pairwise_df) == 0) {
    return(tibble(
      group = character(),
      dataset_gear = character(),
      thr_value = numeric(),
      indicator = character(),
      n_clusters_tested = integer(),
      n_ns = integer(),
      perc_ns = numeric()
    ))
  }
  
  pairwise_df %>%
    filter(!is.na(p.adj)) %>%
    # one row per tested cluster vs removed
    mutate(cluster = as.character(cluster)) %>%
    group_by(dataset_gear, thr_value) %>%
    summarise(
      n_clusters_tested = n_distinct(cluster),
      n_ns = sum(p.adj >= alpha, na.rm = TRUE),
      perc_ns = 100 * n_ns / n_clusters_tested,
      .groups = "drop"
    ) %>%
    mutate(
      group = group_name,
      indicator = indicator_name
    ) %>%
    select(group, dataset_gear, thr_value, indicator, n_clusters_tested, n_ns, perc_ns)
}

# build the full table (both groups, all indicators)
removed_eval_summary <- bind_rows(
  summarise_removed_vs_clusters(SoS_pairwise_group1_df,             "SoS",            "group1", alpha),
  summarise_removed_vs_clusters(SoS_pairwise_group2_df,             "SoS",            "group2", alpha),
  
  summarise_removed_vs_clusters(richness_pairwise_group1_df,        "richness",       "group1", alpha),
  summarise_removed_vs_clusters(richness_pairwise_group2_df,        "richness",       "group2", alpha),
  
  summarise_removed_vs_clusters(total_biomass_pairwise_group1_df,   "total_biomass",  "group1", alpha),
  summarise_removed_vs_clusters(total_biomass_pairwise_group2_df,   "total_biomass",  "group2", alpha),
  
  summarise_removed_vs_clusters(total_abundance_pairwise_group1_df, "total_abundance","group1", alpha),
  summarise_removed_vs_clusters(total_abundance_pairwise_group2_df, "total_abundance","group2", alpha),
  
  summarise_removed_vs_clusters(relM_biomass_pairwise_group1_df,    "relM_biomass",   "group1", alpha),
  summarise_removed_vs_clusters(relM_biomass_pairwise_group2_df,    "relM_biomass",   "group2", alpha),
  
  summarise_removed_vs_clusters(relM_abundance_pairwise_group1_df,  "relM_abundance", "group1", alpha),
  summarise_removed_vs_clusters(relM_abundance_pairwise_group2_df,  "relM_abundance", "group2", alpha)
) %>%
  arrange(group, indicator, dataset_gear, thr_value)

# quick check
print(removed_eval_summary)

# save
fwrite(
  removed_eval_summary,
  "./WKBENTH4/results/phase4_cluster_evaluation/pairwise_comparison_significance/WKBENTH4_Type3_removed_vs_clusters_NS_summary.csv"
)