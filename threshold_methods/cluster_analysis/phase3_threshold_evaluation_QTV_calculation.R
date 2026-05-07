#########################################################################################
#
#                         Phase 3: Quality threshold evaluation
#                                 Calculation of QTV
#
##########################################################################################
#ICES WKBENTH4
#Script Author: Gabriele Di Bona
#February 2026


library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(ggplot2)
library(viridis)
library(lme4)
library(broom.mixed)
library(patchwork)
library(ggh4x)

rm(list = ls())
options(scipen = 999)
set.seed(666)

#Load k-means clustering results
dataset.merged.thr.group1<-as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_pressuretype_fixedpthr_kmeans_clusters_group1.csv"))
dataset.merged.thr.group2<-as.data.frame(fread("./WKBENTH4/results/WKBENTH4_Type3_pressuretype_fixedpthr_kmeans_clusters_group2.csv"))
data.table::setnames(dataset.merged.thr.group1, "kmean_cluster", "kmean_cluster_group1")
data.table::setnames(dataset.merged.thr.group2, "kmean_cluster", "kmean_cluster_group2")


#merge the dataframes with relevant columns
key <- c("station","year","longitude","latitude","month","depth","gear","replicates","habitat_type",
         "pressure_type","pressure_value","total_biomass","total_abundance","richness","relM_biomass","relM_abundance",      
         "sample_ID","dataset","SoS","ICES_ecoregion","emod_depth","dataset_gear","thr" )

dataset.merged.thr <- full_join(
  dataset.merged.thr.group1,
  dataset.merged.thr.group2,
  by = key
)

#remove unrealistic values for certain indicators
dataset.merged.thr$relM_abundance<-ifelse(dataset.merged.thr$relM_abundance<=0,0.01,dataset.merged.thr$relM_abundance)
dataset.merged.thr$relM_biomass<-ifelse(dataset.merged.thr$relM_biomass<=0,0.01,dataset.merged.thr$relM_biomass)

#create spatial point of the sampling observations
pts <- st_as_sf(dataset.merged.thr, coords = c("longitude", "latitude"),
                crs = 4326, remove = FALSE)
#project them in a SR that uses meters
pts3035 <- st_transform(pts, 3035)
xy <- st_coordinates(pts3035)

dataset.merged.thr$x_3035 <- xy[, "X"]
dataset.merged.thr$y_3035 <- xy[, "Y"]


#Calculate average euclidean distance, standard deviations of point observation that fall inside the same cluster
#count also number of point and number of years 
pairwise_summary <- function(df, cluster_col) {
  df %>%
    group_by(dataset_gear, thr, !!rlang::sym(cluster_col)) %>%
    summarise(
      n_points = dplyr::n(),
      n_years  = n_distinct(year),
      mean_pair_dist_km = if (dplyr::n() < 2) NA_real_ else {
        d <- dist(cbind(x_3035, y_3035))
        mean(d) / 1000
      },
      sd_pair_dist_km = if (dplyr::n() < 2) NA_real_ else {
        d <- dist(cbind(x_3035, y_3035))
        sd(d) / 1000
      },
      # centroid coordinates (EPSG:3035)
      centroid_x_3035 = mean(x_3035, na.rm = TRUE),
      centroid_y_3035 = mean(y_3035, na.rm = TRUE),
      # alternative compactness metric: mean distance to centroid
      mean_centroid_dist_km = if (dplyr::n() < 2) NA_real_ else {
        cx <- mean(x_3035); cy <- mean(y_3035)
        mean(sqrt((x_3035 - cx)^2 + (y_3035 - cy)^2)) / 1000
      },
      .groups = "drop"
    )
}

pairwise_mean_spat_group1 <- pairwise_summary(dataset.merged.thr, "kmean_cluster_group1")
pairwise_mean_spat_group2 <- pairwise_summary(dataset.merged.thr, "kmean_cluster_group2")


#Extract only clusters with at least 10 years of observations
pairwise_mean_spat_success_group1<-filter(pairwise_mean_spat_group1,n_years>=10)
pairwise_mean_spat_success_group1<-na.omit(pairwise_mean_spat_success_group1)

pairwise_mean_spat_success_group2<-filter(pairwise_mean_spat_group2,n_years>=10)
pairwise_mean_spat_success_group2<-na.omit(pairwise_mean_spat_success_group2)

fwrite(pairwise_mean_spat_success_group1,"./WKBENTH4/results/WKBENTH4_Type3_reference_condition_kmean_clusters_group1_spatial_metrics.csv")
fwrite(pairwise_mean_spat_success_group2,"./WKBENTH4/results/WKBENTH4_Type3_reference_condition_kmean_clusters_group2_spatial_metrics.csv")


#From the starting dataset extract observation belonging to the 10years-clusters
keys_group1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster_group1 = as.integer(kmean_cluster_group1)
  )

dataset_subset_group1 <- dataset.merged.thr %>%
  mutate(
    thr = as.numeric(thr),
    kmean_cluster_group1 = as.integer(kmean_cluster_group1)
  ) %>%
  semi_join(keys_group1, by = c("dataset_gear", "thr", "kmean_cluster_group1"))


keys_group2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster_group2 = as.integer(kmean_cluster_group2)
  ) %>%
  distinct()

dataset_subset_group2 <- dataset.merged.thr %>%
  mutate(
    thr = as.numeric(thr),
    kmean_cluster_group2 = as.integer(kmean_cluster_group2)
  ) %>%
  semi_join(keys_group2, by = c("dataset_gear", "thr", "kmean_cluster_group2"))


#Plot index distribution vs pressure values across fixed threshold,dataset_gear
# --- variables to plot ---
resp_vars <- c("total_biomass", "total_abundance", "richness",
               "relM_biomass", "relM_abundance", "SoS")

# --- long format (one row per indicator value) ---
df_long_group1 <- dataset_subset_group1 %>%
  dplyr::select(dataset_gear, thr, pressure_value, all_of(resp_vars)) %>%
  pivot_longer(cols = all_of(resp_vars),
               names_to = "indicator",
               values_to = "value")

df_long_group2 <- dataset_subset_group2 %>%
  dplyr::select(dataset_gear, thr, pressure_value, all_of(resp_vars)) %>%
  pivot_longer(cols = all_of(resp_vars),
               names_to = "indicator",
               values_to = "value")

# --- helper to draw one plot per indicator ---
plot_one_indicator <- function(ind_name, df) {
  
  d <- df %>% filter(indicator == ind_name)
  
  ggplot(d, aes(x = pressure_value, y = value)) +
    geom_point(alpha = 0.35, size = 1) +
    geom_smooth(method = "gam", se = TRUE, linewidth = 0.8) +
    facet_grid(dataset_gear ~ thr, scales = "free") +
    labs(
      x = "SAR",
      y = ind_name,
      title = paste("SAR vs", ind_name),
      subtitle = "Panels: thr × dataset_gear; line: gam(indicator ~ SAR)"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# --- create plots (a named list) ---
plots_group1 <- setNames(lapply(resp_vars, plot_one_indicator,df_long_group1), resp_vars)
plots_group2 <- setNames(lapply(resp_vars, plot_one_indicator,df_long_group2), resp_vars)

#Print to show results
plots_group1$total_biomass
plots_group2$total_biomass

plots_group1$total_abundance
plots_group2$total_abundance

plots_group1$richness
plots_group2$richness

plots_group1$relM_biomass
plots_group2$relM_biomass

plots_group1$relM_abundance
plots_group2$relM_abundance

plots_group1$SoS
plots_group2$SoS

# Save all
for (nm in names(plots_group1)) {
  ggsave(
    filename = file.path("./WKBENTH4/results/phase2_clusterization/kmeans_cluster_pressure_vs_indicator/", paste0("WKBENTH4_kmeans_cluster_group1_pressure_vs_", nm, "_scatterplot.png")),
    plot = plots_group1[[nm]],
    width = 14, height = 8, dpi = 300
  ) }

for (nm in names(plots_group2)) {
  ggsave(
    filename = file.path("./WKBENTH4/results/phase2_clusterization/kmeans_cluster_pressure_vs_indicator/", paste0("WKBENTH4_kmeans_cluster_group2_pressure_vs_", nm, "_scatterplot.png")),
    plot = plots_group2[[nm]],
    width = 14, height = 8, dpi = 300
  ) }



#Create a long dataframe format to use it for linear regression
#grouping: dataset and gear type, fixed threshold selected, kmeans cluster
#year as fixed effect (temporal detrending)
#add weight based on number of observation per year

#Function for data transforming - logit
logit_boundary <- function(x) {
  ok <- is.finite(x) & !is.na(x)
  n  <- sum(ok)
  xs <- x
  xs[ok] <- (x[ok] * (n - 1) + 0.5) / n
  qlogis(xs)
}

#Function to add weights based on number of observations per year
add_year_weights <- function(df, group_cols) {
  df %>%
    group_by(across(all_of(c(group_cols, "year")))) %>%
    mutate(n_year = dplyr::n()) %>%
    ungroup() %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(w = 1 / n_year) %>%
    ungroup()
}

#Function for weighted linear regression
fit_one <- function(dat) {
  if (nrow(dat) < 5) return(NULL)
  if (length(unique(dat$year)) < 2) return(NULL)
  
  tryCatch(
    lm(value_t ~ year, data = dat, weights = w),
    error = function(e) NULL
  )
}


#Function to recreate the time-series after detrending by weighted linear regression
make_year_series <- function(mod, dat) {
  
  dat <- dat %>%
    mutate(
      resid = resid(mod),
      mu    = weighted.mean(value_t, w = w, na.rm = TRUE),
      resid_centered = resid + mu
    )
  
  ts_detr <- dat %>%
    group_by(year) %>%
    summarise(
      detr = weighted.mean(resid,w = w, na.rm = TRUE),                 
      detr_centered = weighted.mean(resid_centered,w=w, na.rm = TRUE),
      n  = dplyr::n(),
      sd = sd(resid, na.rm = TRUE),
      se = sd / sqrt(n),
      .groups = "drop"
    ) %>%
    arrange(year)
  
  ts_detr
}

####Group1####

#Create long dataframe for analysis
df_long_analysis_group1 <- dataset_subset_group1 %>%
  dplyr::select(dataset_gear, thr, kmean_cluster_group1, year, all_of(resp_vars)) %>%
  pivot_longer(all_of(resp_vars), names_to = "indicator", values_to = "value") %>%
  filter(!is.na(dataset_gear),
         !is.na(thr),
         !is.na(kmean_cluster_group1),
         !is.na(year),
         !is.na(value))%>%
  group_by(dataset_gear, thr, kmean_cluster_group1, indicator) %>% 
  mutate(
    value_t = case_when(
      indicator %in% c("total_biomass", "total_abundance", "richness") ~ log1p(value),
      indicator == "SoS" ~ logit_boundary(value),
      indicator %in% c("relM_abundance","relM_biomass") ~ log(value),
      TRUE ~ value
    ),
    fyear = factor(year)
  ) %>%
  ungroup() %>%
  filter(is.finite(value_t))

#add weights
gcols1 <- c("dataset_gear","thr","kmean_cluster_group1","indicator")
df_long_analysis_group1 <- add_year_weights(df_long_analysis_group1, gcols1)




#fit weighted linear model by grouping and recreate time-series
fits_group1 <- df_long_analysis_group1 %>%
  group_by(dataset_gear, thr, kmean_cluster_group1, indicator) %>%
  nest() %>%
  mutate(
    model = map(data, fit_one),
    ok    = map_lgl(model, ~ !is.null(.x)),
    year_index = map2(model, data, ~ if (is.null(.x)) NULL else make_year_series(.x, .y))
  )


#extrapolate
ts_pred_year_group1 <- fits_group1 %>%
  filter(ok) %>%
  dplyr::select(dataset_gear, thr, kmean_cluster_group1, indicator, year_index) %>%
  unnest(year_index)


####Group2####
df_long_analysis_group2 <- dataset_subset_group2 %>%
  dplyr::select(dataset_gear, thr, kmean_cluster_group2, year, all_of(resp_vars)) %>%
  pivot_longer(all_of(resp_vars), names_to = "indicator", values_to = "value") %>%
  filter(!is.na(dataset_gear),
         !is.na(thr),
         !is.na(kmean_cluster_group2),
         !is.na(year),
         !is.na(value))%>%
  group_by(dataset_gear, thr, kmean_cluster_group2, indicator) %>% 
  mutate(
    value_t = case_when(
      indicator %in% c("total_biomass", "total_abundance", "richness") ~ log1p(value),
      indicator == "SoS" ~ logit_boundary(value),
      indicator %in% c("relM_biomass","relM_abundance") ~ log(value),
      TRUE ~ value
    ),
    fyear = factor(year)
  ) %>%
  ungroup() %>%
  filter(is.finite(value_t))

gcols1 <- c("dataset_gear","thr","kmean_cluster_group2","indicator")
df_long_analysis_group2 <- add_year_weights(df_long_analysis_group2, gcols1)




fits_group2 <- df_long_analysis_group2 %>%
  group_by(dataset_gear, thr, kmean_cluster_group2, indicator) %>%
  nest() %>%
  mutate(
    model = map(data, fit_one),
    ok    = map_lgl(model, ~ !is.null(.x)),
    year_index = map2(model, data, ~ if (is.null(.x)) NULL else make_year_series(.x, .y))
  )



ts_pred_year_group2 <- fits_group2 %>%
  filter(ok) %>%
  dplyr::select(dataset_gear, thr, kmean_cluster_group2, indicator, year_index) %>%
  unnest(year_index)



# Set indicators and their transformation
inds_log1p  <- c("total_biomass", "total_abundance", "richness")#log1p()
inds_log  <- c("relM_biomass", "relM_abundance")#log()
inds_logit <- c("SoS") # logit_boundary()

#Function for inverting transformation based on indicator
back_transform_y <- function(y_t, indicator) {
  dplyr::case_when(
    indicator %in% inds_log1p   ~ expm1(y_t),
    indicator %in% inds_log ~ exp(y_t),
    indicator %in% inds_logit ~ plogis(y_t),
    TRUE                      ~ y_t
  )
}


#Add baseline (mean over the time series) in original scale and backtransformed
ts_pred_year_group1 <- ts_pred_year_group1 %>%
  mutate(
    mu_t    = detr_centered-detr,
    mu_orig = back_transform_y(mu_t, indicator)
  )
fwrite(ts_pred_year_group1,"./WKBENTH4/results/Type3_group1_Indices_time_series_predictions_matrices.csv")


#Evaluate Quality threshold for coarse k clusters (group1)

thresh_summary_group1 <- ts_pred_year_group1 %>%
  group_by(dataset_gear, thr, kmean_cluster_group1, indicator) %>%
  summarise(
    n_years = sum(!is.na(detr)),
    baseline = first(mu_orig),
    baseline_t = first(mu_t),
    mean_detr = mean(detr, na.rm = TRUE),
    sd_detr   = sd(detr, na.rm = TRUE),
    se_detr   = sd_detr / sqrt(n_years),
    
    # Detectable change - 95% CI for the MEAN detrended anomaly (t-interval)
    tval   = qt(0.975, df = pmax(n_years - 1, 1)),
    margin = tval * se_detr,
    ci_low = mean_detr - margin,
    ci_up  = mean_detr + margin,
    dect_change=back_transform_y(baseline_t+ci_low,unique(indicator))/baseline,
    
    #Natural Variation
    nv_025  = back_transform_y(baseline_t+(qnorm(0.025) * sd_detr),unique(indicator))/baseline,
    nv_050  = back_transform_y(baseline_t+(qnorm(0.05) * sd_detr),unique(indicator))/baseline,
    nv_15  = back_transform_y(baseline_t+(qnorm(0.15) * sd_detr),unique(indicator))/baseline,
    nv_pct_025  = 100 * (nv_025  - 1),
    nv_pct_050  = 100 * (nv_050  - 1),
    nv_pct_15  = 100 * (nv_15  - 1),
    .groups = "drop"
)

centroids_g1 <- pairwise_mean_spat_success_group1 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster_group1 = as.integer(kmean_cluster_group1),
    centroid_x_3035,
    centroid_y_3035)
    
thresh_summary_group1 <- thresh_summary_group1 %>%
      mutate(
        thr = as.numeric(thr),
        kmean_cluster_group1 = as.integer(kmean_cluster_group1)
      ) %>%
      left_join(
        centroids_g1,
        by = c("dataset_gear", "thr", "kmean_cluster_group1")
      )   

fwrite(thresh_summary_group1,"./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group1.csv")


summary(thresh_summary_group1)



ts_pred_year_group2 <- ts_pred_year_group2 %>%
  mutate(
    mu_t    = detr_centered-detr,
    mu_orig = back_transform_y(mu_t, indicator)
  )
fwrite(ts_pred_year_group2,"./WKBENTH4/results/Type3_group2_Indices_time_series_predictions_matrices.csv")

thresh_summary_group2 <- ts_pred_year_group2 %>%
  group_by(dataset_gear, thr, kmean_cluster_group2, indicator) %>%
  summarise(
    n_years = sum(!is.na(detr)),
    baseline = first(mu_orig),
    baseline_t = first(mu_t),
    mean_detr = mean(detr, na.rm = TRUE),
    sd_detr   = sd(detr, na.rm = TRUE),
    se_detr   = sd_detr / sqrt(n_years),
    
    # 95% CI for the MEAN detrended anomaly (t-interval)
    tval   = qt(0.975, df = pmax(n_years - 1, 1)),
    margin = tval * se_detr,
    ci_low = mean_detr - margin,
    ci_up  = mean_detr + margin,
    dect_change=back_transform_y(baseline_t+ci_low,unique(indicator))/baseline,
    
    #Natural Variation
    nv_025  = back_transform_y(baseline_t+(qnorm(0.025) * sd_detr),unique(indicator))/baseline,
    nv_050  = back_transform_y(baseline_t+(qnorm(0.05) * sd_detr),unique(indicator))/baseline,
    nv_15  = back_transform_y(baseline_t+(qnorm(0.15) * sd_detr),unique(indicator))/baseline,
    nv_pct_025  = 100 * (nv_025  - 1),
    nv_pct_050  = 100 * (nv_050  - 1),
    nv_pct_15  = 100 * (nv_15  - 1),
    .groups = "drop"
  )


centroids_g2 <- pairwise_mean_spat_success_group2 %>%
  transmute(
    dataset_gear,
    thr = as.numeric(thr),
    kmean_cluster_group2 = as.integer(kmean_cluster_group2),
    centroid_x_3035,
    centroid_y_3035)

thresh_summary_group2 <- thresh_summary_group2 %>%
  mutate(
    thr = as.numeric(thr),
    kmean_cluster_group2 = as.integer(kmean_cluster_group2)
  ) %>%
  left_join(
    centroids_g2,
    by = c("dataset_gear", "thr", "kmean_cluster_group2")
  )   


fwrite(thresh_summary_group2,"./WKBENTH4/results/WKBENTH4_Type3_Indices_reference_condition_thresholds_kmean_cluster_group2.csv")


summary(thresh_summary_group2)



# ============================================================
# Create panel with Quality threshold value
# - Reference condition (y=1): BLUE
# - Detectable change (dect_change): GREEN
# - Natural variation (nv_*): gradient of REDS
# 
# ============================================================

#colors
col_ref  <- "blue"
col_dc   <- "#0d98ba"
col_nv15 <- "#FCA5A5"  
col_nv05 <- "#EF4444" 
col_nv025<- "#7F1D1D" 

# long table of lines for BOTH panels

plot_dat_g1 <- bind_rows(
  df_long_analysis_group1 %>%
    transmute(dataset_gear, thr, kmean_cluster_group1, indicator, year,
              panel = "Orig", y = value),
  
  df_long_analysis_group1 %>%
    transmute(dataset_gear, thr, kmean_cluster_group1, indicator, year,
              panel = "Transf", y = value_t),
  
  ts_pred_year_group1 %>%
    transmute(dataset_gear, thr, kmean_cluster_group1, indicator, year,
              panel = "Detr", y = detr),
  
  ts_pred_year_group1 %>%
    transmute(dataset_gear, thr, kmean_cluster_group1, indicator, year,
              panel = "Ratio", y = 1 + detr)
) %>%
  mutate(
    panel = factor(panel, levels = c("Orig","Transf","Detr","Ratio")),
    kmean_cluster_group1 = factor(kmean_cluster_group1)
  )


thr_lines_long_g1 <- thresh_summary_group1 %>%
  transmute(
    dataset_gear, thr, kmean_cluster_group1, indicator,
    kmean_cluster_group1 = factor(kmean_cluster_group1),
    
    # --- Detr-panel lines (on detr scale) ---
    ref_detr  = 0,
    dc_detr   = ci_low,
    nv15_detr = qnorm(0.15)  * sd_detr,
    nv05_detr = qnorm(0.05)  * sd_detr,
    nv025_detr= qnorm(0.025) * sd_detr,
    
    # --- Ratio-panel lines (on ratio scale) ---
    ref_ratio  = 1,
    dc_ratio   = dect_change,
    nv15_ratio = nv_15,
    nv05_ratio = nv_050,
    nv025_ratio= nv_025
  ) %>%
  # make two panel-specific blocks
  tidyr::pivot_longer(
    cols = c(ref_detr, dc_detr, nv15_detr, nv05_detr, nv025_detr,
             ref_ratio, dc_ratio, nv15_ratio, nv05_ratio, nv025_ratio),
    names_to = "name",
    values_to = "yline"
  ) %>%
  mutate(
    panel = case_when(
      grepl("_detr$",  name) ~ "Detr",
      grepl("_ratio$", name) ~ "Ratio",
      TRUE ~ NA_character_
    ),
    thr_type = case_when(
      grepl("^ref_",  name) ~ "Reference",
      grepl("^dc_",   name) ~ "Detectable change",
      grepl("^nv15_", name) ~ "NV 0.15",
      grepl("^nv05_", name) ~ "NV 0.05",
      grepl("^nv025_",name) ~ "NV 0.025",
      TRUE ~ NA_character_
    ),
    panel = factor(panel, levels = c("Orig","Transf","Detr","Ratio")),
    thr_type = factor(thr_type, levels = c("Reference","Detectable change","NV 0.15","NV 0.05","NV 0.025"))
  ) %>%
  filter(!is.na(panel), !is.na(thr_type))


plot_one_dg_thr_g1 <- function(dg, thr_val, ind) {
  
  dd <- plot_dat_g1 %>%
    filter(dataset_gear == dg, thr == thr_val, indicator == ind) %>%
    mutate(
      year_f = factor(year, levels = sort(unique(year))),
      kmean_cluster_group1 = factor(kmean_cluster_group1)
    )
  
  if (nrow(dd) == 0) return(NULL)
  
  ll <- thr_lines_long_g1 %>%
    filter(dataset_gear == dg, thr == thr_val, indicator == ind)
  
  ggplot(dd, aes(x = year_f, y = y)) +
    geom_point(size = 1.6, alpha = 0.8) +
    
    # coloured thresholds (only exist for panels Detr + Ratio)
    geom_hline(
      data = ll,
      aes(yintercept = yline, colour = thr_type),
      linewidth = 0.8
    ) +
    
    facet_grid2(
      kmean_cluster_group1 ~ panel,
      scales = "free_y",
      independent = "y"
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_colour_manual(
      name = NULL,
      values = c(
        "Reference"         = col_ref,
        "Detectable change" = col_dc,
        "NV 0.15"           = col_nv15,
        "NV 0.05"           = col_nv05,
        "NV 0.025"          = col_nv025
      )
    ) +
    guides(
      colour = guide_legend(
        direction = "horizontal",
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.2)
      )
    ) +
    labs(
      title = paste0(dg, " | thr = ", thr_val, " | ", ind),
      x = NULL, y = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.background = element_rect(fill = "grey95"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center",
      legend.justification = "center"
    )
}

#Test
plot_one_dg_thr_g1(
  dg = "BoBIC_IberianChabitats_otter_trawl",
  thr_val = 0.1,
  ind = "richness"
)



#total_biomass
cfg_thr_total_biomass <- thr_lines_long_g1[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="total_biomass")

plots_thr_total_biomass <- map2(cfg_thr_total_biomass$dataset_gear, cfg_thr_total_biomass$thr, ~ plot_one_dg_thr_g1(.x, .y, "total_biomass"))
plots_thr_total_biomass <- compact(plots_thr_total_biomass) 
names(plots_thr_total_biomass)<-paste(cfg_thr_total_biomass$dataset_gear,cfg_thr_total_biomass$thr,sep = "_")


purrr::walk2(
  plots_thr_total_biomass,
  names(plots_thr_total_biomass),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group1_thresholds/total_biomass", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)

#total_abundance
cfg_thr_total_abundance <- thr_lines_long_g1[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="total_abundance")

plots_thr_total_abundance <- map2(cfg_thr_total_abundance$dataset_gear, cfg_thr_total_abundance$thr, ~ plot_one_dg_thr_g1(.x, .y, "total_abundance"))
plots_thr_total_abundance <- compact(plots_thr_total_abundance) 
names(plots_thr_total_abundance)<-paste(cfg_thr_total_abundance$dataset_gear,cfg_thr_total_abundance$thr,sep = "_")


purrr::walk2(
  plots_thr_total_abundance,
  names(plots_thr_total_abundance),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group1_thresholds/total_abundance", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)


#richness
cfg_thr_richness <- thr_lines_long_g1[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="richness")

plots_thr_richness <- map2(cfg_thr_richness$dataset_gear, cfg_thr_richness$thr, ~ plot_one_dg_thr_g1(.x, .y, "richness"))
plots_thr_richness <- compact(plots_thr_richness) 
names(plots_thr_richness)<-paste(cfg_thr_richness$dataset_gear,cfg_thr_richness$thr,sep = "_")


purrr::walk2(
  plots_thr_richness,
  names(plots_thr_richness),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group1_thresholds/richness", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)



#relM_biomass
cfg_thr_relM_biomass <- thr_lines_long_g1[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="relM_biomass")

plots_thr_relM_biomass <- map2(cfg_thr_relM_biomass$dataset_gear, cfg_thr_relM_biomass$thr, ~ plot_one_dg_thr_g1(.x, .y, "relM_biomass"))
plots_thr_relM_biomass <- compact(plots_thr_relM_biomass) 
names(plots_thr_relM_biomass)<-paste(cfg_thr_relM_biomass$dataset_gear,cfg_thr_relM_biomass$thr,sep = "_")


purrr::walk2(
  plots_thr_relM_biomass,
  names(plots_thr_relM_biomass),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group1_thresholds/relM_biomass", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)



#relM_abundance
cfg_thr_relM_abundance <- thr_lines_long_g1[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="relM_abundance")

plots_thr_relM_abundance <- map2(cfg_thr_relM_abundance$dataset_gear, cfg_thr_relM_abundance$thr, ~ plot_one_dg_thr_g1(.x, .y, "relM_abundance"))
plots_thr_relM_abundance <- compact(plots_thr_relM_abundance) 
names(plots_thr_relM_abundance)<-paste(cfg_thr_relM_abundance$dataset_gear,cfg_thr_relM_abundance$thr,sep = "_")


purrr::walk2(
  plots_thr_relM_abundance,
  names(plots_thr_relM_abundance),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group1_thresholds/relM_abundance", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)



#SoS
cfg_thr_SoS <- thr_lines_long_g1[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="SoS")

plots_thr_SoS <- map2(cfg_thr_SoS$dataset_gear, cfg_thr_SoS$thr, ~ plot_one_dg_thr_g1(.x, .y, "SoS"))
plots_thr_SoS <- compact(plots_thr_SoS) 
names(plots_thr_SoS)<-paste(cfg_thr_SoS$dataset_gear,cfg_thr_SoS$thr,sep = "_")


purrr::walk2(
  plots_thr_SoS,
  names(plots_thr_SoS),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group1_thresholds/SoS", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)




# long table of lines for BOTH panels
plot_dat_g2 <- bind_rows(
  df_long_analysis_group2 %>%
    transmute(dataset_gear, thr, kmean_cluster_group2, indicator, year,
              panel = "Orig", y = value),
  
  df_long_analysis_group2 %>%
    transmute(dataset_gear, thr, kmean_cluster_group2, indicator, year,
              panel = "Transf", y = value_t),
  
  ts_pred_year_group2 %>%
    transmute(dataset_gear, thr, kmean_cluster_group2, indicator, year,
              panel = "Detr", y = detr),
  
  ts_pred_year_group2 %>%
    transmute(dataset_gear, thr, kmean_cluster_group2, indicator, year,
              panel = "Ratio", y = 1 + detr)
) %>%
  mutate(
    panel = factor(panel, levels = c("Orig","Transf","Detr","Ratio")),
    kmean_cluster_group2 = factor(kmean_cluster_group2)
  )


thr_lines_long_g2 <- thresh_summary_group2 %>%
  transmute(
    dataset_gear, thr, kmean_cluster_group2, indicator,
    kmean_cluster_group2 = factor(kmean_cluster_group2),
    
    # --- Detr-panel lines (on detr scale) ---
    ref_detr  = 0,
    dc_detr   = ci_low,
    nv15_detr = qnorm(0.15)  * sd_detr,
    nv05_detr = qnorm(0.05)  * sd_detr,
    nv025_detr= qnorm(0.025) * sd_detr,
    
    # --- Ratio-panel lines (on ratio scale) ---
    ref_ratio  = 1,
    dc_ratio   = dect_change,
    nv15_ratio = nv_15,
    nv05_ratio = nv_050,
    nv025_ratio= nv_025
  ) %>%
  # make two panel-specific blocks
  tidyr::pivot_longer(
    cols = c(ref_detr, dc_detr, nv15_detr, nv05_detr, nv025_detr,
             ref_ratio, dc_ratio, nv15_ratio, nv05_ratio, nv025_ratio),
    names_to = "name",
    values_to = "yline"
  ) %>%
  mutate(
    panel = case_when(
      grepl("_detr$",  name) ~ "Detr",
      grepl("_ratio$", name) ~ "Ratio",
      TRUE ~ NA_character_
    ),
    thr_type = case_when(
      grepl("^ref_",  name) ~ "Reference",
      grepl("^dc_",   name) ~ "Detectable change",
      grepl("^nv15_", name) ~ "NV 0.15",
      grepl("^nv05_", name) ~ "NV 0.05",
      grepl("^nv025_",name) ~ "NV 0.025",
      TRUE ~ NA_character_
    ),
    panel = factor(panel, levels = c("Orig","Transf","Detr","Ratio")),
    thr_type = factor(thr_type, levels = c("Reference","Detectable change","NV 0.15","NV 0.05","NV 0.025"))
  ) %>%
  filter(!is.na(panel), !is.na(thr_type))


plot_one_dg_thr_g2 <- function(dg, thr_val, ind) {
  
  dd <- plot_dat_g2 %>%
    filter(dataset_gear == dg, thr == thr_val, indicator == ind) %>%
    mutate(
      year_f = factor(year, levels = sort(unique(year))),
      kmean_cluster_group2 = factor(kmean_cluster_group2)
    )
  
  if (nrow(dd) == 0) return(NULL)
  
  ll <- thr_lines_long_g2 %>%
    filter(dataset_gear == dg, thr == thr_val, indicator == ind)
  
  ggplot(dd, aes(x = year_f, y = y)) +
    geom_point(size = 1.6, alpha = 0.8) +
    
    # coloured thresholds (only exist for panels Detr + Ratio)
    geom_hline(
      data = ll,
      aes(yintercept = yline, colour = thr_type),
      linewidth = 0.8
    ) +
    
    facet_grid2(
      kmean_cluster_group2 ~ panel,
      scales = "free_y",
      independent = "y"
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_colour_manual(
      name = NULL,
      values = c(
        "Reference"         = col_ref,
        "Detectable change" = col_dc,
        "NV 0.15"           = col_nv15,
        "NV 0.05"           = col_nv05,
        "NV 0.025"          = col_nv025
      )
    ) +
    guides(
      colour = guide_legend(
        direction = "horizontal",
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.2)
      )
    ) +
    labs(
      title = paste0(dg, " | thr = ", thr_val, " | ", ind),
      x = NULL, y = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.background = element_rect(fill = "grey95"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center",
      legend.justification = "center"
    )
}


plot_one_dg_thr_g2(
  dg = "BoBIC_IberianChabitats_otter_trawl",
  thr_val = 0.1,
  ind = "richness"
)



#total_biomass
cfg_thr_total_biomass <- thr_lines_long_g2[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="total_biomass")

plots_thr_total_biomass <- map2(cfg_thr_total_biomass$dataset_gear, cfg_thr_total_biomass$thr, ~ plot_one_dg_thr_g2(.x, .y, "total_biomass"))
plots_thr_total_biomass <- compact(plots_thr_total_biomass) 
names(plots_thr_total_biomass)<-paste(cfg_thr_total_biomass$dataset_gear,cfg_thr_total_biomass$thr,sep = "_")


purrr::walk2(
  plots_thr_total_biomass,
  names(plots_thr_total_biomass),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group2_thresholds/total_biomass", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)

#total_abundance
cfg_thr_total_abundance <- thr_lines_long_g2[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="total_abundance")

plots_thr_total_abundance <- map2(cfg_thr_total_abundance$dataset_gear, cfg_thr_total_abundance$thr, ~ plot_one_dg_thr_g2(.x, .y, "total_abundance"))
plots_thr_total_abundance <- compact(plots_thr_total_abundance) 
names(plots_thr_total_abundance)<-paste(cfg_thr_total_abundance$dataset_gear,cfg_thr_total_abundance$thr,sep = "_")


purrr::walk2(
  plots_thr_total_abundance,
  names(plots_thr_total_abundance),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group2_thresholds/total_abundance", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)


#richness
cfg_thr_richness <- thr_lines_long_g2[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="richness")

plots_thr_richness <- map2(cfg_thr_richness$dataset_gear, cfg_thr_richness$thr, ~ plot_one_dg_thr_g2(.x, .y, "richness"))
plots_thr_richness <- compact(plots_thr_richness) 
names(plots_thr_richness)<-paste(cfg_thr_richness$dataset_gear,cfg_thr_richness$thr,sep = "_")


purrr::walk2(
  plots_thr_richness,
  names(plots_thr_richness),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group2_thresholds/richness", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)



#relM_biomass
cfg_thr_relM_biomass <- thr_lines_long_g2[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="relM_biomass")

plots_thr_relM_biomass <- map2(cfg_thr_relM_biomass$dataset_gear, cfg_thr_relM_biomass$thr, ~ plot_one_dg_thr_g2(.x, .y, "relM_biomass"))
plots_thr_relM_biomass <- compact(plots_thr_relM_biomass) 
names(plots_thr_relM_biomass)<-paste(cfg_thr_relM_biomass$dataset_gear,cfg_thr_relM_biomass$thr,sep = "_")


purrr::walk2(
  plots_thr_relM_biomass,
  names(plots_thr_relM_biomass),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group2_thresholds/relM_biomass", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)



#relM_abundance
cfg_thr_relM_abundance <- thr_lines_long_g2[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="relM_abundance")

plots_thr_relM_abundance <- map2(cfg_thr_relM_abundance$dataset_gear, cfg_thr_relM_abundance$thr, ~ plot_one_dg_thr_g2(.x, .y, "relM_abundance"))
plots_thr_relM_abundance <- compact(plots_thr_relM_abundance) 
names(plots_thr_relM_abundance)<-paste(cfg_thr_relM_abundance$dataset_gear,cfg_thr_relM_abundance$thr,sep = "_")


purrr::walk2(
  plots_thr_relM_abundance,
  names(plots_thr_relM_abundance),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group2_thresholds/relM_abundance", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)



#SoS
cfg_thr_SoS <- thr_lines_long_g2[,c("dataset_gear","thr","indicator")] %>%
  distinct(dataset_gear, thr,indicator) %>%
  filter(indicator=="SoS")

plots_thr_SoS <- map2(cfg_thr_SoS$dataset_gear, cfg_thr_SoS$thr, ~ plot_one_dg_thr_g2(.x, .y, "SoS"))
plots_thr_SoS <- compact(plots_thr_SoS) 
names(plots_thr_SoS)<-paste(cfg_thr_SoS$dataset_gear,cfg_thr_SoS$thr,sep = "_")


purrr::walk2(
  plots_thr_SoS,
  names(plots_thr_SoS),
  ~ ggsave(
    filename = file.path("./WKBENTH4/results/phase3_threshold_evaluation/kmeans_cluster_group2_thresholds/SoS", paste0(.y, ".png")),
    plot = .x,
    width = 14, height = 9, dpi = 300
  )
)