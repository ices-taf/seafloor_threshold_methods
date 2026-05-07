#########################################################################################
#
#                              Phase 2: Clusterization
#                    K-means clustering per dataset_gear, per pressure_type,
#            with MULTIPLE thresholds, using OPTIMAL k (group2) from summary table
#
##########################################################################################
#ICES WKBENTH4
#Script Author: Gabriele Di Bona
#February 2026

library(data.table)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(viridis)

rm(list = ls())
options(scipen = 999)
set.seed(666)

# ---- paths ----
in_file  <- "./WKBENTH4/data/WKBENTH4_datacall_merged.csv"
k_file   <- "./WKBENTH4/results/WKBENTH4_Type3_k_optimum_kmean_cluster_summary.csv"
out_dir  <- "./WKBENTH4/results/k_mean_cluster_block"

# ---- load data ----
dataset.merged <- as.data.frame(fread(in_file)) %>%
  mutate(
    dataset_gear = paste(dataset, gear, sep = "_"),
    longitude = as.numeric(gsub(",", ".", longitude)),
    latitude  = as.numeric(gsub(",", ".", latitude)),
    pressure_value = as.numeric(gsub(",", ".", pressure_value))
  )

# ---- load optimal k summary ----
k_opt <- as.data.frame(fread(k_file)) %>%
  mutate(
    thr_chosen = as.numeric(thr_chosen),
    k_group1   = as.integer(k_group1),
    k_group2   = suppressWarnings(as.integer(k_group2))
  ) %>%
  filter(!is.na(dataset_gear), !is.na(pressure_type), is.finite(thr_chosen)) %>%
  # keep only rows where k_group2 is valid
  filter(is.finite(k_group2), k_group2 >= 2)

# ============================================================
# 1) K-means clustering for ONE dataset_gear at ONE threshold
# Returns mapping table: dataset_gear, lon, lat -> kmean_cluster
# ============================================================
cluster_one_dataset <- function(dat, dataset_name, k, pressure_thresh, make_maps = FALSE) {
  
  df <- dat %>%
    filter(dataset_gear == dataset_name,
           is.finite(pressure_value),
           pressure_value <= pressure_thresh)
  
  if (nrow(df) == 0) return(NULL)
  if (is.na(k) || !is.finite(k) || k < 2) return(NULL)
  
  # unique points aggregation
  df.agg <- df %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    group_by(dataset_gear, longitude, latitude) %>%
    summarise(
      n_obs      = dplyr::n(),
      emod_depth = mean(emod_depth, na.rm = TRUE),
      .groups    = "drop"
    )
  
  df.agg$emod_depth[is.nan(df.agg$emod_depth)] <- NA
  
  # project to EPSG:3035
  pts <- st_as_sf(df.agg, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  xy  <- st_coordinates(st_transform(pts, 3035))
  df.agg$x_3035 <- xy[, 1]
  df.agg$y_3035 <- xy[, 2]
  
  X <- df.agg[, c("x_3035", "y_3035", "emod_depth")]
  keep <- complete.cases(X)
  df.agg <- df.agg[keep, , drop = FALSE]
  X <- X[keep, , drop = FALSE]
  
  n <- nrow(X)
  if (n < 3) return(NULL)
  
  # keep k valid
  if (k > (n - 1)) k <- n - 1
  if (k < 2) return(NULL)
  
  km <- kmeans(scale(X), centers = k, nstart = 50, iter.max = 100)
  df.agg$kmean_cluster <- km$cluster
  
  if (make_maps) {
    p <- ggplot(df.agg, aes(longitude, latitude, color = factor(kmean_cluster))) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = paste0("Kmeans clusters - ", dataset_name,
                          " (thr=", pressure_thresh, ", k=", k, ")"),
           color = "cluster")
    print(p)
  }
  
  df.agg %>%
    transmute(dataset_gear, longitude, latitude, kmean_cluster)
}

# ============================================================
# 2) Plot helper (two stacked barplots) + save
# ============================================================
make_cluster_year_plots <- function(df_thr, out_prefix, out_dir) {
  
  df_plot <- df_thr %>%
    filter(!is.na(year), !is.na(kmean_cluster), !is.na(dataset_gear)) %>%
    mutate(
      year = as.integer(year),
      year.f = factor(year),
      kmean_cluster = factor(kmean_cluster),
      dataset_gear = factor(dataset_gear)
    )
  
  if (nrow(df_plot) == 0) return(invisible(NULL))
  
  p1 <- ggplot(df_plot, aes(x = year, fill = kmean_cluster)) +
    geom_bar(width = 0.9) +
    facet_wrap(~ dataset_gear, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(df_plot$year))) +
    labs(x = "Year", y = "Number of observations", fill = "Cluster") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(df_plot, aes(x = kmean_cluster, fill = year.f)) +
    geom_bar(width = 0.9) +
    facet_wrap(~ dataset_gear, scales = "free_y") +
    labs(x = "Cluster", y = "Number of observations", fill = "Year") +
    scale_fill_viridis_d(option = "C") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(plot = p1,
         filename = file.path(out_dir, paste0(out_prefix, "_v1.png")),
         dpi = 300, width = 25, height = 20, units = "cm", bg = "white")
  
  ggsave(plot = p2,
         filename = file.path(out_dir, paste0(out_prefix, "_v2.png")),
         dpi = 300, width = 25, height = 20, units = "cm", bg = "white")
  
  invisible(list(p_year_by_cluster = p1, p_cluster_by_year = p2))
}

# ============================================================
# 3) Runner: use k_opt to cluster for each (pressure_type, thr)
# ============================================================
run_from_kopt <- function(dat_all, k_opt,
                          make_maps = FALSE,
                          save_plots = TRUE,
                          out_dir = "./WKBENTH4/results",
                          out_prefix_base = "WKBENTH4_Type3") {
  
  thr_tag <- function(thr) gsub("\\.", "p", format(thr, scientific = FALSE, trim = TRUE))
  
  combos <- k_opt %>%
    distinct(pressure_type, thr_chosen) %>%
    arrange(pressure_type, thr_chosen)
  
  out_list <- vector("list", nrow(combos))
  names(out_list) <- paste0(combos$pressure_type, "__thr_", thr_tag(combos$thr_chosen))
  
  for (i in seq_len(nrow(combos))) {
    
    pt  <- combos$pressure_type[i]
    thr <- combos$thr_chosen[i]
    
    dat_pt <- dat_all %>% filter(pressure_type == pt)
    if (nrow(dat_pt) == 0) next
    
    # dataset_gear list present for this pressure_type in data
    datasets <- sort(unique(dat_pt$dataset_gear))
    
    # k by dataset from k_opt (group2)
    k_by_dataset <- k_opt %>%
      filter(pressure_type == pt, thr_chosen == thr) %>%
      select(dataset_gear, k_group2) %>%
      distinct() %>%
      # keep only those dataset_gear that exist in dat_pt
      filter(dataset_gear %in% datasets) %>%
      tibble::deframe()
    
    if (length(k_by_dataset) == 0) next
    
    # cluster map (bind_rows handles NULLs)
    cl_map <- bind_rows(lapply(names(k_by_dataset), function(ds) {
      cluster_one_dataset(
        dat = dat_pt,
        dataset_name = ds,
        k = k_by_dataset[[ds]],
        pressure_thresh = thr,
        make_maps = make_maps
      )
    }))
    
    if (nrow(cl_map) == 0) next
    
    # apply thr, join clusters
    dat_thr <- dat_pt %>%
      filter(is.finite(pressure_value), pressure_value <= thr) %>%
      left_join(cl_map, by = c("dataset_gear", "longitude", "latitude")) %>%
      filter(!is.na(kmean_cluster)) %>%
      mutate(thr = thr, pressure_type = pt)
    
    # plots
    if (save_plots && nrow(dat_thr) > 0) {
      out_prefix <- paste0(out_prefix_base, "_", pt,
                           "_thr_", thr_tag(thr),
                           "_dataset_kmeancluster_group2_year_barplot")
      make_cluster_year_plots(dat_thr, out_prefix = out_prefix, out_dir = out_dir)
    }
    
    out_list[[i]] <- list(
      data = dat_thr,
      clusters = mutate(cl_map, thr = thr, pressure_type = pt)
    )
  }
  
  out_list
}

# ============================================================
# 4) RUN ALL (from k_opt)
# ============================================================
res_all <- run_from_kopt(
  dat_all = dataset.merged,
  k_opt   = k_opt,
  make_maps = TRUE,
  save_plots = TRUE,
  out_dir = out_dir,
  out_prefix_base = "WKBENTH4_Type3"
)

# remove NULL entries (pressure_type x thr with no output)
res_all <- res_all[!vapply(res_all, is.null, logical(1))]

# combined thresholded+clustered data across ALL pressure types and thresholds
dataset.merged.thr_all <- bind_rows(lapply(res_all, `[[`, "data"))

# Combine cluster maps across all
cluster.maps.all <- bind_rows(lapply(res_all, `[[`, "clusters"))

# Save data
fwrite(dataset.merged.thr_all,
       file.path(out_dir, "WKBENTH4_Type3_pressuretype_fixedpthr_kmeans_clusters_group2.csv"))

# optional: save cluster maps too
fwrite(cluster.maps.all,
       file.path(out_dir, "WKBENTH4_Type3_pressuretype_fixedpthr_kmeans_cluster_maps_group2.csv"))

dataset.merged.thr_all