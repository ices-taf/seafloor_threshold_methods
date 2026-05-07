#########################################################################################
#
#                              Phase 2: Clusterization
#                    Optimum k for clustering algorithm evaluation
#                 Loop across pressure_type and fixed pressure thresholds
#
##########################################################################################
#ICES WKBENTH4
#Script Author: Gabriele Di Bona
#February 2026


library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(fpc)

rm(list=ls()) # clean R environment
options(scipen = 999) # adopt scientific notation
set.seed(666)

# ---- load data ----
dataset.merged <- as.data.frame(
  fread("./WKBENTH4/data/WKBENTH4_datacall_merged.csv")
)
dataset.merged$dataset_gear <- paste(dataset.merged$dataset, dataset.merged$gear, sep = "_")
dataset.merged<-filter(dataset.merged,pressure_type!="total_phosphorus")

# ---- helper functions ----

# safer than first() for habitat_type at a point
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# plateau-based per-index coarse/fine suggestions
pick_plateau_k <- function(k, score, maximize = TRUE, tol = 0.05) {
  ok <- is.finite(score) & is.finite(k)
  k <- k[ok]; score <- score[ok]
  if (!length(k)) return(list(coarse = NA_integer_, fine = NA_integer_))
  
  if (maximize) {
    best <- max(score, na.rm = TRUE)
    keep <- score >= (1 - tol) * best
  } else {
    best <- min(score, na.rm = TRUE)
    keep <- score <= (1 + tol) * best
  }
  
  ks <- sort(unique(k[keep]))
  if (!length(ks)) return(list(coarse = NA_integer_, fine = NA_integer_))
  list(coarse = min(ks), fine = median(ks))
}

# choose most frequent integer
mode_int <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_integer_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_int <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_integer_ else as.integer(x[1])
}

# ---- clustering function (per dataset_gear) ----
run_clustering_one_dataset <- function(dat,
                                       dataset_name,
                                       pressure_thr,
                                       features = c("x_3035", "y_3035", "emod_depth"),
                                       scale_features = TRUE,
                                       hc_method = "ward.D2",
                                       k_hc = 4,
                                       k_grid = 2:50,
                                       verbose = TRUE) {
  
  # filter to dataset_gear and thr
  df <- dat %>%
    filter(dataset_gear == dataset_name) %>%
    filter(pressure_value <= pressure_thr)
  
  if (nrow(df) == 0) {
    if (verbose) warning(paste("No rows after filtering for dataset =", dataset_name))
    return(NULL)
  }
  
  # aggregate to unique spatial points
  df.agg <- df %>%
    filter(!is.na(dataset), !is.na(gear),
           !is.na(longitude), !is.na(latitude)) %>%
    group_by(dataset_gear, longitude, latitude) %>%
    summarise(
      n_obs      = dplyr::n(),
      depth      = mean(depth, na.rm = TRUE),
      emod_depth = mean(emod_depth, na.rm = TRUE),
      .groups = "drop"
    )
  
  df.agg$depth[is.nan(df.agg$depth)] <- NA
  df.agg$emod_depth[is.nan(df.agg$emod_depth)] <- NA
  
  # to EPSG:3035
  pts <- st_as_sf(df.agg, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  pts3035 <- st_transform(pts, 3035)
  xy <- st_coordinates(pts3035)
  
  df.agg$x_3035 <- xy[, "X"]
  df.agg$y_3035 <- xy[, "Y"]
  
  X <- as.data.frame(df.agg[, features, drop = FALSE])
  
  keep <- stats::complete.cases(X)
  df.agg <- df.agg[keep, , drop = FALSE]
  X <- X[keep, , drop = FALSE]
  
  n <- nrow(X)
  if (n < 3) {
    if (verbose) warning(paste("Too few complete rows for clustering in dataset =", dataset_name, "(n =", n, ")"))
    return(NULL)
  }
  
  if (scale_features) X <- scale(X)
  
  # valid k for this dataset
  k_hc_use   <- min(k_hc, n)
  k_grid_use <- intersect(k_grid, 2:(n - 1))
  
  if (length(k_grid_use) < 1) {
    if (verbose) warning(paste("No valid k in k_grid for dataset =", dataset_name, "(n =", n, ")."))
    return(list(dataset = dataset_name, n_points = n, kmeans_metrics = NULL, best_k = NULL))
  }
  
  # distance in standardized feature space
  d <- dist(X, method = "euclidean")
  
  # optional HC diagnostic
  hc <- hclust(d, method = hc_method)
  hc_labels <- cutree(hc, k = k_hc_use)
  
  # compute kmeans + indices
  perf_list <- lapply(k_grid_use, function(k) {
    km <- kmeans(X, centers = k, nstart = 50, iter.max = 100)
    cs <- cluster.stats(d, km$cluster)
    list(k = k, stats = cs)
  })
  
  metrics <- data.frame(
    dataset         = dataset_name,
    n_points        = n,
    k              = sapply(perf_list, `[[`, "k"),
    average.between = sapply(perf_list, function(z) z$stats$average.between),
    average.within  = sapply(perf_list, function(z) z$stats$average.within),
    max.distance    = sapply(perf_list, function(z) z$stats$max.diameter),
    min.separation  = sapply(perf_list, function(z) z$stats$min.separation),
    ch              = sapply(perf_list, function(z) z$stats$ch),
    avg_silwidth    = sapply(perf_list, function(z) z$stats$avg.silwidth),
    dunn            = sapply(perf_list, function(z) z$stats$dunn),
    dunn2           = sapply(perf_list, function(z) z$stats$dunn2),
    pearsongamma    = sapply(perf_list, function(z) z$stats$pearsongamma),
    wb.ratio        = sapply(perf_list, function(z) z$stats$wb.ratio),
    sindex          = sapply(perf_list, function(z) z$stats$sindex),
    stringsAsFactors = FALSE
  )
  
  # ---- deterministic coarse/fine selection (mode across per-index plateaus) ----
  tol_max <- 0.05
  tol_min <- 0.05
  
  k_suggestions <- list(
    sil           = pick_plateau_k(metrics$k, metrics$avg_silwidth, maximize = TRUE,  tol = tol_max),
    wb.ratio      = pick_plateau_k(metrics$k, metrics$wb.ratio,     maximize = FALSE, tol = tol_min)
  )
  
  k_sug_df <- data.frame(
    index    = names(k_suggestions),
    k_coarse = sapply(k_suggestions, function(z) z$coarse),
    k_fine   = sapply(k_suggestions, function(z) z$fine),
    stringsAsFactors = FALSE
  )
  
  k_group1 <- mode_int(k_sug_df$k_coarse)
  k_group2 <- mode_int(k_sug_df$k_fine)
  
  valid_k <- sort(unique(metrics$k))
  k_group1 <- if (is.finite(k_group1)) valid_k[which.min(abs(valid_k - k_group1))] else NA_integer_
  k_group2 <- if (is.finite(k_group2)) valid_k[which.min(abs(valid_k - k_group2))] else NA_integer_
  
  if (is.finite(k_group1) && is.finite(k_group2) && k_group2 < k_group1) {
    tmp <- k_group1; k_group1 <- k_group2; k_group2 <- tmp
  }
  
  
  best_k <- list(
    by_index = k_sug_df,
    k_group1 = k_group1,
    k_group2 = k_group2
  )
  
  list(
    dataset = dataset_name,
    n_points = n,
    kmeans_metrics = metrics,
    best_k = best_k,
    hc_labels = hc_labels
  )
}

# ---- Phase 2 runner for one (pressure_type, thr) ----
run_phase2_for_pressure_thr <- function(dataset_all,
                                        pressure_type_selected,
                                        thr_selected,
                                        k_grid = 2:50,
                                        outdir = "./WKBENTH4/results/phase2_clusterization",
                                        verbose = TRUE) {
  
  dat_pt <- dplyr::filter(dataset_all, pressure_type == pressure_type_selected)
  if (nrow(dat_pt) == 0) {
    if (verbose) message("No rows for pressure_type = ", pressure_type_selected)
    return(NULL)
  }
  
  dataset_names <- sort(unique(dat_pt$dataset_gear))
  if (length(dataset_names) == 0) return(NULL)
  
  # run clustering
  results_by_dataset <- lapply(dataset_names, function(ds) {
    run_clustering_one_dataset(
      dat = dat_pt,
      dataset_name = ds,
      pressure_thr = thr_selected,
      k_grid = k_grid,
      verbose = FALSE
    )
  })
  names(results_by_dataset) <- dataset_names
  
  # habitat summary at unique point level, within thr
  summary_by_dataset <- dat_pt %>%
    filter(dataset_gear %in% dataset_names, pressure_value <= thr_selected) %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    group_by(dataset_gear, longitude, latitude) %>%
    summarise(
      habitat_type = get_mode(habitat_type),
      .groups = "drop"
    ) %>%
    group_by(dataset_gear) %>%
    summarise(
      n_points = dplyr::n(),
      n_habitat_types = n_distinct(habitat_type, na.rm = TRUE),
      .groups = "drop"
    )
  
  # build k summary
  k_summary <- data.frame(
    dataset_gear  = names(results_by_dataset),
    pressure_type = pressure_type_selected,
    thr_chosen    = thr_selected,
    n_points_k    = NA_integer_,
    k_group1      = NA_integer_,
    k_group2      = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(results_by_dataset)) {
    res <- results_by_dataset[[i]]
    if (is.null(res)) next
    k_summary$n_points_k[i] <- safe_int(res$n_points)
    k_summary$k_group1[i]   <- safe_int(res$best_k$k_group1)
    k_summary$k_group2[i]   <- safe_int(res$best_k$k_group2)
  }
  
  k_summary <- k_summary %>%
    left_join(summary_by_dataset, by = "dataset_gear") %>%
    mutate(n_points = ifelse(!is.na(n_points_k), n_points_k, n_points)) %>%
    select(dataset_gear, pressure_type, thr_chosen, k_group1, k_group2, n_points, n_habitat_types)
  
  # write output
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  thr_tag <- gsub("\\.", "p", format(thr_selected, scientific = FALSE, trim = TRUE))
  pt_tag  <- gsub("[^A-Za-z0-9_]+", "_", pressure_type_selected)
  out_file <- file.path(outdir, paste0("k_optimum_", pt_tag, "_thr_", thr_tag, ".csv"))
  fwrite(k_summary, out_file)
  
  if (verbose) message("Saved: ", out_file)
  
  k_summary
}

# ---- define the run grid ----

pressure_types_to_run <- sort(unique(dataset.merged$pressure_type))

# Define thresholds per pressure type.
thr_grid_by_pressure <- list(
  SAR_5yr_avg      = c(0.10,0.25, 0.35, 0.50, 0.65, 0.75, 1.00),
  SAR_3yr_avg      = c(0.10, 0.35, 0.50, 0.65, 1.00),
  SAR_1yr_lag      = c(0.10, 0.35, 0.50, 0.65, 1.00),
  subSAR_total_avg = c(0.08, 0.28, 0.40, 0.52, 0.80),
  total_phosphorus = c(21)
)


# ---- run loop ----
all_k_summaries <- list()

for (pt in pressure_types_to_run) {
  thr_vec <- thr_grid_by_pressure[[pt]]
  if (is.null(thr_vec) || length(thr_vec) == 0) next
  
  for (thr in thr_vec) {
    message("Running pressure_type = ", pt, " | thr = ", thr)
    out <- run_phase2_for_pressure_thr(
      dataset_all = dataset.merged,
      pressure_type_selected = pt,
      thr_selected = thr,
      k_grid = 2:50,
      outdir = "./WKBENTH4/results/phase2_clusterization/k_optimum",
      verbose = TRUE
    )
    if (!is.null(out)) {
      all_k_summaries[[paste(pt, thr, sep = "__")]] <- out
    }
  }
}

k_summary_all <- bind_rows(all_k_summaries)

# ---- save combined table ----
out_all <- "./WKBENTH4/results/WKBENTH4_Type3_k_optimum_kmean_cluster_summary.csv"
fwrite(k_summary_all, out_all)


# inspect
k_summary_all
