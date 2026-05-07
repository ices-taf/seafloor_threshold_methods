# =======================================================================
# Pressure-state threshold estimation for Type 3 datasets: Richness
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script estimates pressure-state thresholds for one Type 3 dataset
# and one response indicator at a time, using GAM-based pressure-state
# relationships fitted separately for each habitat.
#
# Threshold methods implemented:
#   - Natural Variation (NV), also referred to as Zero Pressure (ZP)
#   - Detectable Change (DC), also referred to as First Detectable Change (FDC)
#
# The script reads the "Station information" sheet from one post-workshop
# harmonised Type 3 Excel file containing indicator values, applies data
# cleaning and habitat-level filtering, fits a GAM for each habitat, and
# computes thresholds only when the fitted pressure-state relationship
# meets the required criteria.
#
# Outputs include:
#   - a table of thresholds for retained habitats
#   - a table summarising GAM significance and model statistics
#   - one diagnostic plot per habitat showing NV and DC  results

# -----------------------------------------------------------------------
# Methodological notes
# -----------------------------------------------------------------------
# Thresholds are estimated independently for each habitat within each
# dataset. Pressure-state relationships are modelled using GAMs, and
# predictions are standardised to the fitted value at zero pressure
# (P = 0).
#
# The pressure domain is restricted to a common range (SAR <= 12) to
# ensure comparability across datasets and habitats.
#
# For richness, the response variable is log1p-transformed prior to
# modelling and fitted using a gaussian GAM.
#
# Thresholds are only derived for habitats that satisfy the minimum data
# requirements and for which the fitted GAM smooth term is considered
# significant (p < 0.06).
#
# A slightly relaxed threshold is used to allow threshold estimation for
# habitats showing near-significant pressure-state relationships.
# This is intended to avoid excluding ecologically meaningful responses
# due to marginal statistical support.
#
# However, results for habitats with p-values > 0.05 should be treated
# with caution.
#
# Additional filtering may be applied to exclude habitats with unstable
# responses or insufficient representation along the pressure gradient.

# -----------------------------------------------------------------------
# Note on pressure variable selection
# -----------------------------------------------------------------------
# The pressure variable is not always stored under the same column name
# across Type 3 datasets. Depending on the dataset, the selected trawling
# pressure metric may appear, for example, as:
#   - pressure_value
#   - pressure_value2
#   - SAR5
#
# The user must check the metadata sheet in the corresponding Excel file
# and manually specify the pressure column to be used in the script.
#
# When several SAR metrics are available, the preferred order is:
#   1. SAR5
#   2. SAR3
#   3. SAR1
#
# The selected column is renamed to "pressure_value" after import so that
# the rest of the script can run unchanged.
# -----------------------------------------------------------------------
# Input data
# -----------------------------------------------------------------------
# Input data correspond to post-workshop harmonised Type 3 datasets.
# These datasets include habitat-level pressure and indicator information
# prepared for pressure-state modelling after the WKBENTH4 workshop.
#
# The "Station information" sheet may contain, depending on the dataset,
# the following indicators:
#   - SoS (Sentinel of the Seabed)
#   - Total biomass
#   - Richness
#   - Margalef diversity
#
# This script uses the richness field, modelled as log1p-transformed richness.
# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - mgcv
# - readxl
# - dplyr
# - ggplot2
# - ggpubr
# - openxlsx

# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------

library(mgcv)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(openxlsx)

# =========================
# 1) User settings
# =========================

dataset_name <- "BoBIC_IberianChabitats"
sheet_name   <- "Station information"

# Check the metadata sheet before selecting the pressure column.
# Preferred order: SAR5, then SAR3, then SAR1.
pressure_column <- "pressure_value"


# Input folder containing post-workshop harmonised Type 3 Excel files
input_folder <- "../../../data/Type3"

# Output folder for threshold results, organised by dataset and indicator
output_folder <- file.path(
  "../../../outputs/Thresholds/Pressure_state",
  dataset_name,
  "Richness"
)

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Input Excel file
input_xlsx <- file.path(input_folder, paste0(dataset_name, ".xlsx"))

if (!file.exists(input_xlsx)) {
  stop(paste("Input file not found:", input_xlsx))
}

base_name <- tools::file_path_sans_ext(basename(input_xlsx))
# Output files
thresholds_file <- file.path(
  output_folder,
  paste0(dataset_name, "_Richness_thresholds.xlsx")
)


plots_folder <- file.path(output_folder, "plots")

if (!dir.exists(plots_folder)) {
  dir.create(plots_folder, recursive = TRUE)
}

# =========================
# 2) Read input data
# =========================

station_info <- read_excel(input_xlsx, sheet = sheet_name)


if (!pressure_column %in% names(station_info)) {
  stop(paste("Pressure column not found in Station information sheet:", pressure_column))
}

station_info$pressure_value <- station_info[[pressure_column]]

# =========================
# Helper: keep label visible inside [0,1]
# =========================
label_y <- function(y, offset = 0.05) {
  pmax(0.02, pmin(0.98, y - offset))
}

# =========================
# 3) Clean core fields (IMPORTANT)
# =========================
station_info$pressure_value <- as.numeric(gsub(",", ".", station_info$pressure_value))
station_info$richness       <- as.numeric(gsub(",", ".", station_info$richness))

# log1p-Richness
station_info$bio_log <- log1p(station_info$richness)

station_info <- station_info %>%
  filter(is.finite(pressure_value), is.finite(bio_log)) %>%
  filter(pressure_value <= 12)

# Pressure grid for smooth predictions
nd <- data.frame(pressure_value = seq(0, 12, by = 0.001))
i0_nd <- which.min(abs(nd$pressure_value - 0))  # baseline index (P=0)


# =========================
# 5) Tables to store outputs
# =========================
Thresholds_table <- data.frame(
  Habitat = character(),
  Method = character(),
  Pressure_threshold = numeric(),
  State_threshold = numeric(),
  stringsAsFactors = FALSE
)

GAM_pvalues <- data.frame(
  Habitat = character(),
  n = integer(),
  p_smooth = numeric(),
  edf = numeric(),
  ref_df = numeric(),
  stringsAsFactors = FALSE
)

# =========================
# 6) Run per habitat
# =========================
habitats <- unique(station_info$habitat_type)

for (msfd_bht in habitats) {
  
  cat("\n============================================================\n")
  cat("Habitat:", msfd_bht, "\n")
  
  msfd_data <- station_info %>%
    filter(habitat_type == msfd_bht) %>%
    filter(is.finite(bio_log), is.finite(pressure_value))
  
  if (nrow(msfd_data) == 0) {
    cat("No data. Skipping habitat.\n")
    next
  }
  
  # EXCLUSION RULE 1: minimum sample size
  if (nrow(msfd_data) < 20) {
    cat("Excluded: <20 records (n =", nrow(msfd_data), ")\n")
    next
  }
  
  # EXCLUSION RULE 2: minimum number of hauls at low pressure
  n_high <- sum(msfd_data$pressure_value <= 0.65, na.rm = TRUE)
  if (n_high < 4) {
    cat("Excluded: <4 records with pressure_value <= 0.65 (n_high =", n_high, ")\n")
    next
  }
  
  # ---------------------------------------------------------
  # EXCLUSION RULE 3 (NEW): degradation at high pressure
  #   Keep if >=50% of predicted curve at P>=0.65 is below baseline (P=0)
  #   Exclude otherwise
  # ---------------------------------------------------------
  tmp_gam <- tryCatch(
    gam(bio_log ~ s(pressure_value, k = 3),
        data = msfd_data, family = gaussian(), method = "REML"),
    error = function(e) NULL
  )
  
  if (is.null(tmp_gam)) {
    cat("Excluded: preliminary GAM failed.\n")
    next
  }
  
  nd_tmp <- data.frame(pressure_value = seq(0, 12, by = 0.01))
  pred_tmp <- predict(tmp_gam, newdata = nd_tmp, type = "response")
  
  df_tmp <- data.frame(x = nd_tmp$pressure_value, y = pred_tmp)
  df_high <- df_tmp[df_tmp$x >= 0.65, ]
  
  if (nrow(df_high) < 5) {
    cat("Excluded: not enough grid points at high pressure.\n")
    next
  }
  
  y0 <- df_tmp$y[which.min(abs(df_tmp$x - 0))]
  prop_below <- mean(df_high$y < y0, na.rm = TRUE)
  
  cat("High-pressure check: prop_below_baseline =",
      round(prop_below, 2),
      " | y0 =", round(y0, 3), "\n")
  
  if (!is.finite(prop_below) || prop_below < 0.50) {
    cat("Excluded: <50% of predicted curve at P>=0.65 is below baseline (no clear degradation at high pressure).\n")
    next
  }
  
  cat("Shape-check passed: >=50% of high-pressure curve below baseline. Keeping habitat.\n")
  
  # Use the SAME model downstream (consistency)
  Mymodel_UBS <- tmp_gam
  
  smry <- summary(Mymodel_UBS)
  print(smry)
  
  # store p-values for ALL habitats (even if later skip thresholds)
  p_smooth <- suppressWarnings(smry$s.table[1, "p-value"])
  edf      <- suppressWarnings(smry$s.table[1, "edf"])
  ref_df   <- suppressWarnings(smry$s.table[1, "Ref.df"])
  
  GAM_pvalues <- rbind(
    GAM_pvalues,
    data.frame(
      Habitat = msfd_bht,
      n = nrow(msfd_data),
      p_smooth = p_smooth,
      edf = edf,
      ref_df = ref_df
    )
  )
  
  # p threshold
  if (is.na(p_smooth) || p_smooth >= 0.06) {
    cat("Smooth term NOT significant (p =", p_smooth, "). Skipping thresholds/plot.\n")
    next
  }
  cat("Smooth term SIGNIFICANT (p =", p_smooth, "). Computing thresholds...\n")
  
  # ======================================================================
  # A) Prediction curve + 95% CI (NV & DC)
  # ======================================================================
  pr <- predict(Mymodel_UBS, newdata = nd, type = "response", se.fit = TRUE)
  fit <- pr$fit
  se  <- pr$se.fit
  
  upper <- fit + 1.96 * se
  lower <- fit - 1.96 * se
  
  # normalize by fitted value at P=0 (baseline)
  m0 <- fit[i0_nd]
  if (!is.finite(m0) || m0 == 0) m0 <- max(fit, na.rm = TRUE)  # fallback
  
  NewData_CI <- data.frame(
    x = nd$pressure_value,
    y = fit / m0,
    upper = upper / m0,
    lower = lower / m0
  )
  
  # =========================
  # B) NATURAL VARIATION (NV)
  # =========================
  df <- NewData_CI[order(NewData_CI$x), ]
  i0 <- which.min(abs(df$x - 0))
  
  Sta_UBS_Nat <- df$lower[i0]
  thr <- Sta_UBS_Nat
  
  if (df$y[1] >= thr) idx <- which(df$y <= thr)[1] else idx <- which(df$y >= thr)[1]
  
  Pres_UBS_Nat <- NA
  if (!is.na(idx)) {
    if (idx == 1) {
      Pres_UBS_Nat <- df$x[1]
    } else {
      x1 <- df$x[idx - 1]; x2 <- df$x[idx]
      y1 <- df$y[idx - 1]; y2 <- df$y[idx]
      Pres_UBS_Nat <- x1 + (thr - y1) * (x2 - x1) / (y2 - y1)
    }
  }
  
  cat("NV threshold: P =", round(Pres_UBS_Nat, 3), " S =", round(Sta_UBS_Nat, 3), "\n")
  
  p_nv <- ggplot(NewData_CI, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    geom_line(linewidth = 1) +
    {if(!is.na(Pres_UBS_Nat)) geom_vline(xintercept = Pres_UBS_Nat, linetype = "dashed", linewidth = 0.8)} +
    geom_hline(yintercept = Sta_UBS_Nat, linetype = "dashed", linewidth = 0.8) +
    {if(!is.na(Pres_UBS_Nat)) annotate("point", x = Pres_UBS_Nat, y = Sta_UBS_Nat, size = 2)} +
    {if(!is.na(Pres_UBS_Nat)) annotate("text",
                                       x = Pres_UBS_Nat, y = label_y(Sta_UBS_Nat),
                                       label = paste0("P=", round(Pres_UBS_Nat, 3), "\nS=", round(Sta_UBS_Nat, 3)),
                                       hjust = -0.1, vjust = 1, size = 3)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, 12)) +
    labs(x = "Pressure", y = "Richness (log1p, normalized to P=0)", title = "Natural Variation") +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Habitat = msfd_bht,
      Method = "Natural variation",
      Pressure_threshold = round(Pres_UBS_Nat, 3),
      State_threshold = round(Sta_UBS_Nat, 3)
    )
  )
  
  # =========================
  # C) DETECTABLE CHANGE (DC)
  # =========================
  Th0 <- NewData_CI$lower[i0]
  
  idx_dc <- which(NewData_CI$upper <= Th0)[1]
  Pres_UBS_DC <- if(!is.na(idx_dc)) NewData_CI$x[idx_dc] else NA
  Sta_UBS_DC  <- if(!is.na(idx_dc)) NewData_CI$y[idx_dc] else NA
  
  cat("DC threshold: P =", round(Pres_UBS_DC, 3),
      " S_final =", round(Sta_UBS_DC, 3),
      " (S_ref@P0_lowerCI =", round(Th0, 3), ")\n")
  
  p_dc <- ggplot(NewData_CI, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = Th0, linetype = "dotted", linewidth = 0.4) +
    {if(!is.na(Pres_UBS_DC)) geom_vline(xintercept = Pres_UBS_DC, linetype = "dashed", linewidth = 0.8)} +
    {if(!is.na(Sta_UBS_DC))  geom_hline(yintercept = Sta_UBS_DC, linetype = "dashed", linewidth = 0.8)} +
    {if(!is.na(Pres_UBS_DC)) annotate("point", x = Pres_UBS_DC, y = Th0, size = 2)} +
    {if(!is.na(Pres_UBS_DC) && !is.na(Sta_UBS_DC)) annotate("point", x = Pres_UBS_DC, y = Sta_UBS_DC, size = 2)} +
    {if(!is.na(Pres_UBS_DC) && !is.na(Sta_UBS_DC)) annotate("text",
                                                            x = Pres_UBS_DC, y = label_y(Sta_UBS_DC),
                                                            label = paste0("P=", round(Pres_UBS_DC, 3), "\nS=", round(Sta_UBS_DC, 3)),
                                                            hjust = -0.1, vjust = 1, size = 3)} +
    {if(!is.na(Pres_UBS_DC)) annotate("text",
                                      x = Pres_UBS_DC, y = label_y(Th0, offset = 0.08),
                                      label = paste0("Ref S (P=0, lower CI)\n", round(Th0, 3)),
                                      hjust = -0.1, vjust = 1, size = 3)} +
    {if(is.na(Pres_UBS_DC)) annotate("text", x = 6, y = 0.95, label = "No crossing within 0-12", size = 3)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, 12)) +
    labs(x = "Pressure", y = "Richness (log1p, normalized to P=0)", title = "Detectable Change") +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Habitat = msfd_bht,
      Method = "Detectable change",
      Pressure_threshold = round(Pres_UBS_DC, 3),
      State_threshold = round(Sta_UBS_DC, 3)
    )
  )
  

  # ======================================================================
  # E) FINAL FIGURE PER HABITAT
  # ======================================================================
  final_plot <- ggarrange(p_nv, p_dc, ncol = 2)  
  final_plot <- annotate_figure(
    final_plot,
    top = text_grob(paste0("Habitat: ", msfd_bht), face = "bold", size = 14)
  )
  
  safe_bht <- gsub("[^A-Za-z0-9]+", "_", msfd_bht)
  out_plot_hab <- file.path(
    plots_folder,
    paste0(base_name, "_", safe_bht, "_PLOT_THRESHOLDS_RICHNESSLOG.png")
    )
  ggsave(
    filename = out_plot_hab,
    plot = final_plot,
    width = 14,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  cat("Plot saved to:\n", out_plot_hab, "\n")
}

# =========================
# 7) Export tables (ONCE) - 2 sheets
# =========================
write.xlsx(
  list(
    Thresholds = Thresholds_table,
    GAM_pvalues = GAM_pvalues
  ),
  file = thresholds_file,
  rowNames = FALSE,
  overwrite = TRUE
)

cat("\n============================================================\n")
cat("Excel saved to:\n", thresholds_file, "\n")
cat("Done.\n")

