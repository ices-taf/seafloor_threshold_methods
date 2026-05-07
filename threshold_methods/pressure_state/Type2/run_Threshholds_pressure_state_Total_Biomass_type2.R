# =======================================================================
# Pressure-state threshold estimation for Type 2 datasets: Total Biomass
# Example application: Gotland dataset
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script estimates pressure-state thresholds for one Type 2 dataset
# and one response indicator at a time, using GAM-based pressure-state
# relationships.
#
# This script provides an example implementation using the Gotland dataset.
# The same workflow can be applied to other Type 2 datasets by modifying
# the input file and output paths accordingly.
#
# Threshold methods implemented:
#   - Natural Variation (NV), also referred to as Zero Pressure (ZP)
#   - Detectable Change (DC), also referred to as First Detectable Change (FDC)
#
# The Distance to Degradation (D2D) method is NOT applied to this indicator.
# D2D is treated here as a method specific to SoS-type bounded indicators,
# and is therefore not used for continuous, unbounded indicators such as
# total biomass.
#
# The script reads the "Station information" sheet from one post-workshop
# harmonised Type 2 Excel file containing indicator values. Thresholds
# are computed only when the fitted pressure-state relationship meets
# the required criteria.
#
# Outputs include:
#   - a threshold table, only if both gating criteria are met
#   - one diagnostic threshold plot, only if both gating criteria are met
#   - GAM diagnostics (always exported)
#
# If the gating criteria are not met, no threshold values or threshold
# plots are produced for this indicator.
#
# -----------------------------------------------------------------------
# Gating criteria for threshold estimation (HARD RULE)
# -----------------------------------------------------------------------
# Thresholds are computed ONLY when both of the following conditions are met:
#
# 1. Degradation condition:
#    At least 50% of predicted values at moderate-to-high pressure
#    (P >= 0.65) are below the predicted baseline value at P = 0.
#
# 2. Statistical significance:
#    The smooth term of the GAM is significant at p < 0.06.
#
# If any of these conditions is not met:
#   - thresholds are NOT computed
#   - threshold plots are NOT produced
#   - the script stops without deriving threshold values
#   - only GAM diagnostics (p-values) are exported
#
# This ensures that thresholds are derived only from robust and
# ecologically meaningful pressure-state relationships.
#
# -----------------------------------------------------------------------
# Methodological notes
# -----------------------------------------------------------------------
# Thresholds are estimated independently for each dataset.
# Pressure-state relationships are modelled using GAMs, and predictions
# are standardised to the fitted value at zero pressure (P = 0).
#
# The pressure domain is restricted to a common range (SAR <= 12) to
# ensure comparability across bottom-trawling datasets.
#
# For total biomass, the response variable is transformed using log1p()
# and modelled using a Gaussian GAM. This transformation reduces
# right-skewness and improves variance stability while retaining zero
# values in the dataset.
#
# A slightly relaxed significance threshold (p < 0.06) is used to allow
# threshold estimation for datasets showing near-significant responses.
# However, results with p-values > 0.05 should be interpreted with caution.
#
# -----------------------------------------------------------------------
# Specific considerations for Type 2 datasets
# -----------------------------------------------------------------------
# Type 2 datasets typically have:
#   - limited sample size
#   - reduced pressure gradients
#   - fewer significant pressure-state relationships
#
# As a result:
#   - thresholds may not be computed for many datasets
#   - lack of thresholds does NOT imply absence of impact
#   - it reflects insufficient statistical support under current data
#
# This behaviour is expected and consistent with the applied framework.
#
# -----------------------------------------------------------------------
# Indicator-specific implementation
# -----------------------------------------------------------------------
# This script is specific to the total biomass indicator.
#
# The same workflow should be applied separately to other indicators
# (e.g. SoS, richness, Margalef), using indicator-appropriate model
# structures and response distributions.
#
# This ensures methodological consistency while respecting differences
# among indicators.
#
# -----------------------------------------------------------------------
# Input data
# -----------------------------------------------------------------------
# Input data correspond to post-workshop harmonised Type 2 datasets.
# These datasets include pressure and indicator information prepared
# for pressure-state modelling after the WKBENTH4 workshop.
#
# The "Station information" sheet may contain, depending on the dataset:
#   - SoS (Sentinel of the Seabed)
#   - Total biomass
#   - Richness
#   - Margalef diversity
#
# This script uses the total biomass field only.
#
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

# -----------------------------------------------------------------------
# Clear workspace
# -----------------------------------------------------------------------

rm(list = ls())
gc()

# -----------------------------------------------------------------------
# User settings
# -----------------------------------------------------------------------

# Pressure domain used for bottom-trawling datasets.
# For non-SAR pressure variables, review this value before running.
P_MAX     <- 12
P_LOW_MAX <- 0.65
P_SIGNIF  <- 0.06
SMALL_N   <- 15

# Example input file.
# Replace this path with the Type 2 Excel file to be analysed.
xlsx_file <- file.path(
  "../input/Type_2",
  "BS_gotland_btrawling.xlsx"
)

# Example output folder.
out_dir <- file.path(
  "../outputs/Type_2_thresholds",
  "BS_gotland_btrawling",
  "Total_biomass"
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_name <- tools::file_path_sans_ext(basename(xlsx_file))
out_table <- file.path(out_dir, paste0(base_name, "_TABLE_THRESHOLDS_TOTAL_BIOMASS.xlsx"))

# -----------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------

num_clean <- function(x){
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

label_y <- function(y, offset = 0.05){
  pmax(0.02, pmin(0.98, y - offset))
}

find_col <- function(nms, pattern){
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if(length(hit) == 0) return(NA_character_)
  hit[1]
}

export_now <- function(thr_tbl, pval_tbl){
  write.xlsx(
    list(
      Thresholds = thr_tbl,
      GAM_pvalues = pval_tbl
    ),
    file = out_table,
    rowNames = FALSE,
    overwrite = TRUE
  )
  cat("Excel saved to:\n", out_table, "\n")
}

# -----------------------------------------------------------------------
# Output tables
# -----------------------------------------------------------------------

Thresholds_table <- data.frame(
  Dataset = character(),
  Indicator = character(),
  Method = character(),
  Pressure_threshold = numeric(),
  State_threshold = numeric(),
  n = integer(),
  Small_n_warning = logical(),
  stringsAsFactors = FALSE
)

GAM_pvalues <- data.frame(
  Dataset = character(),
  Indicator = character(),
  n = integer(),
  edf = numeric(),
  ref_df = numeric(),
  p_smooth = numeric(),
  Small_n_warning = logical(),
  prop_below_baseline_highP = numeric(),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------
# Read and clean data
# -----------------------------------------------------------------------

cat("Reading:", xlsx_file, "\n")
if(!file.exists(xlsx_file)) stop("File not found: ", xlsx_file)

station_info <- read_excel(xlsx_file, sheet = "Station information")
nms <- names(station_info)

# Column detection is intentionally flexible because Type 2 files may
# contain slightly different naming conventions after harmonisation.
col_p <- find_col(nms, "^pressure_value$")
col_bio <- find_col(
  nms,
  "^total_biomass$|^total_biomass_2$|total.*biomass|biomass"
)

if(is.na(col_p)){
  stop("Could not find pressure column (expected pressure_value). Check names(station_info).")
}

if(is.na(col_bio)){
  stop("Could not find total biomass column. Check names(station_info).")
}

station_info$pressure_value <- num_clean(station_info[[col_p]])
station_info$total_biomass  <- num_clean(station_info[[col_bio]])
station_info$bio_log <- log1p(station_info$total_biomass)

station_info <- station_info %>%
  filter(is.finite(pressure_value), is.finite(bio_log)) %>%
  filter(pressure_value <= P_MAX)

msfd_data <- station_info %>%
  filter(is.finite(bio_log), is.finite(pressure_value))

n_obs <- nrow(msfd_data)
small_n_warning <- n_obs < SMALL_N

cat("\n============================================================\n")
cat("Dataset:", base_name, "\n")
cat("Indicator: Total biomass (log1p)\n")
cat("n =", n_obs, " | Small_n_warning =", small_n_warning, "\n")

if(n_obs == 0){
  cat("No data after cleaning. Exporting empty thresholds and p-values.\n")
  GAM_pvalues <- rbind(
    GAM_pvalues,
    data.frame(
      Dataset = base_name,
      Indicator = "Total biomass (log1p)",
      n = 0,
      edf = NA,
      ref_df = NA,
      p_smooth = NA,
      Small_n_warning = TRUE,
      prop_below_baseline_highP = NA
    )
  )
  export_now(Thresholds_table, GAM_pvalues)
  stop("Stopping: no data.")
}

# -----------------------------------------------------------------------
# Pressure grid
# -----------------------------------------------------------------------

nd <- data.frame(pressure_value = seq(0, P_MAX, by = 0.001))
i0_nd <- which.min(abs(nd$pressure_value - 0))

# -----------------------------------------------------------------------
# Fit preliminary GAM for gating criteria
# -----------------------------------------------------------------------

tmp_gam <- tryCatch(
  gam(
    bio_log ~ s(pressure_value, k = 3),
    data = msfd_data,
    family = gaussian(),
    method = "REML"
  ),
  error = function(e){
    cat("GAM failed:\n", e$message, "\n")
    return(NULL)
  }
)

if(is.null(tmp_gam)){
  GAM_pvalues <- rbind(
    GAM_pvalues,
    data.frame(
      Dataset = base_name,
      Indicator = "Total biomass (log1p)",
      n = n_obs,
      edf = NA,
      ref_df = NA,
      p_smooth = NA,
      Small_n_warning = small_n_warning,
      prop_below_baseline_highP = NA
    )
  )
  export_now(Thresholds_table, GAM_pvalues)
  stop("Stopping: GAM failed.")
}

# -----------------------------------------------------------------------
# Gate A: degradation rule
# -----------------------------------------------------------------------

nd_tmp <- data.frame(pressure_value = seq(0, P_MAX, by = 0.01))
pred_tmp <- predict(tmp_gam, newdata = nd_tmp, type = "response")
df_tmp <- data.frame(x = nd_tmp$pressure_value, y = pred_tmp)
df_high <- df_tmp[df_tmp$x >= P_LOW_MAX, ]

y0 <- df_tmp$y[which.min(abs(df_tmp$x - 0))]
prop_below <- mean(df_high$y < y0, na.rm = TRUE)

# -----------------------------------------------------------------------
# Gate B: GAM smooth significance
# -----------------------------------------------------------------------

smry_tmp <- summary(tmp_gam)
p_smooth <- suppressWarnings(smry_tmp$s.table[1, "p-value"])
edf      <- suppressWarnings(smry_tmp$s.table[1, "edf"])
ref_df   <- suppressWarnings(smry_tmp$s.table[1, "Ref.df"])

cat("High-P rule: prop_below_baseline (P >= 0.65) =",
    round(prop_below, 3),
    " | baseline y0 =", round(y0, 4), "\n")
cat("Smooth p-value =", p_smooth, " | edf =", edf, " | Ref.df =", ref_df, "\n")

GAM_pvalues <- rbind(
  GAM_pvalues,
  data.frame(
    Dataset = base_name,
    Indicator = "Total biomass (log1p)",
    n = n_obs,
    edf = edf,
    ref_df = ref_df,
    p_smooth = p_smooth,
    Small_n_warning = small_n_warning,
    prop_below_baseline_highP = prop_below
  )
)

gate_ok <- is.finite(prop_below) && prop_below >= 0.50 &&
  is.finite(p_smooth) && !is.na(p_smooth) && p_smooth < P_SIGNIF

if(!gate_ok){
  cat("\nGATES FAILED -> NO THRESHOLDS COMPUTED.\n")
  if(!is.finite(prop_below) || prop_below < 0.50){
    cat(" - Failed degradation rule (prop_below < 0.50)\n")
  }
  if(is.na(p_smooth) || !is.finite(p_smooth) || p_smooth >= P_SIGNIF){
    cat(" - Failed significance rule (p >= ", P_SIGNIF, ")\n", sep = "")
  }
  export_now(Thresholds_table, GAM_pvalues)
  cat("Done. No thresholds derived because gates failed.\n")
} else {
  
  cat("\nGATES PASSED -> computing thresholds and plot.\n")
  
  # Use the same model downstream for consistency.
  Mymodel <- tmp_gam
  
  # ---------------------------------------------------------------------
  # Prediction curve and 95% confidence interval
  # ---------------------------------------------------------------------
  
  pr <- predict(Mymodel, newdata = nd, type = "response", se.fit = TRUE)
  fit <- pr$fit
  se  <- pr$se.fit
  
  upper <- fit + 1.96 * se
  lower <- fit - 1.96 * se
  
  # Standardise by fitted value at P = 0.
  m0 <- fit[i0_nd]
  if(!is.finite(m0) || m0 == 0){
    m0 <- max(fit, na.rm = TRUE)
  }
  
  NewData_CI <- data.frame(
    x = nd$pressure_value,
    y = fit / m0,
    upper = upper / m0,
    lower = lower / m0
  )
  
  # ---------------------------------------------------------------------
  # Natural Variation (NV)
  # ---------------------------------------------------------------------
  
  df <- NewData_CI[order(NewData_CI$x), ]
  i0 <- which.min(abs(df$x - 0))
  
  Sta_NV <- df$lower[i0]
  thr <- Sta_NV
  
  idx <- if(df$y[1] >= thr){
    which(df$y <= thr)[1]
  } else {
    which(df$y >= thr)[1]
  }
  
  Pres_NV <- NA
  if(!is.na(idx)){
    if(idx == 1){
      Pres_NV <- df$x[1]
    } else {
      x1 <- df$x[idx - 1]
      x2 <- df$x[idx]
      y1 <- df$y[idx - 1]
      y2 <- df$y[idx]
      Pres_NV <- x1 + (thr - y1) * (x2 - x1) / (y2 - y1)
    }
  }
  
  cat("NV threshold: P =", round(Pres_NV, 3), " S =", round(Sta_NV, 3), "\n")
  
  p_nv <- ggplot(NewData_CI, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    geom_line(linewidth = 1) +
    {if(!is.na(Pres_NV)) geom_vline(xintercept = Pres_NV, linetype = "dashed", linewidth = 0.8)} +
    geom_hline(yintercept = Sta_NV, linetype = "dashed", linewidth = 0.8) +
    {if(!is.na(Pres_NV)) annotate("point", x = Pres_NV, y = Sta_NV, size = 2)} +
    {if(!is.na(Pres_NV)) annotate(
      "text",
      x = Pres_NV,
      y = label_y(Sta_NV),
      label = paste0("P=", round(Pres_NV, 3), "\nS=", round(Sta_NV, 3)),
      hjust = -0.1,
      vjust = 1,
      size = 3
    )} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, P_MAX)) +
    labs(
      x = "Pressure",
      y = "Total biomass (log1p, normalised to P=0)",
      title = "Natural Variation"
    ) +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Dataset = base_name,
      Indicator = "Total biomass (log1p)",
      Method = "Natural variation",
      Pressure_threshold = round(Pres_NV, 3),
      State_threshold = round(Sta_NV, 3),
      n = n_obs,
      Small_n_warning = small_n_warning
    )
  )
  
  # ---------------------------------------------------------------------
  # Detectable Change (DC)
  # ---------------------------------------------------------------------
  
  Th0 <- NewData_CI$lower[i0]
  
  idx_dc <- which(NewData_CI$upper <= Th0)[1]
  Pres_DC <- if(!is.na(idx_dc)) NewData_CI$x[idx_dc] else NA
  Sta_DC  <- if(!is.na(idx_dc)) NewData_CI$y[idx_dc] else NA
  
  cat("DC threshold: P =", round(Pres_DC, 3),
      " S_final =", round(Sta_DC, 3),
      " (S_ref@P0_lowerCI =", round(Th0, 3), ")\n")
  
  p_dc <- ggplot(NewData_CI, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = Th0, linetype = "dotted", linewidth = 0.4) +
    {if(!is.na(Pres_DC)) geom_vline(xintercept = Pres_DC, linetype = "dashed", linewidth = 0.8)} +
    {if(!is.na(Sta_DC))  geom_hline(yintercept = Sta_DC, linetype = "dashed", linewidth = 0.8)} +
    {if(!is.na(Pres_DC)) annotate("point", x = Pres_DC, y = Th0, size = 2)} +
    {if(!is.na(Pres_DC) && !is.na(Sta_DC)) annotate("point", x = Pres_DC, y = Sta_DC, size = 2)} +
    {if(!is.na(Pres_DC) && !is.na(Sta_DC)) annotate(
      "text",
      x = Pres_DC,
      y = label_y(Sta_DC),
      label = paste0("P=", round(Pres_DC, 3), "\nS=", round(Sta_DC, 3)),
      hjust = -0.1,
      vjust = 1,
      size = 3
    )} +
    {if(!is.na(Pres_DC)) annotate(
      "text",
      x = Pres_DC,
      y = label_y(Th0, offset = 0.08),
      label = paste0("Ref S (P=0, lower CI)\n", round(Th0, 3)),
      hjust = -0.1,
      vjust = 1,
      size = 3
    )} +
    {if(is.na(Pres_DC)) annotate(
      "text",
      x = P_MAX / 2,
      y = 0.95,
      label = "No crossing within 0-12",
      size = 3
    )} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, P_MAX)) +
    labs(
      x = "Pressure",
      y = "Total biomass (log1p, normalised to P=0)",
      title = "Detectable Change"
    ) +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Dataset = base_name,
      Indicator = "Total biomass (log1p)",
      Method = "Detectable change",
      Pressure_threshold = round(Pres_DC, 3),
      State_threshold = round(Sta_DC, 3),
      n = n_obs,
      Small_n_warning = small_n_warning
    )
  )
  
  # ---------------------------------------------------------------------
  # Final figure and export
  # ---------------------------------------------------------------------
  
  final_plot <- ggarrange(p_nv, p_dc, ncol = 2)
  
  title_txt <- paste0(
    "Dataset: ", base_name,
    " | Indicator: Total biomass (log1p)",
    " | n=", n_obs,
    if(small_n_warning) " (WARNING: small n)" else ""
  )
  
  final_plot <- annotate_figure(
    final_plot,
    top = text_grob(title_txt, face = "bold", size = 12)
  )
  
  out_plot <- file.path(out_dir, paste0(base_name, "_PLOT_THRESHOLDS_TOTAL_BIOMASS.png"))
  ggsave(
    filename = out_plot,
    plot = final_plot,
    width = 10,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  cat("Plot saved to:\n", out_plot, "\n")
  
  export_now(Thresholds_table, GAM_pvalues)
  
  cat("\n============================================================\n")
  cat("Done. Thresholds derived because both gates were passed.\n")
  
} # end gate_ok block
