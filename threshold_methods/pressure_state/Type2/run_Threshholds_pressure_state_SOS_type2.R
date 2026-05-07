# =======================================================================
# Pressure-state threshold estimation for Type 2 datasets: SoS
# Example application: Gotland dataset
# =======================================================================

# -----------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------
# This script estimates pressure-state thresholds for one Type 2 dataset
# and one response indicator at a time, using GAM-based pressure-state
# relationships fitted separately for each habitat.
#
# This script provides an example implementation using the Gotland dataset.
# The same workflow can be applied to other Type 2 datasets by modifying
# the input file and output paths accordingly.
#
# Threshold methods implemented:
#   - Natural Variation (NV), also referred to as Zero Pressure (ZP)
#   - Detectable Change (DC), also referred to as First Detectable Change (FDC)
#   - Distance to Degradation (D2D)
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
# ensure comparability across datasets and habitats.
#
# For SoS, the response variable is modelled using a binomial GAM with
# logit link, with SoS values bounded between 0 and 1.
#
# A slightly relaxed significance threshold (p < 0.06) is used to allow
# threshold estimation for habitats showing near-significant responses.
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
# This script is specific to the SoS indicator.
#
# The same workflow should be applied separately to other indicators
# (e.g. biomass, richness, Margalef), using indicator-appropriate
# model structures and response distributions.
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
# This script uses the SoS field only.
#
# -----------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------
# - mgcv
# - readxl
# - boot
# - dplyr
# - ggplot2
# - ggpubr
# - openxlsx


# -----------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------
library(mgcv)
library(readxl)
library(boot)
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
BOOT_R    <- 1000

# Example input file.
# Replace this path with the Data Type 2 Excel file to be analysed.
xlsx_file <- file.path(
  "../final_outputs/Type_2",
  "BS_gotland_btrawling.xlsx"
)

# Example output folder.
out_dir <- file.path(
  "../threshold_outputs/Type_2",
  "BS_gotland_btrawling",
  "SoS"
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_name <- tools::file_path_sans_ext(basename(xlsx_file))
out_table <- file.path(out_dir, paste0(base_name, "_TABLE_THRESHOLDS_SOS.xlsx"))

# =========================
# HELPERS
# =========================
num_clean <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
label_y <- function(y, offset = 0.05) pmax(0.02, pmin(0.98, y - offset))

find_col <- function(nms, pattern) {
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

export_now <- function(thr_tbl, pval_tbl) {
  write.xlsx(
    list(Thresholds = thr_tbl, GAM_pvalues = pval_tbl),
    file = out_table,
    rowNames = FALSE,
    overwrite = TRUE
  )
  cat("Excel saved to:\n", out_table, "\n")
}

# =========================
# OUTPUT TABLES
# =========================
Thresholds_table <- data.frame(
  Dataset = character(),
  Method = character(),
  Pressure_threshold = numeric(),
  State_threshold = numeric(),
  n = integer(),
  Small_n_warning = logical(),
  stringsAsFactors = FALSE
)

GAM_pvalues <- data.frame(
  Dataset = character(),
  n = integer(),
  edf = numeric(),
  ref_df = numeric(),
  p_smooth = numeric(),
  Small_n_warning = logical(),
  prop_below_baseline_highP = numeric(),
  stringsAsFactors = FALSE
)

# =========================
# READ + CLEAN
# =========================
cat("Reading:", xlsx_file, "\n")
if (!file.exists(xlsx_file)) stop("File not found: ", xlsx_file)

station_info <- read_excel(xlsx_file, sheet = "Station information")
nms <- names(station_info)

col_p   <- find_col(nms, "^pressure_value$")
col_sos <- find_col(nms, "^sos_2026$|^sos$|sentinel.*sos|sos")

if (is.na(col_p))   stop("Could not find pressure column (pressure_value). Check names(station_info).")
if (is.na(col_sos)) stop("Could not find SoS column (SoS_2026 / SoS...). Check names(station_info).")

station_info$pressure_value <- num_clean(station_info[[col_p]])
station_info$SoS_2026       <- num_clean(station_info[[col_sos]])

station_info <- station_info %>%
  filter(is.finite(pressure_value), is.finite(SoS_2026)) %>%
  filter(pressure_value <= P_MAX) %>%
  mutate(SoS_2026 = pmin(pmax(SoS_2026, 0), 1))  # safety clamp

msfd_data <- station_info %>%
  filter(is.finite(SoS_2026), is.finite(pressure_value))

n_obs <- nrow(msfd_data)
small_n_warning <- n_obs < SMALL_N

cat("\n============================================================\n")
cat("Dataset:", base_name, "\n")
cat("n =", n_obs, " | Small_n_warning =", small_n_warning, "\n")

if (n_obs == 0) {
  cat("No data after cleaning. Exporting empty thresholds + pvalues.\n")
  GAM_pvalues <- rbind(
    GAM_pvalues,
    data.frame(
      Dataset = base_name, n = 0, edf = NA, ref_df = NA, p_smooth = NA,
      Small_n_warning = TRUE, prop_below_baseline_highP = NA
    )
  )
  export_now(Thresholds_table, GAM_pvalues)
  stop("Stopping: no data.")
}

# =========================
# PRESSURE GRIDS
# =========================
nd <- data.frame(pressure_value = seq(0, P_MAX, by = 0.001))
i0_nd <- which.min(abs(nd$pressure_value - 0))

# =========================
# FIT PRELIM GAM (used for gates + downstream if passes)
# =========================
tmp_gam <- tryCatch(
  gam(SoS_2026 ~ s(pressure_value, k = 3),
      data = msfd_data, family = binomial(), method = "REML"),
  error = function(e) {
    cat("GAM failed:\n", e$message, "\n")
    return(NULL)
  }
)

if (is.null(tmp_gam)) {
  GAM_pvalues <- rbind(
    GAM_pvalues,
    data.frame(
      Dataset = base_name, n = n_obs, edf = NA, ref_df = NA, p_smooth = NA,
      Small_n_warning = small_n_warning, prop_below_baseline_highP = NA
    )
  )
  export_now(Thresholds_table, GAM_pvalues)
  stop("Stopping: GAM failed.")
}

# =========================
# GATE A: Degradation rule (>=50% below baseline at high P)
# =========================
nd_tmp <- data.frame(pressure_value = seq(0, P_MAX, by = 0.01))
pred_tmp <- predict(tmp_gam, newdata = nd_tmp, type = "response")
df_tmp <- data.frame(x = nd_tmp$pressure_value, y = pred_tmp)
df_high <- df_tmp[df_tmp$x >= P_LOW_MAX, ]

y0 <- df_tmp$y[which.min(abs(df_tmp$x - 0))]
prop_below <- mean(df_high$y < y0, na.rm = TRUE)

# =========================
# GATE B: Significance p-value
# =========================
smry_tmp <- summary(tmp_gam)
p_smooth <- suppressWarnings(smry_tmp$s.table[1, "p-value"])
edf      <- suppressWarnings(smry_tmp$s.table[1, "edf"])
ref_df   <- suppressWarnings(smry_tmp$s.table[1, "Ref.df"])

cat("High-P rule: prop_below_baseline (P>=0.65) =", round(prop_below, 3), " | baseline y0 =", round(y0, 4), "\n")
cat("Smooth p-value =", p_smooth, " | edf =", edf, " | Ref.df =", ref_df, "\n")

# always store pvalues
GAM_pvalues <- rbind(
  GAM_pvalues,
  data.frame(
    Dataset = base_name,
    n = n_obs,
    edf = edf,
    ref_df = ref_df,
    p_smooth = p_smooth,
    Small_n_warning = small_n_warning,
    prop_below_baseline_highP = prop_below
  )
)

# =========================
# HARD STOP: if gates fail -> export ONLY pvalues and STOP
# =========================
gate_ok <- is.finite(prop_below) && prop_below >= 0.50 &&
  is.finite(p_smooth) && !is.na(p_smooth) && p_smooth < P_SIGNIF

if (!gate_ok) {
  cat("\nGATES FAILED -> NO THRESHOLDS COMPUTED.\n")
  if (!is.finite(prop_below) || prop_below < 0.50) cat(" - Failed degradation rule (prop_below < 0.50)\n")
  if (is.na(p_smooth) || !is.finite(p_smooth) || p_smooth >= P_SIGNIF) cat(" - Failed significance rule (p >= ", P_SIGNIF, ")\n", sep="")
  export_now(Thresholds_table, GAM_pvalues)
  cat("Done. No thresholds derived because gates failed.\n")
} else {
  
  cat("\nGATES PASSED -> computing thresholds + plot.\n")
  
  # Use SAME model downstream (consistency)
  Mymodel <- tmp_gam
  
  # ======================================================================
  # A) Prediction curve + 95% CI (NV & DC) on link scale
  # ======================================================================
  pr <- predict(Mymodel, newdata = nd, type = "link", se.fit = TRUE)
  eta <- pr$fit
  se  <- pr$se.fit
  inv <- Mymodel$family$linkinv
  
  fit   <- inv(eta)
  upper <- inv(eta + 1.96 * se)
  lower <- inv(eta - 1.96 * se)
  
  # normalize by fitted value at P=0
  m0 <- fit[i0_nd]
  if (!is.finite(m0) || m0 == 0) m0 <- max(fit, na.rm = TRUE)
  
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
  
  Sta_NV <- df$lower[i0]
  thr <- Sta_NV
  
  idx <- if (df$y[1] >= thr) which(df$y <= thr)[1] else which(df$y >= thr)[1]
  
  Pres_NV <- NA
  if (!is.na(idx)) {
    if (idx == 1) Pres_NV <- df$x[1]
    else {
      x1 <- df$x[idx - 1]; x2 <- df$x[idx]
      y1 <- df$y[idx - 1]; y2 <- df$y[idx]
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
    {if(!is.na(Pres_NV)) annotate("text",
                                  x = Pres_NV, y = label_y(Sta_NV),
                                  label = paste0("P=", round(Pres_NV, 3), "\nS=", round(Sta_NV, 3)),
                                  hjust = -0.1, vjust = 1, size = 3)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, P_MAX)) +
    labs(x = "Pressure", y = "SoS (normalized to P=0)", title = "Natural Variation") +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Dataset = base_name,
      Method = "Natural variation",
      Pressure_threshold = round(Pres_NV, 3),
      State_threshold = round(Sta_NV, 3),
      n = n_obs,
      Small_n_warning = small_n_warning
    )
  )
  
  # =========================
  # C) DETECTABLE CHANGE (DC)
  # =========================
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
    {if(!is.na(Pres_DC) && !is.na(Sta_DC)) annotate("text",
                                                    x = Pres_DC, y = label_y(Sta_DC),
                                                    label = paste0("P=", round(Pres_DC, 3), "\nS=", round(Sta_DC, 3)),
                                                    hjust = -0.1, vjust = 1, size = 3)} +
    {if(!is.na(Pres_DC)) annotate("text",
                                  x = Pres_DC, y = label_y(Th0, offset = 0.08),
                                  label = paste0("Ref S (P=0, lower CI)\n", round(Th0, 3)),
                                  hjust = -0.1, vjust = 1, size = 3)} +
    {if(is.na(Pres_DC)) annotate("text", x = P_MAX/2, y = 0.95, label = "No crossing within 0-12", size = 3)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, P_MAX)) +
    labs(x = "Pressure", y = "SoS (normalized to P=0)", title = "Detectable Change") +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Dataset = base_name,
      Method = "Detectable change",
      Pressure_threshold = round(Pres_DC, 3),
      State_threshold = round(Sta_DC, 3),
      n = n_obs,
      Small_n_warning = small_n_warning
    )
  )
  
  # =========================
  # D) DISTANCE TO DEGRADATION (D2D)
  # =========================
  SensBySoS <- function(data, i){
    d2 <- data[i, ]
    names(d2) <- c("Pressure", "SoS")
    d2$Pres_W <- d2$Pressure
    
    SensGam <- gam(SoS ~ s(Pres_W, k = 3),
                   data = d2, family = binomial(), method = "REML")
    
    d3 <- d2
    d3$Pres_W <- seq(0, P_MAX, length.out = nrow(d2))
    Pred <- predict.gam(SensGam, d3, type = "response")
    
    m0b <- Pred[1]
    if (!is.finite(m0b) || m0b == 0) m0b <- max(Pred, na.rm = TRUE)
    d3$Pred <- Pred / m0b
    
    SoS_Cut <- min(d3$Pred, na.rm = TRUE)
    Pres_cut <- unique(d3[d3$Pred == SoS_Cut, ]$Pres_W)
    if (length(Pres_cut) > 0 && is.finite(Pres_cut[1])) {
      Pres_cut <- Pres_cut[1]
      d4 <- d3[d3$Pres_W < Pres_cut, ]
      d5 <- d3[d3$Pres_W >= Pres_cut, ]
      d5$Pred <- ifelse(d5$Pred > SoS_Cut, SoS_Cut, d5$Pred)
      d3 <- rbind.data.frame(d4, d5)
    }
    
    x <- d3$Pres_W
    d3$Sens_1 <- 1 - rev((max((x^(1)))  - (x^(1)))  / (max((x^(1))))  * 0.4)
    d3$Sens_2 <- 1 - rev((max((x^(1)))  - (x^(1)))  / (max((x^(1))))  * 0.7)
    d3$Sens_3 <- 1 - rev((max((x^(2)))  - (x^(2)))  / (max((x^(2))))  * 0.82)
    d3$Sens_4 <- 1 - rev((max((x^(4)))  - (x^(4)))  / (max((x^(4))))  * 0.95)
    d3$Sens_5 <- 1 - rev((max((x^(80))) - (x^(80))) /  max((x^(80))))
    
    AllSumSqr <- c(
      sum((d3$Sens_1 - d3$Pred)^2, na.rm = TRUE),
      sum((d3$Sens_2 - d3$Pred)^2, na.rm = TRUE),
      sum((d3$Sens_3 - d3$Pred)^2, na.rm = TRUE),
      sum((d3$Sens_4 - d3$Pred)^2, na.rm = TRUE),
      sum((d3$Sens_5 - d3$Pred)^2, na.rm = TRUE)
    )
    as.numeric(match(min(AllSumSqr), AllSumSqr))
  }
  
  DataForBoot <- msfd_data[, c("pressure_value", "SoS_2026")]
  
  bootcorr <- tryCatch(
    boot(DataForBoot, SensBySoS, R = BOOT_R),
    error = function(e){
      cat("Bootstrap failed.\nReason:", e$message, "\n")
      return(NULL)
    }
  )
  
  Sta_D2D <- NA
  Pres_D2D <- NA
  
  upper_se <- inv(eta + se) / m0
  lower_se <- inv(eta - se) / m0
  
  NewData_D2D <- data.frame(
    x = nd$pressure_value,
    y = fit / m0,
    se_lower = lower_se,
    se_upper = upper_se
  )
  
  p_d2d <- ggplot(NewData_D2D, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = se_lower, ymax = se_upper), alpha = 0.18) +
    geom_line(linewidth = 1) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, P_MAX)) +
    labs(x = "Pressure", y = "SoS (normalized to P=0)", title = "Distance to degradation (bootstrap failed)") +
    theme_minimal()
  
  if (!is.null(bootcorr)) {
    
    Sens_mean <- mean(bootcorr$t, na.rm = TRUE)
    Sens <- round(Sens_mean)
    cat("Sensitivity (bootstrap mean rounded):", Sens, "\n")
    
    d_final <- dplyr::case_when(
      Sens == 4 ~ 0.25,
      Sens == 3 ~ 0.50,
      Sens %in% c(1,2) ~ 0.75,
      TRUE ~ 0.75
    )
    d_values <- c(0.25, 0.50, 0.75)
    
    ThData <- NewData_D2D %>% transmute(Pressure = x, QualityLoss = 1 - y)
    
    slope_target <- 1 / P_MAX
    spl <- smooth.spline(ThData$Pressure, ThData$QualityLoss, spar = 0.3, tol = 0.001)
    x_vec <- seq(0, P_MAX, by = 0.001)
    slope <- predict(spl, x = x_vec, deriv = 1)$y
    q_vec <- predict(spl, x = x_vec, deriv = 0)$y
    
    slope_sd   <- sd(slope, na.rm = TRUE)
    slope_mean <- mean(abs(slope), na.rm = TRUE)
    
    linear_like <- is.finite(slope_sd) && is.finite(slope_mean) && slope_mean > 0 &&
      (slope_sd < 0.30 * slope_mean)
    
    cat("D2D linear_like =", linear_like, "\n")
    
    if (!linear_like) {
      idx_tip <- which.min(abs(slope - slope_target))
      PressureTipping <- x_vec[idx_tip]
      QualityTipping  <- q_vec[idx_tip]
      StateTipping <- 1 - QualityTipping
    } else {
      PressureTipping <- NA
      QualityTipping  <- NA
      StateTipping <- NA
    }
    
    if (!is.na(PressureTipping) && is.finite(PressureTipping)) {
      
      Th_table <- lapply(d_values, function(d){
        TH_state <- 1 - d * QualityTipping
        
        df2 <- NewData_D2D %>%
          filter(x <= PressureTipping) %>%
          arrange(x) %>%
          dplyr::select(x, y)
        
        idx2 <- if (df2$y[1] >= TH_state) which(df2$y <= TH_state)[1] else which(df2$y >= TH_state)[1]
        
        TH_pressure <- NA
        if (!is.na(idx2)) {
          if (idx2 == 1) TH_pressure <- df2$x[1]
          else {
            x1 <- df2$x[idx2 - 1]; x2 <- df2$x[idx2]
            y1 <- df2$y[idx2 - 1]; y2 <- df2$y[idx2]
            TH_pressure <- x1 + (TH_state - y1) * (x2 - x1) / (y2 - y1)
          }
        }
        data.frame(d = d, State_threshold = TH_state, Pressure_threshold = TH_pressure)
      }) %>% bind_rows()
      
      Th_final <- Th_table[which.min(abs(Th_table$d - d_final)), ]
      Sta_D2D <- Th_final$State_threshold
      Pres_D2D <- Th_final$Pressure_threshold
      
      cat("D2D threshold (selected): P =", round(Pres_D2D, 3), " S =", round(Sta_D2D, 3), "\n")
    } else {
      cat("D2D: no unique tipping point (curve ~ linear). Skipping D2D threshold.\n")
    }
    
    p_d2d <- ggplot(NewData_D2D, aes(x = x, y = y)) +
      geom_ribbon(aes(ymin = se_lower, ymax = se_upper), alpha = 0.18) +
      geom_line(linewidth = 1) +
      {if(!is.na(Sta_D2D)) geom_hline(yintercept = Sta_D2D, linetype = "dashed", linewidth = 0.8)} +
      {if(!is.na(Pres_D2D)) geom_vline(xintercept = Pres_D2D, linetype = "dashed", linewidth = 0.8)} +
      {if(!is.na(Pres_D2D) && !is.na(Sta_D2D)) annotate("point", x = Pres_D2D, y = Sta_D2D, shape = 17, size = 2)} +
      {if(!is.na(Pres_D2D) && !is.na(Sta_D2D)) annotate("text",
                                                        x = Pres_D2D, y = label_y(Sta_D2D),
                                                        label = paste0("P=", round(Pres_D2D, 3), "\nS=", round(Sta_D2D, 3)),
                                                        hjust = -0.1, vjust = 1, size = 3)} +
      coord_cartesian(ylim = c(0, 1)) +
      scale_x_continuous(limits = c(0, P_MAX)) +
      labs(x = "Pressure", y = "SoS (normalized to P=0)", title = "Distance to degradation") +
      theme_minimal()
    
    Thresholds_table <- rbind(
      Thresholds_table,
      data.frame(
        Dataset = base_name,
        Method = "Distance to degradation",
        Pressure_threshold = round(Pres_D2D, 3),
        State_threshold = round(Sta_D2D, 3),
        n = n_obs,
        Small_n_warning = small_n_warning
      )
    )
  }
  
  # =========================
  # FINAL FIGURE (3 panels) + SAVE
  # =========================
  final_plot <- ggarrange(p_d2d, p_nv, p_dc, ncol = 3)
  
  title_txt <- paste0(
    "Dataset: ", base_name,
    " | n=", n_obs,
    if (small_n_warning) " (WARNING: small n)" else ""
  )
  
  final_plot <- annotate_figure(final_plot, top = text_grob(title_txt, face = "bold", size = 12))
  
  out_plot <- file.path(out_dir, paste0(base_name, "_PLOT_THRESHOLDS_SOS.png"))
  ggsave(filename = out_plot, plot = final_plot, width = 14, height = 5, dpi = 300, bg = "white")
  cat("Plot saved to:\n", out_plot, "\n")
  
  # =========================
  # EXPORT EXCEL (2 sheets)
  # =========================
  export_now(Thresholds_table, GAM_pvalues)
  
  cat("\n============================================================\n")
  cat("Done. Thresholds derived because both gates were passed.\n")
  
} # end gate_ok block