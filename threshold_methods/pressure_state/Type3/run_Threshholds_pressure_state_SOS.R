# =======================================================================
# Pressure-state threshold estimation for Type 3 datasets: SoS
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
#   - Distance to Degradation (D2D)
#
# The script reads the "Station information" sheet from one post-workshop
# harmonised Type 3 Excel file containing indicator values, applies data
# cleaning and habitat-level filtering, fits a GAM for each habitat, and
# computes thresholds only when the fitted pressure-state relationship
# meets the required criteria.
#
# Outputs include:
#   - a table of thresholds for retained habitats
#   - one diagnostic plot per habitat showing NV, DC and D2D results

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
# For SoS, the response variable is modelled using a binomial GAM with
# logit link, with SoS values bounded between 0 and 1.
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
# This script uses the SoS field only.

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
  "SoS"
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
  paste0(dataset_name, "_SoS_thresholds.xlsx")
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
# 3) Clean core fields (IMPORTANT)
# =========================
station_info$pressure_value <- as.numeric(gsub(",", ".", station_info$pressure_value))
station_info$SoS_2026 <- as.numeric(station_info$SoS_2026)

station_info <- station_info %>%
  filter(is.finite(pressure_value), is.finite(SoS_2026)) %>%
  filter(pressure_value <= 12)

# Pressure grid for smooth predictions
nd <- data.frame(pressure_value = seq(0, 12, by = 0.001))

# =========================
# 4) Sensitivity bootstrap function
#    (normalised to P = 0)
# =========================
SensBySoS <- function(data, i){
  d2 <- data[i,]
  names(d2) <- c("Pressure", "SoS")
  d2$Pres_W <- d2$Pressure
  SensGam <- gam(SoS ~ s(Pres_W, k=3), data=d2, family=binomial)
  
  d3 <- d2
  d3$Pres_W <- seq(0,12,length.out = nrow(d2))
  Pred_SensGam <- predict.gam(SensGam, d3, type="response")
  
  # Normalize by predicted value at P=0 (first element of the grid)
  m0 <- Pred_SensGam[1]
  if (!is.finite(m0) || m0 == 0) m0 <- max(Pred_SensGam, na.rm = TRUE)  # fallback
  d3$Pred <- Pred_SensGam / m0
  
  # Prevent the fitted response from increasing after the minimum predicted
  # value is reached
  SoS_Cut <- min(d3$Pred, na.rm = TRUE)
  Pres_cut <- unique(d3[d3$Pred==SoS_Cut,]$Pres_W)
  d4 <- d3[d3$Pres_W<Pres_cut,]
  d5 <- d3[d3$Pres_W>=Pres_cut,]
  d5$Pred <- ifelse(d5$Pred>SoS_Cut, SoS_Cut, d5$Pred)
  d3 <- rbind.data.frame(d4,d5)
  
  x <- d3$Pres_W
  d3$Sens_1 <- 1-rev((max((x^(1)))-(x^(1)))/(max((x^(1))))*0.4)
  d3$Sens_2 <- 1-rev((max((x^(1)))-(x^(1)))/(max((x^(1))))*0.7)
  d3$Sens_3 <- 1-rev((max((x^(2)))-(x^(2)))/(max((x^(2))))*0.82)
  d3$Sens_4 <- 1-rev((max((x^(4)))-(x^(4)))/(max((x^(4))))*0.95)
  d3$Sens_5 <- 1-rev((max((x^(80)))-(x^(80)))/max((x^(80))))
  
  SumSqr_1 <- sum((d3$Sens_1- d3$Pred)^2, na.rm = TRUE)
  SumSqr_2 <- sum((d3$Sens_2- d3$Pred)^2, na.rm = TRUE)
  SumSqr_3 <- sum((d3$Sens_3- d3$Pred)^2, na.rm = TRUE)
  SumSqr_4 <- sum((d3$Sens_4- d3$Pred)^2, na.rm = TRUE)
  SumSqr_5 <- sum((d3$Sens_5- d3$Pred)^2, na.rm = TRUE)
  
  AllSumSqr <- c(SumSqr_1, SumSqr_2, SumSqr_3, SumSqr_4, SumSqr_5)
  Sens <- as.numeric(match(min(AllSumSqr),AllSumSqr))
  return(Sens)
}

# =========================
# 5) Table to store thresholds
# =========================
Thresholds_table <- data.frame(
  Habitat = character(),
  Method = character(),
  Pressure_threshold = numeric(),
  State_threshold = numeric(),
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
    filter(is.finite(SoS_2026), is.finite(pressure_value))
  
  if (nrow(msfd_data) == 0) {
    cat("No data. Skipping habitat.\n")
    next
  }
  
  # Fit GAM
  Mymodel_UBS <- tryCatch(
    gam(SoS_2026 ~ s(pressure_value, k=3),
        data = msfd_data,
        family = binomial(),
        method = "REML"),
    error = function(e){
      cat("GAM failed for habitat:", msfd_bht, "\nReason:", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(Mymodel_UBS)) next
  
  # Use the same model downstream
  smry <- summary(Mymodel_UBS)
  print(smry)
  
  p_smooth <- suppressWarnings(smry$s.table[1, "p-value"])
  
  if (is.na(p_smooth) || p_smooth >= 0.06) {
    cat("Smooth term NOT significant (p =", p_smooth, "). Skipping thresholds/plot.\n")
    next
  }
  
  cat("Smooth term SIGNIFICANT (p =", p_smooth, "). Computing thresholds...\n")

  # ======================================================================
  # A) Prediction curve + 95% CI (NV & DC) using link scale
  # ======================================================================
  pr <- predict(Mymodel_UBS, newdata = nd, type = "link", se.fit = TRUE)
  eta <- pr$fit
  se  <- pr$se.fit
  inv <- Mymodel_UBS$family$linkinv
  
  fit   <- inv(eta)
  upper <- inv(eta + 1.96 * se)
  lower <- inv(eta - 1.96 * se)
  
  # --- NEW: normalize by fitted value at P=0 ---
  i0_nd <- which.min(abs(nd$pressure_value - 0))
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
  
  if (df$y[1] >= thr) {
    idx <- which(df$y <= thr)[1]
  } else {
    idx <- which(df$y >= thr)[1]
  }
  
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
    {if(!is.na(Pres_UBS_Nat)) annotate("text", x = Pres_UBS_Nat, y = Sta_UBS_Nat,
                                       label = paste0("P=", round(Pres_UBS_Nat, 3), "\nS=", round(Sta_UBS_Nat, 3)),
                                       hjust = -0.1, vjust = -0.5)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, 12)) +
    labs(x = "Pressure", y = "Sentinel species (normalized to P=0)", title = "Natural Variation") +
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
  Th0 <- NewData_CI$lower[i0]  # reference state at P=0 (lower CI)
  
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
    {if(!is.na(Pres_UBS_DC) && !is.na(Sta_UBS_DC)) annotate("text", x = Pres_UBS_DC, y = Sta_UBS_DC,
                                                            label = paste0("P=", round(Pres_UBS_DC, 3), "\nS=", round(Sta_UBS_DC, 3)),
                                                            hjust = -0.1, vjust = -0.5)} +
    {if(!is.na(Pres_UBS_DC)) annotate("text", x = Pres_UBS_DC, y = Th0,
                                      label = paste0("Ref S (P=0, lower CI)\n", round(Th0, 3)),
                                      hjust = -0.1, vjust = 1.2, size = 3)} +
    {if(is.na(Pres_UBS_DC)) annotate("text", x = 6, y = 0.95, label = "No crossing within 0-12", size = 3)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, 12)) +
    labs(x = "Pressure", y = "Sentinel species (normalized to P=0)", title = "Detectable Change") +
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
  
  # =========================
  # D) DISTANCE TO DEGRADATION (D2D)
  # =========================
  DataForBoot_UBS <- msfd_data[, c("pressure_value", "SoS_2026")]
  
  bootcorr_UBS <- tryCatch(
    boot(DataForBoot_UBS, SensBySoS, R = 1000),
    error = function(e){
      cat("Bootstrap failed for habitat:", msfd_bht, "\nReason:", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(bootcorr_UBS)) next
  
  Sens_mean <- mean(bootcorr_UBS$t, na.rm = TRUE)
  Sens <- round(Sens_mean)
  cat("Sensitivity (bootstrap mode):", Sens, "\n")
  
  d_final <- case_when(
    Sens == 4 ~ 0.25,
    Sens == 3 ~ 0.50,
    Sens %in% c(1,2) ~ 0.75,
    TRUE ~ 0.75
  )
  
  d_values <- c(0.25, 0.50, 0.75)
  
  # ±1 SE for plotting only (normalize by P=0 baseline too)
  upper_se <- inv(eta + se) / m0
  lower_se <- inv(eta - se) / m0
  
  NewData_D2D <- data.frame(
    x = nd$pressure_value,
    y = fit / m0,
    se_lower = lower_se,
    se_upper = upper_se
  )
  
  ThData <- NewData_D2D %>%
    transmute(Pressure = x, QualityLoss = 1 - y)
  
  slope_target <- 1/12
  
  spl <- smooth.spline(ThData$Pressure, ThData$QualityLoss, spar = 0.3, tol = 0.001)
  x_vec <- seq(0, 12, by = 0.001)
  slope <- predict(spl, x = x_vec, deriv = 1)$y
  q_vec <- predict(spl, x = x_vec, deriv = 0)$y
  
  cat("slope range:", range(slope, na.rm=TRUE), "\n")
  cat("target:", 1/12, "\n")
  
  smin <- min(slope, na.rm = TRUE)
  smax <- max(slope, na.rm = TRUE)
  
  if (!is.finite(smin) || !is.finite(smax) || slope_target < smin || slope_target > smax) {
    PressureTipping <- NA
    QualityTipping  <- NA
  } else {
    idx_tip <- which.min(abs(slope - slope_target))
    PressureTipping <- x_vec[idx_tip]
    QualityTipping  <- q_vec[idx_tip]
  }
  
  StateTipping <- if (is.na(QualityTipping)) NA else 1 - QualityTipping
  
  Th_table <- lapply(d_values, function(d){
    
    TH_state <- if (is.na(QualityTipping)) NA else 1 - d * QualityTipping
    
    df2 <- NewData_D2D %>%
      filter(x <= PressureTipping) %>%
      arrange(x) %>%
      dplyr::select(x, y)
    
    thr2 <- TH_state
    
    idx2 <- NA
    if (!is.na(thr2)) {
      if (df2$y[1] >= thr2) {
        idx2 <- which(df2$y <= thr2)[1]
      } else {
        idx2 <- which(df2$y >= thr2)[1]
      }
    }
    
    TH_pressure <- NA
    if(!is.na(idx2)) {
      if (idx2 == 1) {
        TH_pressure <- df2$x[1]
      } else {
        x1 <- df2$x[idx2 - 1]; x2 <- df2$x[idx2]
        y1 <- df2$y[idx2 - 1]; y2 <- df2$y[idx2]
        TH_pressure <- x1 + (thr2 - y1) * (x2 - x1) / (y2 - y1)
      }
    }
    
    data.frame(d = d, State_threshold = TH_state, Pressure_threshold = TH_pressure)
  }) %>%
    bind_rows() %>%
    mutate(
      State_threshold = round(State_threshold, 3),
      Pressure_threshold = round(Pressure_threshold, 3)
    )
  
  Th_final <- Th_table[which.min(abs(Th_table$d - d_final)), ]
  Sta_D2D <- Th_final$State_threshold
  Pres_D2D <- Th_final$Pressure_threshold
  
  cat("D2D threshold (selected): P =", round(Pres_D2D, 3), " S =", round(Sta_D2D, 3), "\n")
  
  p_d2d <- ggplot(NewData_D2D, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = se_lower, ymax = se_upper), alpha = 0.18) +
    geom_line(linewidth = 1) +
    {if(!is.na(PressureTipping)) annotate("point", x = PressureTipping, y = StateTipping, shape = 17, size = 2)} +
    {if(!is.na(PressureTipping)) annotate("text", x = PressureTipping, y = StateTipping,
                                          label = paste0("Degradation point\nP=", round(PressureTipping, 2), " S=", round(StateTipping, 2)),
                                          hjust = -0.1, vjust = -0.6, size = 3)} +
    annotate("text", x = 0.2, y = 0.08, label = paste0("Sensitivity = ", Sens),
             hjust = 0, vjust = 0, size = 3) +
    {if(!is.na(Sta_D2D)) geom_hline(yintercept = Sta_D2D, linetype = "dashed", linewidth = 0.8)} +
    {if(!is.na(Pres_D2D)) geom_vline(xintercept = Pres_D2D, linetype = "dashed", linewidth = 0.8)} +
    {if(!is.na(Pres_D2D) && !is.na(Sta_D2D)) annotate("point", x = Pres_D2D, y = Sta_D2D, shape = 17, size = 2)} +
    {if(!is.na(Pres_D2D) && !is.na(Sta_D2D)) annotate("text", x = Pres_D2D, y = Sta_D2D,
                                                      label = paste0("P=", round(Pres_D2D, 3), "\nS=", round(Sta_D2D, 3)),
                                                      hjust = -0.1, vjust = -0.5)} +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(limits = c(0, 12)) +
    labs(x = "Pressure", y = "Sentinel species (normalized to P=0)", title = "Distance to degradation") +
    theme_minimal()
  
  Thresholds_table <- rbind(
    Thresholds_table,
    data.frame(
      Habitat = msfd_bht,
      Method = "Distance to degradation",
      Pressure_threshold = round(Pres_D2D, 3),
      State_threshold = round(Sta_D2D, 3)
    )
  )
  
  # ======================================================================
  # E) FINAL FIGURE PER HABITAT
  # ======================================================================
  final_plot <- ggarrange(p_d2d, p_nv, p_dc, ncol = 3)
  
  final_plot <- annotate_figure(
    final_plot,
    top = text_grob(paste0("Habitat: ", msfd_bht), face = "bold", size = 14)
  )
  
  safe_bht <- gsub("[^A-Za-z0-9]+", "_", msfd_bht)
  out_plot_hab <- file.path(
    plots_folder,
    paste0(base_name, "_", safe_bht, "_PLOT_THRESHOLDS_SOS.png")
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
# 7) Export thresholds table (ONCE)
# =========================
write.xlsx(Thresholds_table, file = thresholds_file, rowNames = FALSE)

cat("\n============================================================\n")
cat("Threshold table saved to:\n", thresholds_file, "\n")
cat("Done.\n")