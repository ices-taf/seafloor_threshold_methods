#---
#   title: "Loading and initial formatting of Type 3 data"
#   subtitle: "Pressure-state long-lived biomass"
#   author: "Jochen Depestele"
#---

f_biom_above_long <- function(longevity=3, lintercept, lslope){ 
  1-plogis(lintercept + lslope * log(longevity))
}

m.biom.abund <- get("m.biom.abund",
                    envir = local({e <- new.env()
                    load("./indicator_methods/LonglivedBiomass/substrate_threshold_models.RData", envir = e)
                    e
                    }))

PS_dt <- readRDS("./indicator_methods/LonglivedBiomass/Type3_biomass_long_meta_dt.RDS")

# # Keep only groups with >= 10 observations
# toplot <- PS_dt %>%
#   group_by(habitat_type, gear_cat) %>%
#   filter(n() >= 10)
# # Compute mean for 'ref' per habitat_type & gear_cat
# mean_ref <- toplot %>%
#   filter(pressure_value < 0.75) %>%  # category "ref"
#   group_by(habitat_type, gear_cat) %>%
#   summarise(mean_logQ90 = mean(log(Q90long+1e-6), na.rm = TRUE))
# 
# ggplot(data=toplot,
#        aes(x=pressure_value, y=log(Q90long),
#            col=(ifelse(pressure_value<0.75, "ref","fished")),
#            group=interaction(habitat_type, gear_cat)))+
#   geom_point(size=2) +
#   geom_smooth(method="gam", formula = y ~ s(x, k=2),
#               color="#4DB6AC", fill="#4DB6AC", alpha=0.25) +
#   # geom_smooth(formula="y~x", method="loess", color="#4DB6AC", fill="#4DB6AC", alpha=0.25)+
#   geom_smooth(formula="y~x", method="lm", color="#4DB6AC", fill="#4DB6AC", alpha=0.25) + # col="#FF6F61", fill="#FF6F61", alpha=0.25)+
#   geom_hline(data = mean_ref, aes(yintercept = mean_logQ90),
#              linetype="dashed", color="black") +
#   facet_wrap(habitat_type~gear_cat, scales="free") +
#   scale_color_manual(values=c("#9B59B6","#BFD833")) +
#   theme_bw() +
#   theme(legend.position = "none")

PS_dt[,
      `:=` (refpres_Q10  = round(quantile(pressure_value, .10, na.rm=T),2),
            refpres_Q15  = round(quantile(pressure_value, .15, na.rm=T),2),
            refpres_Q25  = round(quantile(pressure_value, .25, na.rm=T),2)),
      by = .(.id, gear_cat, habitat_type)]

PS_dt[,ID_analy := paste(.id, gear_cat, habitat_type, sep="%%")]

PS_dt[pressure_value <= refpres_Q15,
      .N,
      by = .(.id, gear_cat, habitat_type, ID_analy)][N>=5]

SEL.THR = 0.75
if(SEL.THR=="avg_Q15"){
  selcases <- PS_dt[pressure_value <= refpres_Q15, 
                    .(nr_refs = .N, 
                      avgPres = mean(pressure_value, na.rm = TRUE)),
                    by = .(.id, gear_cat, habitat_type, ID_analy)][avgPres<0.75 & nr_refs>=5] # HERE YOU SELECT CASES WHICH HAVE ON AVERAGE A PRESSURE < 0.75
  PS_dt[, ref := pressure_value <= refpres_Q15]
  table(PS_dt$habitat_type, PS_dt$ref, PS_dt$.id)
}else{
  SEL.THR = 0.75
  selcases <- PS_dt[pressure_value <= SEL.THR, # HERE YOU SELECT CASES WHICH HAVE EACH HAVE A PRESSURE < 0.75
                    .(nr_refs = .N, 
                      avgPres = mean(pressure_value, na.rm = TRUE)),
                    by = .(.id, gear_cat, habitat_type, ID_analy)][nr_refs>=5]
  PS_dt[, ref := pressure_value <= SEL.THR]
  table(PS_dt$habitat_type, PS_dt$ref, PS_dt$.id)
}
selcases

Lclasses <- c("L1", "L3", "L10", "L10plus")

# Estimate longevity of reference samples
B_ref <- PS_dt[ID_analy %in% selcases$ID_analy,
               {
                 L_dt <- data.table(data.frame(L1=L1, L3=L3, L10=L10, L10plus=L10plus))
                 L_dt[,
                      (Lclasses) := {
                        cs <- t(apply(.SD, 1, cumsum))
                        rs <- rowSums(.SD, na.rm=T)
                        as.data.table(cs / rs)},
                      .SDcols = Lclasses]
                 L_dt <- melt(L_dt[,-4],
                              id.vars = setdiff(names(L_dt[,-4]), c("L1", "L3", "L10")),
                              measure.vars = c("L1", "L3", "L10"),
                              variable.name = "Lclasses", value.name = "Cumb")
                 L_dt[,ll:=log(as.numeric(gsub("L","",Lclasses)))]
                 suppressWarnings(mod <- glm(Cumb~ll, family = binomial, data = L_dt))
                 ref_int <- coef(mod)["(Intercept)"]
                 ref_slope <- coef(mod)["ll"]

                 pval <- summary(mod)$coefficients["ll","Pr(>|z|)"]
                 refpres_Q15 <- unique(refpres_Q15)
                 .(ref_int, ref_slope, pval, refpres_Q15)
              },
              by = .(.id, gear_cat, habitat_type, ID_analy)]

B_ref[, long_Ksen := exp((log(9) - ref_int) / ref_slope) ]
summary(B_ref$long_Ksen)

# ADD INDICATOR THRESHOLD from MODEL

step.size <- 0.01
longevity_vec <- seq(1, 200, by = step.size)
B_ref[, row_id := .I]
CJ_dt <- CJ(
  row_id = B_ref$row_id,
  longevity = longevity_vec
)[B_ref, on = "row_id"]
CJ_dt[, K := (ref_slope * exp(ref_slope * log(longevity) + ref_int)) /
        (longevity * (exp(ref_slope * log(longevity) + ref_int) + 1)^2)]
lifespan_dt <- CJ_dt[longevity > long_Ksen,
                     .(ref_biomass.weighted.Tmax.at.sen = sum(longevity * K) / sum(K)),
                     by = row_id]
lifespan_dt[, ref_lifespan.int := ref_biomass.weighted.Tmax.at.sen ^ -0.5 ]
pred_input <- B_ref[lifespan_dt,
                    on = "row_id"][, .(row_id, ID_analy, ref_lifespan.int)]

pred_input

mu <- predict(m.biom.abund,
              newdata = data.frame(longevity.int = pred_input$ref_lifespan.int, data_type="whole_community"),
              type = "response")
phi <- predict(m.biom.abund,
               newdata = data.frame(longevity.int = pred_input$ref_lifespan.int, data_type="whole_community"),
               type = "precision")

alpha_betreg <- mu * phi
beta_betreg  <- (1 - mu) * phi

RBSsen.threshold.50 <- qbeta(0.50, alpha_betreg, beta_betreg)
RBSsen.threshold.90 <- qbeta(0.90, alpha_betreg, beta_betreg)
B_ref[, `:=`(alpha_betreg = alpha_betreg, beta_betreg = beta_betreg, 
             RBSsen.threshold.50 = RBSsen.threshold.50, RBSsen.threshold.90 = RBSsen.threshold.90)]

B_ref

# extract summary statistics across ref samples
B_ref <- B_ref[,as.list(unlist(lapply(.SD, summary))),
               by = c(".id", "gear_cat", "habitat_type", "ID_analy", 
                      "long_Ksen", "ref_int", "ref_slope", "refpres_Q15"), # longevities were estimated for all reference values together
               .SDcols = c("alpha_betreg","beta_betreg", 
                           "RBSsen.threshold.50", "RBSsen.threshold.90")] # 
selcols <- names(B_ref)[!grepl("Min|Max|_betreg.Mean|_betreg.1st|_betreg.3rd", names(B_ref))]
B_ref <- B_ref[,..selcols]

selcols <- c(".id", "gear_cat", "habitat_type", "ID_analy","long_Ksen", "ref_int", "ref_slope", "alpha_betreg.Median", "beta_betreg.Median", 
             names(B_ref)[grepl("RBSsen.threshold",names(B_ref))])
selcols
PScase_dt <- B_ref[,..selcols][PS_dt[ID_analy %in% selcases$ID_analy], on=c(".id", "gear_cat", "habitat_type", "ID_analy")]

PScase_dt[,biom_Ksens := f_biom_above_long(long_Ksen, lintercept, lslope)]

PScase_dt[, mean_biom_Ksens_ref :=
            mean(biom_Ksens[ref == TRUE], na.rm = TRUE),
          by = .(.id, gear_cat, habitat_type, ID_analy)]
PScase_dt[,rel_biom_Ksens := biom_Ksens / mean_biom_Ksens_ref]

PScase_dt[, SEL_THR := RBSsen.threshold.50.Median]
PScase_dt[, SEL_THR := round(SEL_THR, 8)]
#
# Fit GLM per group and get predicted values + CI
B_pred <- PScase_dt[,
                {
                  # Fit GLM on logit scale
                  m <- glm(log(rel_biom_Ksens+1e-6) ~ 0 + pressure_value, family = gaussian)
                  s <- summary(m)
                  
                  # Store intercept, slope, p-value, 15% quantile
                  resp_int <- coef(m)["(Intercept)"]
                  resp_slope <- coef(m)["pressure_value"]
                  resp_se    <- summary(m)$coefficients["pressure_value", "Std. Error"]
                  slope_lwr <- resp_slope - 1.96 * resp_se
                  slope_upr <- resp_slope + 1.96 * resp_se
                  pval <- s$coefficients["pressure_value","Pr(>|t|)"]
                  refpres_Q15 <- round(quantile(pressure_value, .15, na.rm=T),2)
                  max_pres <- max(pressure_value, na.rm=T)
                  
                  # Create prediction grid
                  x_seq <- seq(0, 100, by = 0.01)
                  pred <- predict(m, newdata = data.frame(pressure_value = x_seq),
                                  type = "response", se.fit = TRUE)
                  
                  # Compute 95% CI on logit scale
                  log_fit <- pred$fit
                  log_se  <- pred$se.fit
                  log_upper <- log_fit + 1.96 * log_se
                  log_lower <- log_fit - 1.96 * log_se
                  
                  # Back-transform to 0-1 scale
                  resp_fit   <- exp(log_fit)
                  resp_se   <- exp(log_se)
                  resp_upper <- exp(log_upper)
                  resp_lower <- exp(log_lower)
                  
                  # What SAR at the threshold?
                  SAR_at_THR_pred = (log(unique(SEL_THR) + 1e-6)) / resp_slope  # THRESHOLD MUST BE SPECIFIC BY ID_analy
                  SAR_at_THR_lower <- (log(unique(SEL_THR) + 1e-6)) / slope_lwr
                  SAR_at_THR_upper <- (log(unique(SEL_THR) + 1e-6)) / slope_upr

                  # Return as a data.table
                  .(pressure_value = x_seq,
                    y_pred = resp_fit,
                    y_lower = resp_lower,
                    y_upper = resp_upper,
                    resp_se=exp(log_se),
                    resp_int = resp_int,
                    resp_slope = resp_slope,
                    pval = pval,
                    refpres_Q15 = refpres_Q15,
                    SAR_at_THR_lower=SAR_at_THR_lower,
                    SAR_at_THR_pred=SAR_at_THR_pred,
                    SAR_at_THR_upper=SAR_at_THR_upper,
                    max_pres = max_pres
                  )
                },
                by = .(.id, gear_cat, habitat_type, ID_analy)]

# B_pred

# to_report: 
selcols <- c(".id","gear_cat","habitat_type", "ID_analy","resp_slope","pval","refpres_Q15", "SAR_at_THR_pred", "SAR_at_THR_lower", "SAR_at_THR_upper")
slopes <- unique(B_pred[,..selcols])
selcols <- c(".id","gear_cat","habitat_type", "ID_analy","mean_biom_Ksens_ref","long_Ksen")
to_table2 <- unique(PScase_dt[,..selcols])
to_table2 <- to_table2[slopes[order(pval, decreasing=F)], on=c(".id","gear_cat","habitat_type", "ID_analy")]
num_cols <- names(to_table2)[sapply(to_table2, is.numeric)]
num_cols <- num_cols[!num_cols %in% c("pval")]
to_table2[, (num_cols) := lapply(.SD, round, digits = 2), .SDcols = num_cols]
to_table2[, ("pval") := lapply(.SD, round, digits = 7), .SDcols = "pval"]
selcols <- c(".id","gear_cat","habitat_type", "ID_analy","resp_slope","pval", "SAR_at_THR_lower", "SAR_at_THR_pred", "SAR_at_THR_upper")
to_table2[pval>=0.05, 
          c("SAR_at_THR_lower", "SAR_at_THR_pred", "SAR_at_THR_upper") := NA]
to_table2 <- to_table2[order(pval) ,..selcols]
selcols <- c("ID_analy","SEL_THR")
to_table2 <- unique(PScase_dt[,..selcols])[to_table2, on="ID_analy"]
plot_table2 <- copy(to_table2)
selcols <- names(to_table2)[grepl("SAR_at_THR",names(to_table2))]
to_table2[,
          (selcols):=lapply(.SD, function(x)ifelse(is.na(x), "-",ifelse(x >= 10, ">10", as.character(pmax(0, x))))),
          .SDcols=selcols]
to_table2


# # [ID_analy=="BoBIC_CGFS%%trawl%%Circalittoral coarse sediment"]
# ggplot(B_pred, # B_pred2[ID_analy %in% B_pred2$ID_analy[grepl("Upper bathyal rock",B_pred2$ID_analy)]],
#        aes(x = pressure_value, y = y_pred, group = ID_analy)) +
#   geom_ribbon(aes(ymin = y_lower, ymax = y_upper), fill = "#1F5AA6", alpha = 0.2) +
#   geom_line(color = "#1F5AA6", linewidth = 1) +
#   geom_point(data = PScase_dt,
#              aes(x = pressure_value, y = rel_biom_Ksens, shape = factor(ref)),
#              color = "grey33") +
#   geom_hline(yintercept = 1, linetype=2, col="grey33") +
#   facet_wrap(. ~ ID_analy, scales = "free") +
#   scale_shape_manual(values = c(16, 1)) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   labs(title = "Log-normal Regression with 95% CI",
#        y = "relative longlived biomass to reference conditions",
#        x = "Pressure_value") +
#   scale_x_continuous(limits=c(0,25)) +
#   scale_y_continuous(limits=c(0,5))
# # 


selcols <- names(plot_table2)[grepl("SAR_at_THR",names(plot_table2))]
plot_table2[,
              (selcols):=lapply(.SD, function(x)ifelse(x<0, NA, x)),
              .SDcols=selcols]
plot_table2



facet_max_y <- PScase_dt %>%
  group_by(ID_analy) %>%
  summarise(max_y = max(rel_biom_Ksens, na.rm = TRUE), .groups = "drop")
# max(facet_max_y$max_y)
facet_max_x <- PScase_dt %>%
  group_by(ID_analy) %>%
  summarise(max_x = max(pressure_value, na.rm = TRUE), .groups = "drop")

B_pred2 <- B_pred %>%
  left_join(facet_max_y, by = c("ID_analy"))
B_pred2 <- B_pred2 %>%
  left_join(facet_max_x, by = c("ID_analy"))
B_pred2 <- B_pred2 %>%
  mutate(
    # y_lower  = pmin(y_lower, max_y),
    y_pred   = pmin(y_pred, max_y), # Cap predictions and ribbons at per-facet max
    y_upper  = pmin(y_upper, max_y),
    pressure_value = pmin(pressure_value, max_x)
  )
PScase_dt2 <- PScase_dt %>%
  left_join(facet_max_y, by = c("ID_analy"))
PScase_dt2 <- PScase_dt2 %>%
  left_join(facet_max_x, by = c("ID_analy"))
PScase_dt2 <- PScase_dt2 %>%
  mutate(
    pressure_value = pmin(pressure_value, max_x), # Cap predictions and ribbons at per-facet max
    rel_biom_Ksens = pmin(rel_biom_Ksens, max_y)
  )

toplot_table2 <- copy(plot_table2) %>%
  left_join(facet_max_x, by = c("ID_analy"))
toplot_table2 <- toplot_table2 %>%
  mutate(
    SAR_at_THR_lower = pmin(SAR_at_THR_lower, max_x),
    SAR_at_THR_upper = pmin(SAR_at_THR_upper, max_x),
    SAR_at_THR_pred  = pmin(SAR_at_THR_pred, max_x)
  )

SARcols <- grep("SAR", names(toplot_table2), value = TRUE)
toplot_table2[ , (SARcols) := lapply(.SD, function(x) as.numeric(as.character(x))),
               .SDcols = SARcols]

# max(PScase_dt2$max_y)
# round(PScase_dt2[ID_analy %in% B_pred2$ID_analy[grepl("Upper bathyal rock",B_pred2$ID_analy)],]$rel_biom_Ksens, 2)

unique(B_pred2$ID_analy)
unique(B_pred2$ID_analy)[grepl("rock",unique(B_pred2$ID_analy))] # rock and biogenic reef are not included
plot_cases <- unique(B_pred2$ID_analy)[!unique(B_pred2$ID_analy) %in% c(unique(B_pred2$ID_analy)[grepl("rock",unique(B_pred2$ID_analy))],
                                                                        "WMS_FRMEDITS%%trawl%%Upper bathyal sediment", "WMS_FRMEDITS%%trawl%%Lower bathyal sediment")]
ggplot(B_pred2[ID_analy %in% plot_cases], 
       aes(x = pressure_value, y = y_pred, group = ID_analy)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), fill = "#1F5AA6", alpha = 0.2) +
  geom_line(color = "#1F5AA6", linewidth = 1) +
  geom_point(data = PScase_dt2[ID_analy %in% plot_cases & pressure_value<100],
             aes(x = pressure_value, y = rel_biom_Ksens, shape = factor(ref)),
             color = "grey33") +
  geom_hline(yintercept = 1, linetype=2, col="grey33") +
  geom_hline(data=toplot_table2[ID_analy %in% plot_cases], aes(yintercept = SEL_THR),
             col="#B44A3A") +
  geom_rect(data = toplot_table2[ID_analy %in% plot_cases],
            aes(xmin = SAR_at_THR_lower, xmax = SAR_at_THR_upper, ymin = -Inf, ymax = Inf),
            fill = "grey75", alpha = 0.25, inherit.aes = FALSE) +
  geom_vline(data = toplot_table2[ID_analy %in% plot_cases], aes(xintercept = SAR_at_THR_pred), col="grey75") +
  facet_wrap(. ~ ID_analy, scales = "free") +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Log-normal Regression with 95% CI",
       y = "relative longlived biomass to reference conditions",
       x = "Pressure_value")


ggplot2::ggsave("./indicator_methods/LonglivedBiomass/Type3_PressureState_thresholds.png", unit="cm",width=30,height=20)

fwrite(plot_table2, "./indicator_methods/LonglivedBiomass/Type3_PressureState_thresholds.csv")

#




















# ---------------------------------------------------
# Step 1: Compute per-facet maximum y
# ---------------------------------------------------
facet_max_y <- PScase_dt %>%
  group_by(ID_analy) %>%
  summarise(max_y = max(rel_biom_Ksens, na.rm = TRUE), .groups = "drop")
facet_max_x <- PScase_dt %>%
  group_by(ID_analy) %>%
  summarise(max_x = max(pressure_value, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------
# Step 2: Merge max_y into predictions and observations
# ---------------------------------------------------
B_pred2 <- B_pred %>%
  left_join(facet_max_y, by = c("ID_analy"))
toplot_table2 <- toplot_table2 %>%
  left_join(facet_max_x, by = c("ID_analy"))

PScase_dt2 <- PScase_dt %>%
  left_join(facet_max_y, by = c("ID_analy"))
PScase_dt2 <- PScase_dt2 %>%
  left_join(facet_max_x, by = c("ID_analy"))
PScase_dt2 <- PScase_dt2 %>%
  mutate(
    pressure_value = pmin(pressure_value, max_x), # Cap predictions and ribbons at per-facet max
    rel_biom_Ksens = pmin(rel_biom_Ksens, max_y)
  )

# ---------------------------------------------------
# Step 3: Cap predictions and ribbons at per-facet max
# ---------------------------------------------------
B_pred2 <- B_pred2 %>%
  mutate(
    y_pred   = pmin(y_pred, max_y),
    y_upper  = pmin(y_upper, max_y)
  )
toplot_table2 <- toplot_table2 %>%
  left_join(facet_max_x, by = c("ID_analy"))
toplot_table2 <- toplot_table2 %>%
  mutate(
    SAR_at_THR_lower = pmin(SAR_at_THR_lower, max_x),
    SAR_at_THR_upper = pmin(SAR_at_THR_upper, max_x),
    SAR_at_THR_pred  = pmin(SAR_at_THR_pred, max_x)
  )

# ---------------------------------------------------
# Step 4: Plot
# ---------------------------------------------------
unique(B_pred2$ID_analy)
unique(B_pred2$ID_analy)[grepl("rock",unique(B_pred2$ID_analy))] # rock and biogenic reef are not included
plot_cases <- unique(B_pred2$ID_analy)[!unique(B_pred2$ID_analy) %in% c(unique(B_pred2$ID_analy)[grepl("rock",unique(B_pred2$ID_analy))],
                                                                        "WMS_FRMEDITS%%trawl%%Upper bathyal sediment", "WMS_FRMEDITS%%trawl%%Lower bathyal sediment")]
ggplot(B_pred2[ID_analy %in% plot_cases & pressure_value<100], 
       aes(x = pressure_value, y = y_pred, group = .id)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), fill = "#1F5AA6", alpha = 0.2) +
  geom_line(color = "#1F5AA6", linewidth = 1) +
  geom_point(data = PScase_dt2[ID_analy %in% plot_cases & pressure_value<100],
             aes(x = pressure_value, y = rel_biom_Ksens, shape = factor(ref)),
             color = "grey33") +
  # geom_hline(yintercept = 1, linetype=2, col="grey33") +
  # geom_hline(data=toplot_table2[ID_analy %in% plot_cases], aes(yintercept = SEL_THR), 
  #            col="#B44A3A") +
  # geom_rect(data = toplot_table2[ID_analy %in% plot_cases], 
  #           aes(xmin = SAR_at_THR_lower, xmax = SAR_at_THR_upper, ymin = -Inf, ymax = Inf),
  #           fill = "grey75", alpha = 0.25, inherit.aes = FALSE) +
  # geom_vline(data = toplot_table2[ID_analy %in% plot_cases], aes(xintercept = SAR_at_THR_pred), col="grey75") +
  facet_wrap(. ~ ID_analy, scales = "free") +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Log-normal Regression with 95% CI",
       y = "relative longlived biomass to reference conditions",
       x = "Pressure_value")




#





















# to_report: 
selcols <- c("ID_analy","resp_slope","pval","refpres_Q15")
slopes <- unique(B_pred[,..selcols])
selcols <- c("ID_analy","mean_biom_Ksens_ref","long_Ksen")
to_table2 <- unique(PScase_dt[,..selcols])
to_table2 <- to_table2[slopes[order(pval, decreasing=F)], on=c("ID_analy")]
num_cols <- names(to_table2)[sapply(to_table2, is.numeric)]
num_cols <- num_cols[!num_cols %in% c("pval")]
to_table2[, (num_cols) := lapply(.SD, round, digits = 2), .SDcols = num_cols]
to_table2[, ("pval") := lapply(.SD, round, digits = 7), .SDcols = "pval"]
selcols <- c("ID_analy", "resp_slope","pval")
to_table2 <- to_table2[order(pval) ,..selcols]
to_table2
