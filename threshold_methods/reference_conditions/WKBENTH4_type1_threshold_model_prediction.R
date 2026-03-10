# script to predict thresholds for good status using (range of) natural variation (RNV) reference condition method
# Lorna McKellar, Jan Geert Hiddink, Daniel van Denderen


# ---- load threshold models and functions ---- # 
load('ref.cond_threshold_model_functions.RData')
# thresholds were modeled with environmental gradients - 5 indicators came back with significant relationships
# all thresholds are modeled using beta distribution as thresholds are constrained between 0-1

# ---- predict thresholds using specific environmental variables ---- # 
# functions for predicting thresholds for each indicator require p (probability) 
# and either median longevity, depth, or substrate (Sand, Mud, Coarse, Mixed) values

# biomass/ abundance thresholds ~ median longevity (p = 75% probability)
predict_biomass_longevity_threshold(longevity = 1, p = 0.75)

# relative margalef diversity (biomass) indicator thresholds ~ log10(depth)
predict_DMbiomass_depth_threhsold(depth = 200, p = 0.5)

# long-lived fraction indicator thresholds ~ substrate
predict_Lf_substrate_threhsold(substrate = 'Mixed', p = 0.8)

# species richness indicator thresholds ~ substrate
predict_richness_substrate_threhsold(substrate = 'Sand', p = 0.9)

# sentinels of the seabed indicator thresholds ~ median longevity
predict_sos_longevity_threhsold(longevity = 5, p = 0.6)


# example of predicting thresholds for multiple environmental values and/or probabilities 
predict_biomass_longevity_threshold(longevity = seq(1,5, by = 1), p = c(0.5, 0.75, 0.9))




