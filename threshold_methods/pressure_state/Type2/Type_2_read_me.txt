Pressure–state threshold estimation (Data Type 2)

This repository contains a reproducible workflow to estimate pressure–state thresholds for Data Type 2 datasets using GAM-based modelling.

Type 2 datasets are typically characterised by limited sample size, reduced pressure gradients, and case-study-specific structure. As a result, threshold estimation is conditional and often not possible.

Workflow overview

The workflow:

reads indicator values from harmonised Type 2 Excel files;
fits GAM pressure–state relationships;
applies strict gating criteria;
estimates thresholds only when conditions are met;
exports diagnostic outputs.

Threshold estimation is performed separately for each indicator.

Scripts (indicator-specific)

Thresholds are computed using separate scripts for each indicator:

thresholds_type2_sos.R
thresholds_type2_biomass.R
thresholds_type2_richness.R
thresholds_type2_margalef.R

Each script runs on one dataset at a time.

Threshold methods

For SoS:

Natural Variation (NV), also referred to as Zero Pressure (ZP)
Detectable Change (DC), also referred to as First Detectable Change (FDC)
Distance to Degradation (D2D)

For other indicators (biomass, richness, Margalef):

Natural Variation (NV)
Detectable Change (DC)
D2D is not applied


Gating criteria (HARD RULE)

Thresholds are computed ONLY when both conditions are met:

Degradation condition
At least 50% of predicted values at moderate-to-high pressure
(P = 0.65) are below the predicted baseline value at P = 0.
Statistical significance
The smooth term of the GAM is significant at p < 0.06.
Behaviour when criteria are not met

If any condition is not satisfied:

thresholds are NOT computed
threshold plots are NOT produced
only GAM diagnostics (p-values and model summaries) are exported


Input data
Post-workshop harmonised Type 2 Excel files
Sheet: "Station information"
Required fields:
pressure_value
indicator values (SoS, biomass, richness, Margalef)
Output files

Outputs are written to:

../thresholds_output/Type_2/<dataset_name>/

Including:

threshold tables (if valid)
GAM diagnostics (always)
diagnostic plots (if thresholds are computed)
Key interpretation note

Absence of thresholds does NOT imply absence of impact.
It reflects insufficient statistical support given the available data.