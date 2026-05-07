Pressure–state threshold estimation (Data Type 3)

This repository contains a reproducible workflow to estimate pressure–state thresholds for Data Type 3 datasets using GAM-based modelling.

Type 3 datasets are characterised by sufficient sample size and well-defined pressure gradients (e.g. SAR), allowing robust pressure–state modelling.

Workflow overview

The workflow:

reads indicator values from harmonised datasets;
fits GAM pressure–state relationships per habitat;
estimates thresholds using multiple methods;
produces outputs per dataset and per habitat.

Scripts

Threshold estimation is implemented in dedicated scripts (single or modular).

Each script:

runs on one dataset at a time;
applies the workflow separately to each habitat within the dataset.

Habitat-level implementation

Within each dataset, threshold estimation is performed independently for each habitat.

The example scripts included in this repository may show a single habitat for illustration purposes. In practice, the workflow should be repeated for all habitats that satisfy the required criteria (e.g. sufficient number of samples).


## Threshold methods

### For SoS:

- Natural Variation (NV), also referred to as Zero Pressure (ZP)
- Detectable Change (DC), also referred to as First Detectable Change (FDC)
- Distance to Degradation (D2D)

### For other indicators (biomass, richness, Margalef):

- Natural Variation (NV)
- Detectable Change (DC)

Distance to Degradation (D2D) is not applied to these indicators, as it is considered specific to bounded indicators such as SoS.

## Methodological approach

- GAM models are fitted per habitat
- Predictions are normalised to the fitted value at P = 0
- Pressure domain is standardised (typically SAR = 12)
- Bootstrap is used for D2D sensitivity estimation (SoS only)

Input data
Harmonised Type 3 Excel datasets
Sheet: "Station information"
Required fields:
pressure variables (e.g. SAR)
indicator values
Output files

Outputs are written to:

../thresholds_output/Type_3/<dataset_name>/

Including:

threshold tables
diagnostic plots
multi-panel figures comparing methods

