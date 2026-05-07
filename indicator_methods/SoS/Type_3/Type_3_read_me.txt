Sentinel species and SoS workflow (Data Type 3)

This repository contains a reproducible workflow to identify the Sentinels of the Seabed  and calculate the Sentinel of the Seabed indicator (SoS; Serrano et al., 2022)  for Data Type 3 datasets.

The workflow:

Prepares biological datasets
Assigns species sensitivity (BESITO )
Identifies sentinel species
Calculates SoS values per sampling unit
Exports results to standardised Type 3 formats

The approach is designed to be operational and applicable to real monitoring data, including datasets with limited taxonomic resolution or incomplete sensitivity ( BESITO) information.

Workflow structure

The workflow consists of three main steps:

1. Prepare data
Type 3 datasets are generally more standardised than Type 2 datasets,
but still require dataset-specific handling depending on taxonomic
resolution, sampling design, and national data formats.
Prepared datasets are harmonised into a common structure and exported
as: <dataset_name>_data_ready.xlsx
These files are the direct input for the SoS calculation workflow.

1.1 Epifauna datasets

Prepare_type3_epifauna.R


Use this script for Type 3 epifauna datasets.

This script:

- reads raw Type 3 epifauna Excel files;
- standardises biological and station-level information;
- harmonises taxonomy using WoRMS;
- merges station and pressure information;
- merges BESITO sensitivity scores;
- applies basic filtering and formatting;
- exports one harmonised `_data_ready.xlsx

1.2 Generic infauna datasets

Prepare_Type3_infauna_generic.R

Use this script for standard Type 3 infauna datasets.

This script:

- reads raw Type 3 infauna Excel files;
- standardises biological and station-level information;
- builds consistent station identifiers;
- harmonises taxonomy using WoRMS;
- merges pressure variables (e.g. SAR);
- merges BESITO sensitivity scores;
- assigns default sensitivity where needed;
- exports one harmonised _data_ready.xlsx

1.3 Special cases

#### NS_BE special case

`Prepare_type3_infauna_NS_BE_special_case.R`

Use this script for NS_BE datasets requiring:

- specific station-level aggregation;
- custom merging of biological and station information;
- dataset-specific harmonisation steps.

---

#### NS_NL special case

`Prepare_type3_infauna_NS_NL_special_case.R`

Use this script for NS_N datasets requiring:

- dataset-specific formatting;
- custom handling of station identifiers;
- harmonisation of biological and pressure data.

---

These special-case scripts ensure compatibility with the standard
Type 3 SoS workflow while preserving the original data structure.


2. Run sentinel species selection and SOS calculation


run_type3_sos.R


This script is designed to run on one prepared dataset at a time.

Within each dataset, the workflow is applied at the habitat level.
Sentinel species selection and SoS calculation are performed separately
for each habitat that meets the minimum data requirements.

In the example provided in this repository, the script is shown for a
single habitat. However, in practice, the same procedure should be
repeated for all habitats within the dataset that satisfy the required
criteria (e.g. sufficient number of samples).

This script:

- selects sentinel species using sensitivity scores and occurrence rules;
- applies habitat-specific filtering;
- calculates SoS per station or haul;
- exports:
  - sentinel species list;
  - SoS-ready dataset;
  - diagnostic plots;
  - processing summary.

Outputs are written as: _data_ready.xlsx

3. Export SoS results to Excel

export_sos_to_excel.R

Reads Data_Ready CSV files
Merges SoS values into the "station information" sheet
Writes updated Excel files without overwriting originals

Input data

Required inputs:

Type 3 datasets (Excel format)
Sensitivity database (BESITO)
WoRMS matching (for taxonomic harmonisation)

Output files

Intermediate outputs:
../sos_output/<DATA_TYPE>/<dataset_name>/Data_Ready_<dataset_name>.csv

Final outputs:
../final_output/<DATA_TYPE>/<dataset_name>.xlsx


How to run
Prepare input datasets using the appropriate script
Run sentinel species selection and SoS calculation
Export SoS results to the harmonised Excel file:

Contact: 
For questions or collaboration, please contact with authors.