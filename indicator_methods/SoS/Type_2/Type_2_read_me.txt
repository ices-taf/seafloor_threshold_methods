Sentinel species and SoS workflow (Data Type 2)

This repository contains a reproducible workflow to identify the Sentinels of the Seabed  and calculate the Sentinel of the Seabed indicator (SoS; Serrano et al., 2022)  for Data Type 2 datasets.

The workflow:

Prepares biological datasets
Assigns species sensitivity (BESITO or equivalent)
Identifies sentinel species
Calculates SoS values per sampling unit
Exports results to standardised Type 2 formats

The approach is designed to be operational and applicable to real monitoring data, including datasets with limited taxonomic resolution or incomplete sensitivity ( BESITO) information.

Workflow structure

The workflow consists of three main steps:

1. Prepare data
Type 2 datasets are structurally heterogeneous. Therefore, data preparation is split into several scripts depending on the dataset type and the specific harmonisation issues required.

1.1 Epifauna datasets

Prepare_type2_epifauna.R


This script:

- reads raw Type 2 Excel files;
- detects biological and station-level sheets;
- standardises station, year, taxonomic and biological variables;
- harmonises taxonomy using WoRMS where possible;
- merges station-level information into biological records;
- extracts SAR pressure metrics;
- merges BESITO sensitivity scores;
- exports prepared `_data_ready.xlsx` 

1.2 Generic infauna datasets

Prepare_type2_infauna_generic.R

This script:

-reads raw Type 2 infauna Excel files;
-standardises biological and station-level information;
-builds station-level identifiers where needed;
-harmonises taxonomy using WoRMS;
-merges pressure and station information;
-merges BESITO sensitivity scores;
-assigns BESITO = 1 when no sensitivity score is available;
-exports prepared _data_ready.xlsx 

1.3 Infauna datasets with cumulative pressures

Prepare_type2_infauna_Cumulative_Pressure_datasets.R

Use this script only for datasets affected by both sand extraction and bottom trawling:

NS_Hinderbanken_sandextr_and_btrawling.xlsx
NS_Oostdyck_sandextr_and_btrawling.xlsx
NS_Thornton_sandextr_and_btrawling.xlsx

This script:

builds station_2 = station + year;
retains replicate information as descriptive metadata;
extracts two separate pressure variables:
EXTR_sand from pressure_value1;
SAR1 from pressure_value2;
exports both pressures separately for downstream analysis.

1.4 NS_BfN_grab special case

Prepare_type2_infauna_NS_BfN_grab_special_case.R

Use this script only for:

NS_BfN_grab_btrawling.xlsx

This dataset requires dedicated handling because biological records are replicate-level, whereas station-level information is reported at station-year level.

This script:

creates station_join = station + year for merging station-level fields;
creates a replicate-level identifier for biological records;
preserves replicate information while ensuring correct station-level pressure assignment;
exports a prepared _data_ready.xlsx file compatible with the SoS workflow.

1.5 Oxygen depletion special case

Prepare_type2_infauna_oxygen_depletion_special_caseI.R

Use this script only for:

BS_southernbaltic_oxygendepletion.xlsx

This dataset requires dedicated handling because the pressure variable is oxygen depletion, not bottom-trawling SAR.

This script:

retrieves and harmonises taxonomic information using WoRMS;
converts AMBI ecological groups into BESITO-like scores;
stores the internal sensitivity score as ambi_besito;
creates a standard BESITO column for compatibility with downstream SoS scripts;
stores oxygen depletion pressure in SAR1 for compatibility;
documents the pressure type in info_SAR1.

The external AMBI lookup table is not included in the repository and must be provided by the user.



2. Run sentinel species selection and SOS calculation

For standard Type 2 datasets:

run_type2_sos.R

For cumulative-pressure datasets:

run_type2_sos_special_case_cumulative_pressures.R

These scripts are designed to run on one prepared dataset at a time.

Each run processes a single `_data_ready.xlsx` file and produces
dataset-specific outputs (sentinel species list, SoS values, and diagnostics).

These scripts: 

- selects sentinel species using sensitivity scores and occurrence rules;
- allows dataset-specific pressure-class settings;
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

Type 2 datasets (Excel format)
Sensitivity database (BESITO & AMBI)
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