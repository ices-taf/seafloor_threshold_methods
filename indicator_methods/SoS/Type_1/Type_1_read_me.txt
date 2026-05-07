Sentinel species and SoS workflow (Data Type 1)

This repository contains a reproducible workflow to identify the Sentinels of the Seabed  and calculate the Sentinel of the Seabed indicator (SoS; Serrano et al., 2022)  for Data Type 1 (long-term time series) species by biomass datasets.

The workflow:

Prepares epifauna and infauna datasets
Identifies sentinel species based on BESITO index (Gonzalez-Irusta et al., 2018)
Calculates SoS values per sampling unit
Exports results to standardised Type 1 Excel templates

The approach is designed to be operational and applicable to real monitoring data, including datasets with limited taxonomic resolution or incomplete sensitivity ( BESITO) information.

Workflow structure

The workflow consists of four main scripts:

1. Prepare data (epifauna)

prepare_data_epifauna.R

Loads raw Type 1 datasets
Matches species with sensitivity scores (BESITO + WoRMS fallback)
Applies filtering and standardisation
Outputs prepared datasets

2. Prepare data (infauna)

prepare_data_infauna.R

Similar to epifauna workflow
Includes additional handling for missing sensitivity scores
Assigns conservative default sensitivity where needed

3. Run SoS calculation

run_type1_sos.R

Selects sentinel species based on:
Sensitivity scores
Occurrence thresholds

Computes SoS as:
Biomass-based (PerTS_ByWeight)
Outputs one CSV per dataset:

Data_Ready_<dataset_name>.csv

4. Export results to Excel

export_sos_to_excel.R

Reads Data_Ready CSV files
Merges SoS values into the "Time series information" sheet
Writes updated Excel files without overwriting originals

Input data

Required inputs:

Type 1 datasets (Excel format)
Sensitivity database (BESITO)
WoRMS matching (for taxonomic harmonisation)

Output files

Intermediate outputs:
../sos_output/<DATA_TYPE>/<dataset_name>/Data_Ready_<dataset_name>.csv

Final outputs:
../final_output/<DATA_TYPE>/<dataset_name>.xlsx


How to run
Prepare input datasets
Run prepare_data_epifauna.R or prepare_data_infauna.R
Run run_type1_sos.R
Run export_sos_to_excel.R

Contact: 
For questions or collaboration, please contact with authors.