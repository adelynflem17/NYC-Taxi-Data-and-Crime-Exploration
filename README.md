# NYC-Taxi-Data-and-Crime-Exploration
This project explores the relationship between urban mobility and public safety in New York City by analyzing taxi trip data alongside crime statistics. It aims to uncover spatial and temporal patterns in movement and crime, and visualize these trends to support policy, planning, and academic research. 

## 📁 Data Access and Structure
This repository supports the MS Report NYC Taxi Data and Crime Exploration, which focuses on analyzing spatial and temporal relationships between urban mobility and public safety. Due to file size constraints, data access is split between GitHub and Dropbox. 

🔹 GitHub-Hosted Datasets (/data folder)
These smaller datasets are available directly in this repository: 
- MovementData.xlsx: Net movement metrics by location and month
- Population_data.xlsx: Borough-level population estimates
- crime_counts_by_borough_month.xlsx: Aggregated monthly crime counts by borough and crime description
You can view these files in the /data folder

🔹 Dropbox-Hosted Datasets (Full Taxi & Crime Records)
Due to GitHub's file limit, full datasets for the yellow taxi cab trips are hosted externally
- [Download full Excel files](https://www.dropbox.com/scl/fo/mrss9gpi3mgna98h35mvd/AEDUoPpmHZoa_zKQ5yzL4Sk?rlkey=ao9oyapxx583ovkp0v6xrz007&st=byxsi2tb&dl=0)

These include:
- NYC Taxi Trip Data: Raw trip-level records used for spatial joins and movement analysis for each month
- NYPD Crime Complaint Data: Incident-level reports used for temporal and geographic aggregation

All files are read into R using readxl::read_excel() or data.table::fread() depending on format and size. 

## 📁 R Code
This repository includes modular R scripts for spatial analysis, data merging, and visualization. The code is organized to support reproducible workflows for:
- Mapping taxi zones and census tracts by borough
- Calculating monthly net movement (in/out) by zone
- Merging crime, movement, and population data
- Generating heatmaps of crime occurrences by weekday and hour
- Creating annotated multi-panel plots for borough-level and monthly trends

Each script is commented for clarity and designed to be reusable across boroughs and crime types. Key functions include:
- 01_NYCCrimeData_Data_Processing(): Cleaning up the NYC yellow taxi-cab data
- 02_NYCCrimeData_Combining_Plots(): Computes zone-level net movement for each month and visualizes spatial trends using color-coded maps and ranked labels
- 03_NYCCrimeData_Census_Tract_Zone_Overlay(): Generates spatial overlays of taxi zones and census tracts
- 04_NYCCrimeData_Combined_Visual(): Builds weekday-hour heatmaps for selected offenses
- 05_NYCCrimeData_Correlation_Plots(): Generates scatter plots of crime counts vs. movement/population metrics for a given borough and month

To streamline execution, a consolidated script (Combined File.R) is provided. This script sources all modular components in the correct order, allowing users to run the full analysis pipeline from cleaned data to final visualizations. 
- Note: The raw data cleaning step (01_NYCTaxiData_Data_Processing.R) is excluded from the combined script for efficiency. Cleaned Excel files are available in the /data folder and used directly    in subsequent scripts.
- Required Files: To run Combined File.R, ensure that all necessary Excel files are downloaded locally. These files are available either in the /data folder of this repository 
  (for smaller datasets) or via Dropbox (for full taxi and crime records). Download these files prior to execution and update file paths in the script as needed.

To run the analysis, ensure the required packages are installed and update file paths as needed. All visual outputs are saved as PDFs for easy sharing and review. 

## 🧪 Reproducibility Notes
Assumptions and preprocessing steps:
- All dates are assumed to be in 2018
- Taxi zones and census tracts are spatially joined using WGS84
- Movement is normalized in thousands

