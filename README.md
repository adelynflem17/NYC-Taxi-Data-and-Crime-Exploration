# NYC-Taxi-Data-and-Crime-Exploration
R code for analyzing NYC taxi trip data alongside crime statistics to uncover mobility-crime relationships. 

## 📁 Data Access and Structure
This repository supports the MS Report NYC Taxi Data and Crime Exploration, which focuses on analyzing spatial and temporal relationships between urban mobility and public safety. Due to file size constraints, data access is split between GitHub and Dropbox. 

🔹 GitHub-Hosted Datasets (/data folder)
These smaller datasets are available directly in this repository: 
- MovementData.xlsx:
- Population_data.xlsx:
- crime_counts_by_borough_month.xlsx:
You can view these files in the /data folder

🔹 Dropbox-Hosted Datasets (Full Taxi & Crime Records)
Due to GitHub's file limit, full datasets for the yellow taxi cab trips are hosted externally
- [Download full Excel files]([https://www.dropbox.com/s/your-shared-link](https://www.dropbox.com/scl/fo/mrss9gpi3mgna98h35mvd/AEDUoPpmHZoa_zKQ5yzL4Sk?rlkey=59xff6f7w8qceyu8ypjf7y5z8&st=0k4ph7ww&dl=0))
These include:
- NYC Taxi Trip Data: Raw trip-level records used for spatial joins and movement analysis
- NYPD Crime Complaint Data: Incident-level reports used for temporal and geographic aggregation

All files are read into R using readxl::read_excel() or data.table::fread() depending on format and size. 



