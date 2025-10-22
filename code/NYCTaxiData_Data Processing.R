# Loading Packages Needed 
library(data.table)
library(tidyverse)


# Loading "Uncleaned" Taxi Cab Data
# This data is NOT included in this GitHub, but can be downloaded from the TLC Trip Record Data Website (https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page) for 2018
# Note: Each month is downloaded separately due to the size limit 

January <- fread("2018_January_Taxi.csv", fill = TRUE)
February <- fread("2018_February_Taxi.csv", fill = TRUE)
March <- fread("2018_March_Taxi.csv", fill = TRUE,
               skip = 1) %>%
  rename(., VendorID = V1, tpep_pickup_datetime = V2, tpep_dropoff_datetime = V3, passenger_count = V4, 
         trip_distance = V5, RatecodeID = V6, store_and_fwd_flag = V7, PULocationID = V8, DOLocationID = V9, 
         payment_type = V10, fare_amount = V11, extra = V12, mta_tax = V13, tip_amount = V14, 
         tolls_amount = V15, improvement_surcharge = V16, total_amount = V17)
Unable to gather the header when I skipped lines using fread, therefore, I used the rename statement in tidyverse to rename the columns. 

April <- fread("2018_April_Taxi.csv", fill = TRUE)
May <- fread("2018_May_Taxi.csv", fill = TRUE)
June <- fread("2018_June_Taxi.csv", fill = TRUE)
July <- fread("2018_July_Taxi.csv", fill = TRUE)
August <- fread("2018_August_Taxi.csv", fill = TRUE)
September <- fread("2018_September_Taxi.csv", fill = TRUE)
October <- fread("2018_October_Taxi.csv", fill = TRUE)
November <- fread("2018_November_Taxi.csv", fill = TRUE)
December <- fread("2018_December_Taxi.csv", fill = TRUE)


# Cleaning Taxi Cab Data
# With how large the datasets are, when trying to pipe, the RAM space on my computer wasn't large enough and would crash after trying to pipe for > 15 minutes. Therefore, to obtain the data needed for my research, I used the "old-fashioned" way of coding and re-structuring the data. 

# January
January_Taxi <- January %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
January_Taxi <- tidyr::separate(January_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
January_Taxi <- tidyr::separate(January_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
January_Taxi$PU_Date <- as.Date(January_Taxi$PU_Date, "%m/%d/%Y")
January_Taxi$DO_Date <- as.Date(January_Taxi$DO_Date, "%m/%d/%Y")
January_Taxi$PU_Day <- weekdays(January_Taxi$PU_Date)
January_Taxi$DO_Day <- weekdays(January_Taxi$DO_Date)
Raw_January_Taxi <- January_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# February
February_Taxi <- February %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
February_Taxi <- tidyr::separate(February_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
February_Taxi <- tidyr::separate(February_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
February_Taxi$PU_Date <- as.Date(February_Taxi$PU_Date, "%m/%d/%Y")
February_Taxi$DO_Date <- as.Date(February_Taxi$DO_Date, "%m/%d/%Y")
February_Taxi$PU_Day <- weekdays(February_Taxi$PU_Date)
February_Taxi$DO_Day <- weekdays(February_Taxi$DO_Date)
Raw_February_Taxi <- February_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# March
March_Taxi <- March %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
March_Taxi <- tidyr::separate(March_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time"), sep = " ")
March_Taxi <- tidyr::separate(March_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time"), sep = " ")
March_Taxi$PU_Date <- as.Date(March_Taxi$PU_Date)
March_Taxi$DO_Date <- as.Date(March_Taxi$DO_Date)
March_Taxi$PU_Day <- weekdays(March_Taxi$PU_Date)
March_Taxi$DO_Day <- weekdays(March_Taxi$DO_Date)
Raw_March_Taxi <- March_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# April
April_Taxi <- April %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
April_Taxi <- tidyr::separate(April_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
April_Taxi <- tidyr::separate(April_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
April_Taxi$PU_Date <- as.Date(April_Taxi$PU_Date, "%m/%d/%Y")
April_Taxi$DO_Date <- as.Date(April_Taxi$DO_Date, "%m/%d/%Y")
April_Taxi$PU_Day <- weekdays(April_Taxi$PU_Date)
April_Taxi$DO_Day <- weekdays(April_Taxi$DO_Date)
Raw_April_Taxi <- April_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# May
May_Taxi <- May %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
May_Taxi <- tidyr::separate(May_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
May_Taxi <- tidyr::separate(May_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
May_Taxi$PU_Date <- as.Date(May_Taxi$PU_Date, "%m/%d/%Y")
May_Taxi$DO_Date <- as.Date(May_Taxi$DO_Date, "%m/%d/%Y")
May_Taxi$PU_Day <- weekdays(May_Taxi$PU_Date)
May_Taxi$DO_Day <- weekdays(May_Taxi$DO_Date)
Raw_May_Taxi <- May_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# June
June_Taxi <- June %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
June_Taxi <- tidyr::separate(June_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
June_Taxi <- tidyr::separate(June_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
June_Taxi$PU_Date <- as.Date(June_Taxi$PU_Date, "%m/%d/%Y")
June_Taxi$DO_Date <- as.Date(June_Taxi$DO_Date, "%m/%d/%Y")
June_Taxi$PU_Day <- weekdays(June_Taxi$PU_Date)
June_Taxi$DO_Day <- weekdays(June_Taxi$DO_Date)
Raw_June_Taxi <- June_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# July
July_Taxi <- July %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
July_Taxi <- tidyr::separate(July_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
July_Taxi <- tidyr::separate(July_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
July_Taxi$PU_Date <- as.Date(July_Taxi$PU_Date, "%m/%d/%Y")
July_Taxi$DO_Date <- as.Date(July_Taxi$DO_Date, "%m/%d/%Y")
July_Taxi$PU_Day <- weekdays(July_Taxi$PU_Date)
July_Taxi$DO_Day <- weekdays(July_Taxi$DO_Date)
Raw_July_Taxi <- July_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# August
August_Taxi <- August %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
August_Taxi <- tidyr::separate(August_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
August_Taxi <- tidyr::separate(August_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
August_Taxi$PU_Date <- as.Date(August_Taxi$PU_Date, "%m/%d/%Y")
August_Taxi$DO_Date <- as.Date(August_Taxi$DO_Date, "%m/%d/%Y")
August_Taxi$PU_Day <- weekdays(August_Taxi$PU_Date)
August_Taxi$DO_Day <- weekdays(August_Taxi$DO_Date)
Raw_August_Taxi <- August_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# September
September_Taxi <- September %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
September_Taxi <- tidyr::separate(September_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
September_Taxi <- tidyr::separate(September_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
September_Taxi$PU_Date <- as.Date(September_Taxi$PU_Date, "%m/%d/%Y")
September_Taxi$DO_Date <- as.Date(September_Taxi$DO_Date, "%m/%d/%Y")
September_Taxi$PU_Day <- weekdays(September_Taxi$PU_Date)
September_Taxi$DO_Day <- weekdays(September_Taxi$DO_Date)
Raw_September_Taxi <- September_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# October
October_Taxi <- October %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
October_Taxi <- tidyr::separate(October_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
October_Taxi <- tidyr::separate(October_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
October_Taxi$PU_Date <- as.Date(October_Taxi$PU_Date, "%m/%d/%Y")
October_Taxi$DO_Date <- as.Date(October_Taxi$DO_Date, "%m/%d/%Y")
October_Taxi$PU_Day <- weekdays(October_Taxi$PU_Date)
October_Taxi$DO_Day <- weekdays(October_Taxi$DO_Date)
Raw_October_Taxi <- October_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# November
November_Taxi <- November %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
November_Taxi <- tidyr::separate(November_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
November_Taxi <- tidyr::separate(November_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
November_Taxi$PU_Date <- as.Date(November_Taxi$PU_Date, "%m/%d/%Y")
November_Taxi$DO_Date <- as.Date(November_Taxi$DO_Date, "%m/%d/%Y")
November_Taxi$PU_Day <- weekdays(November_Taxi$PU_Date)
November_Taxi$DO_Day <- weekdays(November_Taxi$DO_Date)
Raw_November_Taxi <- November_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# December
December_Taxi <- December %>%
  select(., -c(VendorID, RatecodeID, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, 
               tolls_amount, improvement_surcharge))
December_Taxi <- tidyr::separate(December_Taxi, tpep_pickup_datetime, c("PU_Date", "PU_Time", "PU_TOD"), sep = " ")
December_Taxi <- tidyr::separate(December_Taxi, tpep_dropoff_datetime, c("DO_Date", "DO_Time", "DO_TOD"), sep = " ")
December_Taxi$PU_Date <- as.Date(December_Taxi$PU_Date, "%m/%d/%Y")
December_Taxi$DO_Date <- as.Date(December_Taxi$DO_Date, "%m/%d/%Y")
December_Taxi$PU_Day <- weekdays(December_Taxi$PU_Date)
December_Taxi$DO_Day <- weekdays(December_Taxi$DO_Date)
Raw_December_Taxi <- December_Taxi %>%
  mutate(PU_group = if_else(weekdays(PU_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"),
         DO_group = if_else(weekdays(DO_Date) %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday"))


# Save the Cleaned Data
save(Raw_January_Taxi, file = "January.csv")
save(Raw_February_Taxi, file = "February.csv")
save(Raw_March_Taxi, file = "March.csv")
save(Raw_April_Taxi, file = "April.csv")
save(Raw_May_Taxi, file = "May.csv")
save(Raw_June_Taxi, file = "June.csv")
save(Raw_July_Taxi, file = "July.csv")
save(Raw_August_Taxi, file = "August.csv")
save(Raw_September_Taxi, file = "September.csv")
save(Raw_October_Taxi, file = "October.csv")
save(Raw_November_Taxi, file = "November.csv")
save(Raw_December_Taxi, file = "December.csv")




# Load Data to Clean More

# January
January_TaxiCab <- get(load("January.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
January_TaxiCab <- subset(January_TaxiCab, January_TaxiCab$PULocationID != 264) 
January_TaxiCab <- subset(January_TaxiCab, January_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
January_TaxiCab <- subset(January_TaxiCab, January_TaxiCab$DOLocationID != 264) 
January_TaxiCab <- subset(January_TaxiCab, January_TaxiCab$DOLocationID != 265)


# February
February_TaxiCab <- get(load("February.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
February_TaxiCab <- subset(February_TaxiCab, February_TaxiCab$PULocationID != 264) 
February_TaxiCab <- subset(February_TaxiCab, February_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
February_TaxiCab <- subset(February_TaxiCab, February_TaxiCab$DOLocationID != 264) 
February_TaxiCab <- subset(February_TaxiCab, February_TaxiCab$DOLocationID != 265)


# March
March_TaxiCab <- get(load("March.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
March_TaxiCab <- subset(March_TaxiCab, March_TaxiCab$PULocationID != 264) 
March_TaxiCab <- subset(March_TaxiCab, March_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
March_TaxiCab <- subset(March_TaxiCab, March_TaxiCab$DOLocationID != 264) 
March_TaxiCab <- subset(March_TaxiCab, March_TaxiCab$DOLocationID != 265)


# April
April_TaxiCab <- get(load("April.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
April_TaxiCab <- subset(April_TaxiCab, April_TaxiCab$PULocationID != 264) 
April_TaxiCab <- subset(April_TaxiCab, April_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
April_TaxiCab <- subset(April_TaxiCab, April_TaxiCab$DOLocationID != 264) 
April_TaxiCab <- subset(April_TaxiCab, April_TaxiCab$DOLocationID != 265)


# May
May_TaxiCab <- get(load("May.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
May_TaxiCab <- subset(May_TaxiCab, May_TaxiCab$PULocationID != 264) 
May_TaxiCab <- subset(May_TaxiCab, May_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
May_TaxiCab <- subset(May_TaxiCab, May_TaxiCab$DOLocationID != 264) 
May_TaxiCab <- subset(May_TaxiCab, May_TaxiCab$DOLocationID != 265)


# June
June_TaxiCab <- get(load("June.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
June_TaxiCab <- subset(June_TaxiCab, June_TaxiCab$PULocationID != 264) 
June_TaxiCab <- subset(June_TaxiCab, June_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
June_TaxiCab <- subset(June_TaxiCab, June_TaxiCab$DOLocationID != 264) 
June_TaxiCab <- subset(June_TaxiCab, June_TaxiCab$DOLocationID != 265)


# July
July_TaxiCab <- get(load("July.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
July_TaxiCab <- subset(July_TaxiCab, July_TaxiCab$PULocationID != 264) 
July_TaxiCab <- subset(July_TaxiCab, July_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
July_TaxiCab <- subset(July_TaxiCab, July_TaxiCab$DOLocationID != 264) 
July_TaxiCab <- subset(July_TaxiCab, July_TaxiCab$DOLocationID != 265)


# August
August_TaxiCab <- get(load("August.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
August_TaxiCab <- subset(August_TaxiCab, August_TaxiCab$PULocationID != 264) 
August_TaxiCab <- subset(August_TaxiCab, August_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
August_TaxiCab <- subset(August_TaxiCab, August_TaxiCab$DOLocationID != 264) 
August_TaxiCab <- subset(August_TaxiCab, August_TaxiCab$DOLocationID != 265)


# September
September_TaxiCab <- get(load("September.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
September_TaxiCab <- subset(September_TaxiCab, September_TaxiCab$PULocationID != 264) 
September_TaxiCab <- subset(September_TaxiCab, September_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
September_TaxiCab <- subset(September_TaxiCab, September_TaxiCab$DOLocationID != 264) 
September_TaxiCab <- subset(September_TaxiCab, September_TaxiCab$DOLocationID != 265)


# October
October_TaxiCab <- get(load("October.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
October_TaxiCab <- subset(October_TaxiCab, October_TaxiCab$PULocationID != 264) 
October_TaxiCab <- subset(October_TaxiCab, October_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
October_TaxiCab <- subset(October_TaxiCab, October_TaxiCab$DOLocationID != 264) 
October_TaxiCab <- subset(October_TaxiCab, October_TaxiCab$DOLocationID != 265)


# November
November_TaxiCab <- get(load("November.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
November_TaxiCab <- subset(November_TaxiCab, November_TaxiCab$PULocationID != 264) 
November_TaxiCab <- subset(November_TaxiCab, November_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
November_TaxiCab <- subset(November_TaxiCab, November_TaxiCab$DOLocationID != 264) 
November_TaxiCab <- subset(November_TaxiCab, November_TaxiCab$DOLocationID != 265)


# December
December_TaxiCab <- get(load("December.csv"))

# Eliminate PU Locations in 264 and 265 as those are unknown Boroughs
December_TaxiCab <- subset(December_TaxiCab, December_TaxiCab$PULocationID != 264) 
December_TaxiCab <- subset(December_TaxiCab, December_TaxiCab$PULocationID != 265)

# Eliminate DO Locations in 264 and 265 as those are unknown Boroughs
December_TaxiCab <- subset(December_TaxiCab, December_TaxiCab$DOLocationID != 264) 
December_TaxiCab <- subset(December_TaxiCab, December_TaxiCab$DOLocationID != 265)



# Save the Final Cleaned Data to be read into the 
save(January_TaxiCab, file = "January_Final.csv")
save(February_TaxiCab, file = "February_Final.csv")
save(March_TaxiCab, file = "March_Final.csv")
save(April_TaxiCab, file = "April_Final.csv")
save(May_TaxiCab, file = "May_Final.csv")
save(June_TaxiCab, file = "June_Final.csv")
save(July_TaxiCab, file = "July_Final.csv")
save(August_TaxiCab, file = "August_Final.csv")
save(September_TaxiCab, file = "September_Final.csv")
save(October_TaxiCab, file = "October_Final.csv")
save(November_TaxiCab, file = "November_Final.csv")
save(December_TaxiCab, file = "December_Final.csv")





