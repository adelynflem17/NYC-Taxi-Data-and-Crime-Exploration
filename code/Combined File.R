# Main driver script for NYC Taxi and Crime Analysis

# Load required packages to compile
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(purrr)
library(tidyr)
library(writexl)
library(tidyverse)
library(patchwork)
library(data.table)
library(hms)
library(classInt)
library(RColorBrewer)
library(cowplot)
library(glue)
library(qpdf)

# Set working directory to where you stored the downloaded data files
setwd("C:/Users/Adelyn/Desktop/MS Report Data")


# Read in the data sources ####
January_TaxiCab <- get(load("January_Final.csv"))
February_TaxiCab <- get(load("February_Final.csv"))
March_TaxiCab <- get(load("March_Final.csv"))
March_TaxiCab$PU_TOD <- ifelse(March_TaxiCab$PU_Time <= "11:59:59", "AM", "PM")
March_TaxiCab$DO_TOD <- ifelse(March_TaxiCab$DO_Time <= "11:59:59", "AM", "PM")
April_TaxiCab <- get(load("April_Final.csv"))
May_TaxiCab <- get(load("May_Final.csv"))
June_TaxiCab <- get(load("June_Final.csv"))
July_TaxiCab <- get(load("July_Final.csv"))
August_TaxiCab <- get(load("August_Final.csv"))
September_TaxiCab <- get(load("September_Final.csv"))
October_TaxiCab <- get(load("October_Final.csv"))
November_TaxiCab <- get(load("November_Final.csv"))
December_TaxiCab <- get(load("December_Final.csv"))
Raw_CrimeData <- fread("NYPD_Complaint_Data_Historic.csv", fill = TRUE)
Population <- read_excel("Population_data.xlsx")
Movement <- read_excel("MovementData.xlsx")
Crime <- read_excel("crime_counts_by_borough_month.xlsx")



# 02_NYCTaxiData_Combining_Plots.R ####

# Read shapefiles and transform to WGS84
fname <- unzip(zipfile = "taxi_zones.zip")
zones <- sf::st_read("./taxi_zones.shp")
zones <- sf::st_transform(zones, crs = 4326)
nyc_boroughs <- st_read("C:/Users/Adelyn/Desktop/MS Report Data/nybb.shp")

# Obtaining Spatial Outline for each Borough
Queens <- subset(nyc_boroughs, nyc_boroughs$BoroName == "Queens")
Brooklyn <- subset(nyc_boroughs, nyc_boroughs$BoroName == "Brooklyn")
Bronx <- subset(nyc_boroughs, nyc_boroughs$BoroName == "Bronx")
Manhattan <- subset(nyc_boroughs, nyc_boroughs$BoroName == "Manhattan")
StatenIsland <- subset(nyc_boroughs, nyc_boroughs$BoroName == "Staten Island")

# Obtaining Spatial Outline for each zone within a Borough
Queens_zones <- subset(zones, zones$borough == "Queens")
Brooklyn_zones <- subset(zones, zones$borough == "Brooklyn")
Bronx_zones <- subset(zones, zones$borough == "Bronx")
Manhattan_zones <- subset(zones, zones$borough == "Manhattan")
StatenIsland_zones <- subset(zones, zones$borough == "Staten Island")



# January

# Calculate the number of individuals who moved out of each zone in January
MoveOut_January <- subset(January_TaxiCab, January_TaxiCab$PULocationID != January_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Jan = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()

# Calculate the number of individuals who moved into of each zone in January
MoveIn_January <- subset(January_TaxiCab, January_TaxiCab$PULocationID != January_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Jan = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()

# Merge inbound and outbound movement data by LocationID
January_Month_Merged_Data <- merge(MoveIn_January, MoveOut_January, by = "LocationID")

# Calculate net movement: positive = more arrivals than departures, negative = more departures
January_Month_Merged_Data$Net_Movement <- January_Month_Merged_Data$Individuals_In_Jan - January_Month_Merged_Data$Individuals_Out_Jan


# Calculating individuals who moved into and out of each zone in February
MoveOut_February <- subset(February_TaxiCab, February_TaxiCab$PULocationID != February_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Feb = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_February <- subset(February_TaxiCab, February_TaxiCab$PULocationID != February_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Feb = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
February_Month_Merged_Data <- merge(MoveIn_February, MoveOut_February, by = "LocationID")
February_Month_Merged_Data$Net_Movement <- February_Month_Merged_Data$Individuals_In_Feb - February_Month_Merged_Data$Individuals_Out_Feb


# Calculating individuals who moved into and out of each zone in March
MoveOut_March <- subset(March_TaxiCab, March_TaxiCab$PULocationID != March_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_March = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_March <- subset(March_TaxiCab, March_TaxiCab$PULocationID != March_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_March = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
March_Month_Merged_Data <- merge(MoveIn_March, MoveOut_March, by = "LocationID")
March_Month_Merged_Data$Net_Movement <- March_Month_Merged_Data$Individuals_In_March - March_Month_Merged_Data$Individuals_Out_March


# Calculating individuals who moved into and out of each zone in April
MoveOut_April <- subset(April_TaxiCab, April_TaxiCab$PULocationID != April_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_April = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_April <- subset(April_TaxiCab, April_TaxiCab$PULocationID != April_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_April = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
April_Month_Merged_Data <- merge(MoveIn_April, MoveOut_April, by = "LocationID")
April_Month_Merged_Data$Net_Movement <- April_Month_Merged_Data$Individuals_In_April - April_Month_Merged_Data$Individuals_Out_April


# Calculating individuals who moved into and out of each zone in May
MoveOut_May <- subset(May_TaxiCab, May_TaxiCab$PULocationID != May_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_May = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_May <- subset(May_TaxiCab, May_TaxiCab$PULocationID != May_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_May = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
May_Month_Merged_Data <- merge(MoveIn_May, MoveOut_May, by = "LocationID")
May_Month_Merged_Data$Net_Movement <- May_Month_Merged_Data$Individuals_In_May - May_Month_Merged_Data$Individuals_Out_May


# Calculating individuals who moved into and out of each zone in June
MoveOut_June <- subset(June_TaxiCab, June_TaxiCab$PULocationID != June_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_June = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_June <- subset(June_TaxiCab, June_TaxiCab$PULocationID != June_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_June = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
June_Month_Merged_Data <- merge(MoveIn_June, MoveOut_June, by = "LocationID")
June_Month_Merged_Data$Net_Movement <- June_Month_Merged_Data$Individuals_In_June - June_Month_Merged_Data$Individuals_Out_June


# Calculating individuals who moved into and out of each zone in July
MoveOut_July <- subset(July_TaxiCab, July_TaxiCab$PULocationID != July_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_July = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_July <- subset(July_TaxiCab, July_TaxiCab$PULocationID != July_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_July = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
July_Month_Merged_Data <- merge(MoveIn_July, MoveOut_July, by = "LocationID")
July_Month_Merged_Data$Net_Movement <- July_Month_Merged_Data$Individuals_In_July - July_Month_Merged_Data$Individuals_Out_July


# Calculating individuals who moved into and out of each zone in August
MoveOut_August <- subset(August_TaxiCab, August_TaxiCab$PULocationID != August_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Aug = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_August <- subset(August_TaxiCab, August_TaxiCab$PULocationID != August_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Aug = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
August_Month_Merged_Data <- merge(MoveIn_August, MoveOut_August, by = "LocationID")
August_Month_Merged_Data$Net_Movement <- August_Month_Merged_Data$Individuals_In_Aug - August_Month_Merged_Data$Individuals_Out_Aug


# Calculating individuals who moved into and out of each zone in September
MoveOut_September <- subset(September_TaxiCab, September_TaxiCab$PULocationID != September_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Sept = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_September <- subset(September_TaxiCab, September_TaxiCab$PULocationID != September_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Sept = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
September_Month_Merged_Data <- merge(MoveIn_September, MoveOut_September, by = "LocationID")
September_Month_Merged_Data$Net_Movement <- September_Month_Merged_Data$Individuals_In_Sept - September_Month_Merged_Data$Individuals_Out_Sept


# Calculating individuals who moved into and out of each zone in October
MoveOut_October <- subset(October_TaxiCab, October_TaxiCab$PULocationID != October_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Oct = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_October <- subset(October_TaxiCab, October_TaxiCab$PULocationID != October_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Oct = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
October_Month_Merged_Data <- merge(MoveIn_October, MoveOut_October, by = "LocationID")
October_Month_Merged_Data$Net_Movement <- October_Month_Merged_Data$Individuals_In_Oct - October_Month_Merged_Data$Individuals_Out_Oct


# Calculating individuals who moved into and out of each zone in November
MoveOut_November <- subset(November_TaxiCab, November_TaxiCab$PULocationID != November_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Nov = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_November <- subset(November_TaxiCab, November_TaxiCab$PULocationID != November_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Nov = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
November_Month_Merged_Data <- merge(MoveIn_November, MoveOut_November, by = "LocationID")
November_Month_Merged_Data$Net_Movement <- November_Month_Merged_Data$Individuals_In_Nov - November_Month_Merged_Data$Individuals_Out_Nov


# Calculating individuals who moved into and out of each zone in December
MoveOut_December <- subset(December_TaxiCab, December_TaxiCab$PULocationID != December_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Dec = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_December <- subset(December_TaxiCab, December_TaxiCab$PULocationID != December_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Dec = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
December_Month_Merged_Data <- merge(MoveIn_December, MoveOut_December, by = "LocationID")
December_Month_Merged_Data$Net_Movement <- December_Month_Merged_Data$Individuals_In_Dec - December_Month_Merged_Data$Individuals_Out_Dec



# Set the borough you want to analyze
Borough_Zone <- Brooklyn_zones

# Merge monthly movement data with spatial zone geometries 
January_merged_data <- merge(Borough_Zone, January_Month_Merged_Data, by = "LocationID")
February_merged_data <- merge(Borough_Zone, February_Month_Merged_Data, by = "LocationID")
March_merged_data <- merge(Borough_Zone, March_Month_Merged_Data, by = "LocationID")
April_merged_data <- merge(Borough_Zone, April_Month_Merged_Data, by = "LocationID")
May_merged_data <- merge(Borough_Zone, May_Month_Merged_Data, by = "LocationID")
June_merged_data <- merge(Borough_Zone, June_Month_Merged_Data, by = "LocationID")
July_merged_data <- merge(Borough_Zone, July_Month_Merged_Data, by = "LocationID")
August_merged_data <- merge(Borough_Zone, August_Month_Merged_Data, by = "LocationID")
September_merged_data <- merge(Borough_Zone, September_Month_Merged_Data, by = "LocationID")
October_merged_data <- merge(Borough_Zone, October_Month_Merged_Data, by = "LocationID")
November_merged_data <- merge(Borough_Zone, November_Month_Merged_Data, by = "LocationID")
December_merged_data <- merge(Borough_Zone, December_Month_Merged_Data, by = "LocationID")


# Function to create a net movement map for a given month
create_net_movement_plot <- function(data, borough, month_name, borough_name = "Brooklyn", year = 2018) {
  # Return an empty plot if data is missing or empty
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + theme_void() + ggtitle(month_name))
  }
  
  # Rank zones by net movement and assign categories for coloring
  Monthly_data_net <- data %>%
    mutate(rank = dense_rank(desc(Net_Movement))) %>%
    mutate(Net_Movement_Category = case_when(
      is.na(Net_Movement) ~ "Missing",
      Net_Movement == 0 ~ "Zero",
      rank <= 5 ~ "red",
      rank <= 10 ~ "orange",
      Net_Movement > 0 & rank > 10 ~ "yellow",
      Net_Movement < 0 ~ "gray",
      TRUE ~ "gray"
    )) %>%
    mutate(Net_Movement_Category = factor(Net_Movement_Category, 
                                          levels = c("red", "orange", "yellow", "gray", "Zero", "Missing")))
  
  # Prepare label data for top zones
  label_data <- Monthly_data_net %>%
    filter(Net_Movement_Category %in% c("red", "orange"))
  
  # Build the map
  ggplot() +
    geom_sf(data = borough, fill = "white", show.legend = FALSE) + 
    geom_sf(data = Monthly_data_net, aes(geometry = geometry, fill = Net_Movement_Category),
            color = "black", size = 0.5, show.legend = TRUE) +
    geom_sf_label(data = label_data, aes(geometry = geometry, label = Net_Movement), 
                  fill = "white", color = "black", size = 1.5, label.size = 0.3,
                  show.legend = FALSE) +
    scale_fill_manual(name = "Net Movement Scale", 
                      values = c("red" = "red", "orange" = "orange", 
                                 "yellow" = "yellow", "gray" = "gray80", "Zero" = "#B0E0E6", "Missing" = "white"),
                      labels = c("red" = "Top 5", "orange" = "Top 6–10", 
                                 "yellow" = "Positive Movement", "gray" = "Negative Movement",
                                 "Zero" = "Zero Movement", "Missing" = "Missing Data"),
                      drop = FALSE) +
    ggtitle(month_name) +
    theme_void() +
    theme(
      plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 8),       # Shrinks legend title text
      legend.text = element_text(size = 7),        # Shrinks legend label text
      legend.key.size = unit(0.4, "cm"),           # Shrinks legend box size
      legend.spacing.y = unit(0.2, "cm")
    )
}

# Create a named list of monthly merged datasets
month_list <- list(
  January = January_merged_data, February = February_merged_data, March = March_merged_data, 
  April = April_merged_data, May = May_merged_data, June = June_merged_data, July = July_merged_data,
  August = August_merged_data, September = September_merged_data, October = October_merged_data,
  November = November_merged_data, December = December_merged_data
)

# Generate a list of plots for each month using map2
plots <- map2(month_list, names(month_list), ~ create_net_movement_plot(.x, Borough_Zone, .y, borough_name = "Brooklyn", year = 2018))

# Combine all monthly plots into a single layout
combined_plot <- wrap_plots(plots, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

# Add a title annotation to the combined plot
final_plot <- combined_plot +
  plot_annotation(
    title = "Brooklyn 2018: Monthly Net Movement by Zone",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )

# Save the final annotated plot to PDF to a file folder 
#ggsave("NetMovement_Combined_Brooklyn.pdf", final_plot, width = 11, height = 8.5, units = "in", dpi = 300)



# 03_NYCCrimeData_Census_Tract_Zone_Overlay.R ####

## Census Tract Zone Overlay
map_taxi_zones_by_borough <- function(zone_path, borough_path, tract_path, borough_name, clip = TRUE, label_areas = NULL) {
    
# Read shapefiles and transform data to WGS84
zones <- st_read(zone_path) %>% st_transform(crs = 4326)
boroughs <- st_read(borough_path) %>% st_transform(crs = 4326)
tracts <- st_read(tract_path) %>% st_transform(crs = 4326)
    
# Ensure geometries are valid
zones <- st_make_valid(zones)
tracts <- st_make_valid(tracts)
    
# Filter data for selected borough
selected_borough <- boroughs %>% filter(BoroName == borough_name)
zone_borough <- zones %>% filter(borough == borough_name)
    
# Handle inconsistent column naming in census tract shapefile
tract_col <- if ("boroname" %in% colnames(tracts)) "boroname" else "BoroName"
tracts_borough <- tracts %>% filter(.data[[tract_col]] == borough_name)
    
# Align CRS before spatial operations
selected_borough <- st_transform(selected_borough, st_crs(zone_borough))
    
# Optionally clip zones and tracts to borough boundary
if (clip) {
  zone_borough <- st_intersection(zone_borough, selected_borough)
  tracts_borough <- st_intersection(tracts_borough, selected_borough)
}
    
# Fail-safe: stop if no tracts found
if (!exists("tracts_borough") || nrow(tracts_borough) == 0) {
  stop("❌ tracts_borough is missing or empty. Check column names and borough_name.")
}
    
# Prepare external labels for highlighted zones
if (!is.null(label_areas)) {
  highlighted_zones <- zone_borough %>% filter(zone %in% label_areas)
  highlighted_zones$centroid <- st_point_on_surface(highlighted_zones$geometry)
  coords <- st_coordinates(highlighted_zones$centroid)
  highlighted_zones$lon <- coords[,1]
  highlighted_zones$lat <- coords[,2]
      
# Get the bounding box to position labels outside map
  bbox <- st_bbox(selected_borough)
  highlighted_zones$label_x <- bbox["xmax"] + 0.01
  highlighted_zones$label_y <- highlighted_zones$lat
      
# Optional: stagger labels vertically to reduce overlap
  highlighted_zones <- highlighted_zones %>%
    arrange(desc(lat)) %>%
    mutate(label_y = label_y + seq(-0.01, 0.01, length.out = n()))
  }
    
# Build layered ggplot
p <- ggplot() +
# Census tracts first so they appear beneath everything
geom_sf(data = tracts_borough, fill = NA, color = "gray60", size = 0.3) +
      
# Fill selected zones with light blue
  { if (!is.null(label_areas)) geom_sf(data = highlighted_zones, fill = "lightblue", color = NA) } +
  
# overlay census tract outlines again so they appear on top of the blue
  geom_sf(data = tracts_borough, fill = NA, color = "gray60", size = 0.3) +
      
# Remaining taxi zones
  geom_sf(data = zone_borough, fill = NA, color = "black", size = 0.4) +
      
# Borough outline
  geom_sf(data = selected_borough, fill = NA, color = "black", size = 1) +
  theme_void() +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"] + 0.05), clip = "off") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))
    
# Add external labels and arrows
  if (!is.null(label_areas)) {
    p <- p +
    geom_segment(data = highlighted_zones,
                 aes(x = label_x, y = label_y, xend = lon, yend = lat),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = "blue", linewidth = 0.2) +
    geom_text(data = highlighted_zones,
                aes(x = label_x, y = label_y, label = zone),
                size = 2, fontface = "bold", hjust = 0, color = "blue")
    }
    
    return(p)
  }


# Each section below generates a map for a specific borough with custom zone labels

# Queens
queens_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS Report Data/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS Report Data/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS Report Data/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Queens",
  clip = TRUE,
  label_areas = c("Astoria", "Steinway", "Jackson Heights", "Forest Hills", "Long Island City & Hunters Point")
)
plot(queens_map)


# Manhattan
manhattan_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS Report Data/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS Report Data/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS Report Data/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Manhattan",
  clip = TRUE,
  label_areas = c("East Harlem North", "East Harlem South", "Two Bridges/Seward Park", "West Chelsea/Hudson Yards")
)
plot(manhattan_map)

# Staten Island
StatenIsland_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS Report Data/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS Report Data/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS Report Data/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Staten Island",
  clip = TRUE,
  label_areas = c("Bloomfield/Emerson Hill", "Stapleton")
)
plot(StatenIsland_map)

# Brooklyn
Brooklyn_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS Report Data/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS Report Data/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS Report Data/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Brooklyn",
  clip = TRUE,
  label_areas = c("Brooklyn Heights", "Greenpoint", "Park Slope", "Williamsburg (North Side)", "Williamsburg (South Side)")
)
plot(Brooklyn_map)

# The Bronx
Bronx_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS Report Data/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS Report Data/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS Report Data/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Bronx",
  clip = TRUE,
  label_areas = c("East Concourse/Concourse Village", "Mott Haven/Port Morris", "Spuyten Duyvil/Kingsbridge")
)
plot(Bronx_map)
  
  
  

# 04_NYCCrimeData_Combined_Visual.R ####

# Clean and transform raw NYPD crime data
CrimeData <- Raw_CrimeData %>%
  select(., -c(CMPLNT_NUM, ADDR_PCT_CD, RPT_DT, KY_CD, PD_CD, CRM_ATPT_CPTD_CD, LOC_OF_OCCUR_DESC, 
               PREM_TYP_DESC, JURIS_DESC, JURISDICTION_CODE, PARKS_NM, HADEVELOPT, HOUSING_PSA, X_COORD_CD,
               Y_COORD_CD, SUSP_AGE_GROUP, SUSP_RACE, SUSP_SEX, TRANSIT_DISTRICT, PATROL_BORO, STATION_NAME,
               VIC_AGE_GROUP, VIC_RACE, VIC_SEX)) %>%      # Remove unnecessary columns to simplify the dataset
  filter(., Latitude != "NA") %>%                          # Filter out rows with missing latitude values
  mutate(CMPLNT_FR_DT = as.Date(CMPLNT_FR_DT, "%m/%d/%Y"),
         CMPLNT_TO_DT = as.Date(CMPLNT_TO_DT, "%m/%d/%Y"),
         CMPLNT_DAY = weekdays(CMPLNT_FR_DT),
         CMPLNT_GRP = if_else(weekdays(CMPLNT_FR_DT) %in% c("Saturday", "Sunday"),
                              "Weekend", "Weekday"),
         Start_Month = month(CMPLNT_FR_DT),
         End_Month = month(CMPLNT_TO_DT)) %>%              # Convert data columns and extract weekday/month info
  rename(., Lat = Latitude, Lon = Longitude)

# Convert complaint time to MHS format and extract hour-minute
CrimeData$CMPLNT_FR_TM <- as_hms(CrimeData$CMPLNT_FR_TM)
CrimeData$HourMin <- format(strptime(CrimeData$CMPLNT_FR_TM, "%H:%M:%S"), '%H:%M')



# Function to create weekday-hour heatmap for a specific crime type
create_overall_crime_heatmap <- function(CrimeData, crime_type) {
# Filter for the specified crime type and extract hour
Crime <- CrimeData %>%
  filter(OFNS_DESC == crime_type) %>%
  mutate(
    Hour = format(strptime(HourMin, "%H:%M"), '%H'),
    Hour = as.integer(Hour)
  )
  
# Add placeholder row for 24:00 to complete the hourly axis
all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
extra_row <- data.frame(CMPLNT_DAY = all_days, Hour = 24, n = NA_integer_)
  
# Aggregate crime counts by weekday and hour
grouped <- Crime %>%
  group_by(CMPLNT_DAY, Hour) %>%
  summarise(n = n(), .groups = "drop") %>%
  bind_rows(extra_row) %>%
  filter(!is.na(n)) %>%
  mutate(                        # Normalize by number of weeks (Monday has 52, others 52 in dataset)
    Avg_Occ_All = ifelse(CMPLNT_DAY == "Monday", n / 53, n / 52),
    Avg_Occ_All = round(Avg_Occ_All, 2)
  )
  
# Create quantile bins for average occurrences
breaks <- quantile(grouped$Avg_Occ_All, probs = seq(0, 1, by = 0.1), type = 1)
labels <- map_chr(seq_along(breaks[-1]), function(i) {
  fmt <- function(x) formatC(x, digits = 2, format = "f")
  if (i == 1) glue::glue("[{fmt(breaks[i])}-{fmt(breaks[i + 1])}]") 
  else glue::glue("({fmt(breaks[i])}-{fmt(breaks[i + 1])}]")
})
  
# Assign each cell to a bin category
grouped <- grouped %>%
  mutate(Avg_Occ_Category = cut(Avg_Occ_All, breaks = breaks, labels = labels, include.lowest = TRUE))
  
# Generate heatmap plot
ggplot(grouped, aes(factor(CMPLNT_DAY, level = all_days), Hour, fill = Avg_Occ_Category)) +
  geom_tile(color = "black", linewidth = 0.3) +
  theme_classic() +
  scale_fill_manual(
    values = rev(brewer.pal(10, "BrBG")),
    breaks = rev(levels(grouped$Avg_Occ_Category))
  ) +
  labs(x = "Day of Week", y = "Time of Day (hours)", fill = "Avg Occurrences") +
  scale_x_discrete(labels = c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")) +
  scale_y_reverse(
    breaks = seq(-0.5, 23.5, 1),
    labels = sprintf("%02d:00", 0:24),
    limits = c(23.5, -0.5)
  ) +
  theme(
    axis.text.x = element_text(size = 7, angle = 40, vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(size = 7),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10, margin = margin(b = -5)),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, "cm")
  )
}

# Generate and save a heatmap for a particular crime - here you can input the crime of your choice; I have the code looking at GRAND LARCENY 
overall_plot <- create_overall_crime_heatmap(CrimeData, "GRAND LARCENY")
overall_plot

# Save the plot
#ggsave("Overall_Heatmap_GrandLarceny.pdf",
#       plot = overall_plot,
#       width = 7,
#       height = 9.5,
#       units = "in",
#       dpi = 300)




# Create weekday-hour heatmaps for crime counts by borough
create_crime_heatmaps <- function(CrimeData, crime_type) {
# Filter the dataset for the selected crime type and extract the hour
Crime <- CrimeData %>%
  filter(OFNS_DESC == crime_type) %>%
  mutate(
    Hour = format(strptime(HourMin, "%H:%M"), '%H'),
    Hour = as.integer(Hour)
  )
  
# Add placeholder row for 24:00 to complete hourly axis
all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
extra_row <- data.frame(CMPLNT_DAY = all_days, Hour = 24, n = NA_integer_)
  
# Helper function toaggregate and bin data
generate_heat_data <- function(data) {
  grouped <- data %>%
    group_by(CMPLNT_DAY, Hour) %>%
    summarise(n = n(), .groups = "drop") %>%
    bind_rows(extra_row) %>%
    filter(!is.na(n)) %>%
    mutate(                      # Normalize by number of weeks (Monday has 53, others 52)
      Avg_Occ_All = ifelse(CMPLNT_DAY == "Monday", n / 53, n / 52),
      Avg_Occ_All = round(Avg_Occ_All, 2)
    )
  
# Create quantile-based bins for average occurrences
  breaks <- quantile(grouped$Avg_Occ_All, probs = seq(0, 1, by = 0.1), type = 1)
  labels <- map_chr(seq_along(breaks[-1]), function(i) {
    fmt <- function(x) formatC(x, digits = 2, format = "f")
    if (i == 1) glue::glue("[{fmt(breaks[i])}-{fmt(breaks[i + 1])}]") 
    else glue::glue("({fmt(breaks[i])}-{fmt(breaks[i + 1])}]")
  })
  
  grouped %>%
    mutate(Avg_Occ_Category = cut(Avg_Occ_All, breaks = breaks, labels = labels, include.lowest = TRUE))
}

# Helper function to plot a heatmap from binned data
plot_heatmap <- function(df, title) {
  ggplot(df, aes(factor(CMPLNT_DAY, level = all_days), Hour, fill = Avg_Occ_Category)) +
    geom_tile(color = "black", linewidth = 0.3) +
    theme_classic() +
    scale_fill_manual(
      values = rev(brewer.pal(10, "BrBG")),
      breaks = rev(levels(df$Avg_Occ_Category))
    ) +
    labs(x = "Day of Week", y = "Time of Day (hours)", title = title, fill = "Avg Occurrences") +
    scale_x_discrete(labels = c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")) +
    scale_y_reverse(
      breaks = seq(-0.5, 23.5, 1),
      labels = sprintf("%02d:00", 0:24),
      limits = c(23.5, -0.5)
    ) + 
    #scale_y_continuous(
    #  breaks = seq(-0.5, 23.5, 1),
    #  labels = sprintf("%02d:00", 0:24),
    #  limits = c(-0.5, 23.5)
    #) +
    theme(
      axis.text.x = element_text(size = 7, angle = 40, vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 10, margin = margin(b = -5)),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.3, "cm")
    )
}
  
# Define boroughs and their display names
boroughs <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
borough_labels <- c("BRONX" = "The Bronx", 
                    "BROOKLYN" = "Brooklyn", 
                    "MANHATTAN" = "Manhattan", 
                    "QUEENS" = "Queens", 
                    "STATEN ISLAND" = "Staten Island")

# Create overall heatmap (all boroughs combined)
plots[["Overall"]] <- Crime %>%
  generate_heat_data() %>%
  plot_heatmap(title = paste("Overall"))

# Create heatmap for each borough
for (b in boroughs) {
  df_boro <- Crime %>% filter(BORO_NM == b)
  if (nrow(df_boro) > 0) {
    pretty_name <- borough_labels[[b]]
    plots[[b]] <- df_boro %>%
      generate_heat_data() %>%
      plot_heatmap(title = paste(pretty_name))
  }
}
return(plots)
}

# Generate heatmaps for a specific crime
plots <- create_crime_heatmaps(CrimeData, "HARRASSMENT 2")

# Combine all plots into a grid layout
grid <- cowplot::plot_grid(plotlist = plots, ncol = 2)

# Save combined plot to PDF
#ggsave("Combined_Heatmaps_Borough_Harassment.pdf",
#       plot = grid,
#       width = 7,
#       height = 9.5,
#       units = "in",
#       dpi = 300)



# Create weekday-hour heatmap panels for monthly crime counts
create_grand_larceny_monthly_heatmaps <- function(CrimeData, output_file = "Monthly_Harassment_Panels.pdf", save_pdf = TRUE) {
  
# Define weekday order
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
# Define the number of each weekday per month (used for averaging)
weekday_averaging <- list(
  "January"   = c(Monday = 5, Tuesday = 5, Wednesday = 5, Thursday = 4, Friday = 4, Saturday = 4, Sunday = 4),
  "February"  = c(Monday = 4, Tuesday = 4, Wednesday = 4, Thursday = 4, Friday = 4, Saturday = 4, Sunday = 4),
  "March"     = c(Monday = 4, Tuesday = 4, Wednesday = 4, Thursday = 5, Friday = 5, Saturday = 5, Sunday = 4),
  "April"     = c(Monday = 5, Tuesday = 4, Wednesday = 4, Thursday = 4, Friday = 4, Saturday = 4, Sunday = 5),
  "May"       = c(Monday = 4, Tuesday = 5, Wednesday = 5, Thursday = 5, Friday = 4, Saturday = 4, Sunday = 4),
  "June"      = c(Monday = 4, Tuesday = 4, Wednesday = 4, Thursday = 4, Friday = 5, Saturday = 5, Sunday = 4),
  "July"      = c(Monday = 5, Tuesday = 5, Wednesday = 4, Thursday = 4, Friday = 4, Saturday = 4, Sunday = 5),
  "August"    = c(Monday = 4, Tuesday = 4, Wednesday = 5, Thursday = 5, Friday = 5, Saturday = 4, Sunday = 4),
  "September" = c(Monday = 4, Tuesday = 4, Wednesday = 4, Thursday = 4, Friday = 4, Saturday = 5, Sunday = 5),
  "October"   = c(Monday = 5, Tuesday = 5, Wednesday = 5, Thursday = 4, Friday = 4, Saturday = 4, Sunday = 4),
  "November"  = c(Monday = 4, Tuesday = 4, Wednesday = 4, Thursday = 5, Friday = 5, Saturday = 4, Sunday = 4),
  "December"  = c(Monday = 5, Tuesday = 4, Wednesday = 4, Thursday = 4, Friday = 4, Saturday = 5, Sunday = 5)
)
  
# Filter for specific crime and extract hour/month
Crime <- CrimeData %>%
  filter(OFNS_DESC == "HARRASSMENT 2") %>%
  mutate(
    Hour = format(strptime(HourMin, "%H:%M"), "%H") %>% as.integer(),
    Month = month.name[Start_Month]
  )
  
# Function to generate a single month's heatmap panel
generate_month_panel <- function(data, month = NULL) {
  df <- if (!is.null(month)) filter(data, Month == month) else data
  if (nrow(df) == 0) return(NULL)
    
# Use month-specific weekday counts for averaging
  avg_days <- if (is.null(month)) {
    c(Monday = 53, Tuesday = 52, Wednesday = 52, Thursday = 52, Friday = 52, Saturday = 52, Sunday = 52)
  } else {
     weekday_averaging[[month]]
   }
   names(avg_days) <- days
    
# Aggregate and normalize by weekday frequency 
  summarized <- df %>%
    group_by(CMPLNT_DAY, Hour) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Avg_Occ = round(n / avg_days[CMPLNT_DAY], 2)) 
  
# Create quantile bins for average occurrences
  breaks <- quantile(summarized$Avg_Occ, probs = seq(0, 1, by = 0.1), type = 1)
  labels <- character(length(breaks) - 1)
  for (i in seq_along(labels)) {
    if (i == 1) {
      labels[i] <- sprintf("[%.2f-%.2f]", breaks[i], breaks[i + 1])
    } else {
      labels[i] <- sprintf("(%.2f-%.2f]", breaks[i], breaks[i + 1])
    }
  }
    
# Assign bin categories
  summarized <- summarized %>%
    mutate(Avg_Occ_Category = factor(
      cut(Avg_Occ, breaks = breaks, labels = labels, include.lowest = TRUE),
      levels = rev(labels)  # descending legend
    ))
  
# Generate heatmap
  ggplot(summarized, aes(factor(CMPLNT_DAY, level = days), Hour, fill = Avg_Occ_Category)) +
    geom_tile(color = "black", linewidth = 0.2) +
    scale_fill_manual(values = rev(brewer.pal(10, "BrBG"))) +
    scale_x_discrete(labels = substr(days, 1, 2)) +
    scale_y_reverse(
      breaks = seq(-0.5, 23.5, 1),
      labels = sprintf("%02d:00", 0:24),
      limits = c(23.5, -0.5)
    ) + 
    #scale_y_continuous(
    #  breaks = seq(-0.5, 23.5, 1),
    #  labels = sprintf("%02d:00", 0:24),
    #  limits = c(-0.5, 24)) +
    labs(title = ifelse(is.null(month), "Overall", month), x = "Day of Week", y = "Time of Day (hours)", fill = "Avg Occurrences") +
    theme_classic(base_size = 8) +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5, margin = margin(b = -4)),
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 6),
      axis.text.x = element_text(size = 5, angle = 40, vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 4),
      legend.title = element_text(size = 5),
      legend.text = element_text(size = 4),
      legend.key.size = unit(0.3, "cm")
    )
}
  
# Generate all panels: overall + monthly
plots <- list()
plots[["Overall"]] <- generate_month_panel(Crime)
  
for (m in month.name) {
  if (m %in% Crime$Month) {
    plots[[m]] <- generate_month_panel(Crime, m)
  }
}
  
# Create blank placeholder for layout symmetry
blank_plot <- ggplot() + theme_void()
  
# Arrange plots in a 5-row x 3-column grid
ordered_plots <- list(
  blank_plot, plots$Overall, blank_plot,
  plots$January, plots$February, plots$March,
  plots$April, plots$May, plots$June,
  plots$July, plots$August, plots$September,
  plots$October, plots$November, plots$December
)

layout <- matrix(1:15, ncol = 3, byrow = TRUE)
  
# Combine all plots into one panel
panel <- cowplot::plot_grid(plotlist = ordered_plots, ncol = 3)
  
# Save to PDF if needed
if (save_pdf) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_file, panel, width = 8.5, height = 11, dpi = 300, units = "in")
}
  
return(panel)
}

# Return function and save manually
p <- create_grand_larceny_monthly_heatmaps(CrimeData, save_pdf = FALSE)
#ggsave("Harassment_Heatmaps_Manual.pdf", plot = p, width = 7, height = 9.5, units = "in", dpi = 300) 
  


# 05_NYCCrimeData_Correlation_Plots.R ####

# Read in data and combine individual datasets into one
Crime <- Crime %>%
  select(-geometry) %>%                  # Drop geometry column if present
  rename(Month = Start_Month) %>%        # Rename month column for consistency
  mutate(Month = as.numeric(Month))      # Ensure Month is numeric
Population <- Population %>%
  select(-geometry)                      # Drop geometry column
Movement <- Movement %>%
  mutate(Month = as.numeric(Month))      # Ensure Month is numeric

# Create a complete grid of all LocationID-month combinations
all_combinations <- expand.grid(LocationID = unique(Crime$LocationID), Month = 1:12)

# Merge crime data with full grid to ensure all combinations are present
complete_crime <- all_combinations %>%
  left_join(Crime, by = c("LocationID", "Month"))

# Merge with movement data
merged_data <- complete_crime %>%
  full_join(Movement, by = c("LocationID", "Month"))

# Merge with population data and clean offense decriptions
final_merged_data <- merged_data %>%
  left_join(Population, by = c("LocationID", "borough")) %>%
  mutate(OFNS_DESC = recode(OFNS_DESC, "ASSAULT 3 & RELATED OFFENSES" = "Assault 3", "CRIMINAL MISCHIEF & RELATED OF" = "Criminal Mischief",
                            "GRAND LARCENY" = "Grand Larceny", "HARRASSMENT 2" = "Harassment", "PETIT LARCENY" = "Petit Larceny")) %>%
  filter(!(LocationID %in% c(132, 138)))          # Remove outlier or invalid zones

# Normalize population and movement values (in thousands)
final_merged_data$Population <- final_merged_data$Population / 1000
final_merged_data$Individuals_In <- final_merged_data$Individuals_In / 1000
final_merged_data$Individuals_Out <- final_merged_data$Individuals_Out / 1000

# Standardize borough name
final_merged_data$borough[final_merged_data$borough == "Bronx"] <- "The Bronx"



# Function to generate scatter plots for a given borough and month
create_crime_plots <- function(data, month, Borough) {
  month_names <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")
  month_name <- month_names[month]
  filtered_data <- data %>%
    filter(Month == month, borough == Borough)
  
  print(filtered_data) 
  
# Define crime types to plot
crime_order <- c("Petit Larceny", "Harassment", "Assault 3", "Criminal Mischief", "Grand Larceny")
  
# Create a PDF file for the plots
pdf(paste0("Crime Correlation Plots_", month, "_", Borough, ".pdf"), width = 8, height = 11)
  
# Set up multi-panel layout: 6 rows x 3 columns
par(mfrow = c(6, 3), mar = c(3.5, 4, 1, 0.5), mgp = c(2, 1, 0))
  
# Loop through each crime type and create 3 scatter plots
for (crime in crime_order) {
  crime_data <- filtered_data %>% filter(OFNS_DESC == crime)
    
  if (nrow(crime_data) > 0) {
    # Plot 1: Population vs Crime Count
    plot(crime_data$Population, crime_data$crime_count,
         xlab = "Population Count (thousands)", ylab = paste("# of", crime, "Crimes"),
         pch = 19, xlim = c(0, 40), ylim = c(0, 50), cex.axis = 0.9, cex.lab = 0.9)
    abline(lm(crime_count ~ Population, data = crime_data), col = "blue")
    legend("topright", legend = paste("r = ", sprintf("%.3f", cor(crime_data$Population, crime_data$crime_count, use = "complete.obs"))),
           bty = "n", cex = 0.8, text.col = "black")
      
    # Plot 2: Individuals In vs Crime Count
    plot(crime_data$Individuals_In, crime_data$crime_count,
         xlab = "Move Into Zone (thousands)", ylab = paste("# of", crime, "Crimes"),
         pch = 19, xlim = c(0, 0.4), ylim = c(0, 50), cex.axis = 0.9, cex.lab = 0.9)
    abline(lm(crime_count ~ Individuals_In, data = crime_data), col = "blue")
    legend("topright", legend = paste("r = ", sprintf("%.3f", cor(crime_data$crime_count, crime_data$Individuals_In, use = "complete.obs"))),
           bty = "n", cex = 0.8, text.col = "black")
      
    # Plot 3: Individuals Out vs Crime Count
    plot(crime_data$Individuals_Out, crime_data$crime_count,
         xlab = "Move Out of Zone (thousands)", ylab = paste("# of", crime, "Crimes"),
         pch = 19, xlim = c(0, 0.04), ylim = c(0, 50), cex.axis = 0.9, cex.lab = 0.9)
    abline(lm(crime_count ~ Individuals_Out, data = crime_data), col = "blue")
    legend("topright", legend = paste("r = ", sprintf("%.3f", cor(crime_data$Individuals_Out, crime_data$crime_count, use = "complete.obs"))),
           bty = "n", cex = 0.8, text.col = "black")
  }
}
  
# Additional plot: Population vs. Individuals In
plot(filtered_data$Population, filtered_data$Individuals_In,
     xlab = "Population (thousands)", ylab = "Move Into Zone (thousands)", 
     pch = 19, xlim = c(0, 40), ylim = c(0, max(filtered_data$Individuals_In, na.rm = TRUE)), cex.axis = 0.9, cex.lab = 0.9)
abline(lm(Individuals_In ~ Population, data = filtered_data), col = "blue")
legend("topright", legend = paste("r = ", sprintf("%.3f", cor(filtered_data$Population, filtered_data$Individuals_In, use = "complete.obs"))),
       bty = "n", cex = 0.8, text.col = "black")
  
# Additional Plot: Population vs. Individuals Out
plot(filtered_data$Population, filtered_data$Individuals_Out,
     xlab = "Population (thousands)", ylab = "Move Out of Zone (thousands)", 
     pch = 19, xlim = c(0, 40), ylim = c(0, max(filtered_data$Individuals_Out, na.rm = TRUE)), cex.axis = 0.9, cex.lab = 0.9)
abline(lm(Individuals_Out ~ Population, data = filtered_data), col = "blue")
legend("topright", legend = paste("r = ", sprintf("%.3f", cor(filtered_data$Population, filtered_data$Individuals_Out, use = "complete.obs"))),
       bty = "n", cex = 0.8, text.col = "black")
  
# Title slide with borough and month
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE)
text(5, 7, Borough, cex = 2)
text(5, 3, paste(month_name, "2018"), cex = 2)
  
# Close the PDF device
dev.off()
}



# Example usage with your data - I chose to look at Staten Island here
create_crime_plots(final_merged_data, 1, "Staten Island")
create_crime_plots(final_merged_data, 2, "Staten Island")
create_crime_plots(final_merged_data, 3, "Staten Island")
create_crime_plots(final_merged_data, 4, "Staten Island")
create_crime_plots(final_merged_data, 5, "Staten Island")
create_crime_plots(final_merged_data, 6, "Staten Island")
create_crime_plots(final_merged_data, 7, "Staten Island")
create_crime_plots(final_merged_data, 8, "Staten Island")
create_crime_plots(final_merged_data, 9, "Staten Island")
create_crime_plots(final_merged_data, 10, "Staten Island")
create_crime_plots(final_merged_data, 11, "Staten Island")
create_crime_plots(final_merged_data, 12, "Staten Island")


# Combine PDF Files into one large PDF document
pdf_files <- c("Crime Correlation Plots_1_Staten Island.pdf", "Crime Correlation Plots_2_Staten Island.pdf", "Crime Correlation Plots_3_Staten Island.pdf", "Crime Correlation Plots_4_Staten Island.pdf", 
               "Crime Correlation Plots_5_Staten Island.pdf", "Crime Correlation Plots_6_Staten Island.pdf", "Crime Correlation Plots_7_Staten Island.pdf",
               "Crime Correlation Plots_8_Staten Island.pdf", "Crime Correlation Plots_9_Staten Island.pdf", "Crime Correlation Plots_10_Staten Island.pdf", "Crime Correlation Plots_11_Staten Island.pdf",
               "Crime Correlation Plots_12_Staten Island.pdf")

pdf_combine(input = pdf_files, output = "combined_StatenIsland_monthly_correlation_plots.pdf")



  

