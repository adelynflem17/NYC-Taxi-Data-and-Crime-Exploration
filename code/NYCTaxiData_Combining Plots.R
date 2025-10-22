setwd("F:/ade/Desktop/USU Masters Work/Final MS Work/Datasets/Monthly Taxi Data/Cleaned Data 2")

library(dplyr)
library(sf)
library(readxl)
library(ggplot2)
library(purrr)
library(tidyr)
library(writexl)
library(tidyverse)
library(patchwork)

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


setwd("F:/ade/Desktop/USU Masters Work/Final MS Work")
fname <- unzip(zipfile = "taxi_zones.zip")
zones <- sf::st_read("./taxi_zones.shp")
zones <- sf::st_transform(zones, crs = 4326)
nyc_boroughs <- st_read("F:/ade/Desktop/USU Masters Work/Final MS Work/R Code/nybb_24d/nybb.shp")

# Obtaining Spatial Outline for each individual Borough
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
MoveOut_January <- subset(January_TaxiCab, January_TaxiCab$PULocationID != January_TaxiCab$DOLocationID) %>%
  group_by(PULocationID) %>%
  summarise(Individuals_Out_Jan = sum(passenger_count)) %>%
  rename(., LocationID = PULocationID) %>%
  as.data.frame()
MoveIn_January <- subset(January_TaxiCab, January_TaxiCab$PULocationID != January_TaxiCab$DOLocationID) %>%
  group_by(DOLocationID) %>%
  summarise(Individuals_In_Jan = sum(passenger_count)) %>%
  rename(., LocationID = DOLocationID) %>%
  as.data.frame()
January_Month_Merged_Data <- merge(MoveIn_January, MoveOut_January, by = "LocationID")
January_Month_Merged_Data$Net_Movement <- January_Month_Merged_Data$Individuals_In_Jan - January_Month_Merged_Data$Individuals_Out_Jan


# February
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


# March
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


# April
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


# May
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


# June
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


# July
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


# August
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


# September
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


# October
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


# November
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


# December
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




# Change line 233 to whichever Borough zone we want to look at to create the heat maps and line 234 to whichever folder you want to save the visuals to
Borough_Zone <- Brooklyn_zones
setwd("F:/ade/Desktop/USU Masters Work/Final MS Work/Graphics/Final Crime Visuals/Movement by Zone/Net Movement 2")


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



# Function to generate a plot for a given month
#create_net_movement_plot <- function(data, borough, month_name, borough_name = "Queens", year = 2018) {
#  Monthly_data_net <- data %>%
#    mutate(rank = dense_rank(desc(Net_Movement))) %>%
#    mutate(Net_Movement_Category = case_when(
#      rank <= 5 ~ "red",
#      rank <= 10 ~ "orange",
#      Net_Movement > 0 & rank > 10 ~ "yellow",
#      Net_Movement < 0 ~ "gray",
#      TRUE ~ "gray"
#    )) %>%
#    mutate(Net_Movement_Category = factor(Net_Movement_Category, 
#                                          levels = c("red", "orange", "yellow", "gray")))

#  label_data <- Monthly_data_net %>%
#    filter(Net_Movement_Category %in% c("red", "orange"))

#  ggplot() +
#    geom_sf(data = borough, fill = "white", show.legend = FALSE) + 
#    geom_sf(data = Monthly_data_net, aes(geometry = geometry, fill = Net_Movement_Category),
#            color = "black", size = 0.5) +
#    geom_sf_label(data = label_data, aes(geometry = geometry, label = Net_Movement), 
#                 fill = "white", color = "black", size = 1.0, label.size = 0.2,
#                 show.legend = FALSE) +
#    scale_fill_manual(name = "Net Movement Scale", 
#                     values = c("red" = "red", "orange" = "orange", 
#                                "yellow" = "yellow", "gray" = "gray80"),
#                      labels = c("red" = "Top 5", "orange" = "Top 6-10", 
#                                 "yellow" = "Positive Movement", "gray" = "Negative Movement"), drop = FALSE) +
#    ggtitle(month_name) +
#   theme_void() 
#    theme(
#      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
#      legend.position = "none")
#}

#month_list <- list(January = January_merged_data, February = February_merged_data, March = March_merged_data, 
#                   April = April_merged_data, May = May_merged_data, June = June_merged_data, July = July_merged_data,
#                  August = August_merged_data, September = September_merged_data, October = October_merged_data,
#                   November = November_merged_data, December = December_merged_data)

#plots <- map2(month_list, names(month_list), ~ create_net_movement_plot(.x, Borough_Zone, .y, borough_name = "Staten Island", year = 2018))

#combined_plot <- wrap_plots(plots, ncol = 3, guides = "collect") &
#  theme(legend.position = "bottom")

#final_plot <- combined_plot +
#  plot_annotation(
#    title = "Queens 2018: Monthly Net Movement by Zone",
#    theme = theme(
#      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
#    )
#  )


#ggsave("NetMovement_Combined_Queens.pdf", final_plot, width = 11, height = 8.5, units = "in", dpi = 300)








create_net_movement_plot <- function(data, borough, month_name, borough_name = "Brooklyn", year = 2018) {
  # Return empty plot if data is missing or empty
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + theme_void() + ggtitle(month_name))
  }
  
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
  
  label_data <- Monthly_data_net %>%
    filter(Net_Movement_Category %in% c("red", "orange"))
  
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

# Create list of monthly data
month_list <- list(
  January = January_merged_data, February = February_merged_data, March = March_merged_data, 
  April = April_merged_data, May = May_merged_data, June = June_merged_data, July = July_merged_data,
  August = August_merged_data, September = September_merged_data, October = October_merged_data,
  November = November_merged_data, December = December_merged_data
)

# Generate plots
plots <- map2(month_list, names(month_list), ~ create_net_movement_plot(.x, Borough_Zone, .y, borough_name = "Brooklyn", year = 2018))

# Combine plots
combined_plot <- wrap_plots(plots, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

# Add annotation
final_plot <- combined_plot +
  plot_annotation(
    title = "Brooklyn 2018: Monthly Net Movement by Zone",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )

# Save output
ggsave("NetMovement_Combined_Brooklyn.pdf", final_plot, width = 11, height = 8.5, units = "in", dpi = 300)





















