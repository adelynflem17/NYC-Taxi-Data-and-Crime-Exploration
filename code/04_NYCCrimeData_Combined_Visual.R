# Load required libraries
library(data.table)
library(tidyverse)
library(hms)
library(classInt)
library(RColorBrewer)
library(cowplot)
library(glue)

# Read in NYPD crime data
Raw_CrimeData <- fread("NYPD_Complaint_Data_Historic.csv", fill = TRUE)

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

# Generate and save a heatmap for a particular crime
overall_plot <- create_overall_crime_heatmap(CrimeData, "GRAND LARCENY")

ggsave("Overall_Heatmap_GrandLarceny.pdf",
       plot = overall_plot,
       width = 7,
       height = 9.5,
       units = "in",
       dpi = 300)




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
ggsave("Combined_Heatmaps_Borough_Harassment.pdf",
       plot = grid,
       width = 7,
       height = 9.5,
       units = "in",
       dpi = 300)



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
ggsave("Harassment_Heatmaps_Manual.pdf", plot = p, width = 7, height = 9.5, units = "in", dpi = 300)









