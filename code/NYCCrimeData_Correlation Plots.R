setwd("F:/ade/Desktop/USU Masters Work/Final MS Work/Datasets")

library(readxl)
library(dplyr)
library(ggplot2)
library(qpdf)

#Datasets to read in and merge together
#crime_counts_by_borough_month - provides the number of crimes that occur within each Location ID over the 12 months
#Population_data - provides the population at each LocationID
#MovementData - provides the movement in, out, and net movement for each LocationID for each month


# Read in data and combine individual datasets into one
Crime <- read_excel("G:/ade/Desktop/USU Masters Work/Final MS Work/Datasets/crime_counts_by_borough_month.xlsx") %>%
  select(-geometry) %>%
  rename(Month = Start_Month) %>%
  mutate(Month = as.numeric(Month))
Population <- read_excel("G:/ade/Desktop/USU Masters Work/Final MS Work/Datasets/Population_data.xlsx") %>%
  select(-geometry)
Movement <- read_excel("G:/ade/Desktop/USU Masters Work/Final MS Work/Datasets/MovementData.xlsx") %>%
  mutate(Month = as.numeric(Month))


all_combinations <- expand.grid(LocationID = unique(Crime$LocationID), Month = 1:12)
complete_crime <- all_combinations %>%
  left_join(Crime, by = c("LocationID", "Month"))
merged_data <- complete_crime %>%
  full_join(Movement, by = c("LocationID", "Month"))


final_merged_data <- merged_data %>%
  left_join(Population, by = c("LocationID", "borough")) %>%
  mutate(OFNS_DESC = recode(OFNS_DESC, "ASSAULT 3 & RELATED OFFENSES" = "Assault 3", "CRIMINAL MISCHIEF & RELATED OF" = "Criminal Mischief",
                            "GRAND LARCENY" = "Grand Larceny", "HARRASSMENT 2" = "Harassment", "PETIT LARCENY" = "Petit Larceny")) %>%
  filter(!(LocationID %in% c(132, 138)))
final_merged_data$Population <- final_merged_data$Population / 1000
final_merged_data$Individuals_In <- final_merged_data$Individuals_In / 1000
final_merged_data$Individuals_Out <- final_merged_data$Individuals_Out / 1000

final_merged_data$borough[final_merged_data$borough == "Bronx"] <- "The Bronx"




## Testing to make sure I am getting the same correlation numbers as before!!
#test <- subset(final_merged_data, final_merged_data$Month == 1 & final_merged_data$OFNS_DESC == "Petit Larceny" & final_merged_data$borough == "Queens")

#plot_crime_data <- function(df, month, crime, Borough) {
#  filtered_df <- df %>%
#    filter(Month == month, OFNS_DESC == crime, borough == Borough)
  
#plot(filtered_df$Individuals_In, filtered_df$crime_count,
#     xlab = "Population", ylab = "Crime",
#     pch = 19, xlim = c(0, 80), cex.axis = 1, cex.lab = 1)
#abline(lm(crime_count ~ Individuals_In, data = filtered_df), col = "blue")
#legend("topright", legend = paste("r = ", round(cor(filtered_df$crime_count, filtered_df$Individuals_In, use = "complete.obs"), 3)),
#       bty = "n", cex = 0.8, text.col = "black")
#}

#plot_crime_data(final_merged_data, 1, "Petit Larceny", "Queens")


#write_xlsx(test, "D:/ade/Desktop/USU Masters Work/Final MS Work/Datasets/test_function_data.xlsx")







setwd("G:/ade/Desktop/USU Masters Work/Final MS Work/Graphics/Final Crime Visuals/Correlation Scatterplots/Crime Correlation Plots/StatenIsland2")

# Define the function to create scatter plots for different crimes
create_crime_plots <- function(data, month, Borough) {
  month_names <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")
  month_name <- month_names[month]
  filtered_data <- data %>%
    filter(Month == month, borough == Borough)
  
print(filtered_data) 

# List of crimes to plot
#crimes <- unique(filtered_data$OFNS_DESC)
crime_order <- c("Petit Larceny", "Harassment", "Assault 3", "Criminal Mischief", "Grand Larceny")

# Create a PDF file to save the plots
pdf(paste0("Crime Correlation Plots_", month, "_", Borough, ".pdf"), width = 8, height = 11)

# Set up the plot layout
par(mfrow = c(6, 3), mar = c(3.5, 4, 1, 0.5), mgp = c(2, 1, 0))

for (crime in crime_order) {
  crime_data <- filtered_data %>% filter(OFNS_DESC == crime)

if (nrow(crime_data) > 0) {
# Plot Population vs Crime Count
  plot(crime_data$Population, crime_data$crime_count,
       xlab = "Population Count (thousands)", ylab = paste("# of", crime, "Crimes"),
       pch = 19, xlim = c(0, 40), ylim = c(0, 50), cex.axis = 0.9, cex.lab = 0.9)
  abline(lm(crime_count ~ Population, data = crime_data), col = "blue")
  legend("topright", legend = paste("r = ", sprintf("%.3f", cor(crime_data$Population, crime_data$crime_count, use = "complete.obs"))),
         bty = "n", cex = 0.8, text.col = "black")
  
# Plot Individuals In vs Crime Count
  plot(crime_data$Individuals_In, crime_data$crime_count,
       xlab = "Move Into Zone (thousands)", ylab = paste("# of", crime, "Crimes"),
       pch = 19, xlim = c(0, 0.4), ylim = c(0, 50), cex.axis = 0.9, cex.lab = 0.9)
  abline(lm(crime_count ~ Individuals_In, data = crime_data), col = "blue")
  legend("topright", legend = paste("r = ", sprintf("%.3f", cor(crime_data$crime_count, crime_data$Individuals_In, use = "complete.obs"))),
         bty = "n", cex = 0.8, text.col = "black")
  
# Plot Individuals Out vs Crime Count
  plot(crime_data$Individuals_Out, crime_data$crime_count,
       xlab = "Move Out of Zone (thousands)", ylab = paste("# of", crime, "Crimes"),
       pch = 19, xlim = c(0, 0.04), ylim = c(0, 50), cex.axis = 0.9, cex.lab = 0.9)
  abline(lm(crime_count ~ Individuals_Out, data = crime_data), col = "blue")
  legend("topright", legend = paste("r = ", sprintf("%.3f", cor(crime_data$Individuals_Out, crime_data$crime_count, use = "complete.obs"))),
         bty = "n", cex = 0.8, text.col = "black")
}
}

#, xlim = c(0, 50)

# Additional plots
# Plot Population vs Movement In
plot(filtered_data$Population, filtered_data$Individuals_In,
     xlab = "Population (thousands)", ylab = "Move Into Zone (thousands)", 
     pch = 19, xlim = c(0, 40), ylim = c(0, max(filtered_data$Individuals_In, na.rm = TRUE)), cex.axis = 0.9, cex.lab = 0.9)
abline(lm(Individuals_In ~ Population, data = filtered_data), col = "blue")
legend("topright", legend = paste("r = ", sprintf("%.3f", cor(filtered_data$Population, filtered_data$Individuals_In, use = "complete.obs"))),
       bty = "n", cex = 0.8, text.col = "black")

# Plot Population vs Movement Out
plot(filtered_data$Population, filtered_data$Individuals_Out,
     xlab = "Population (thousands)", ylab = "Move Out of Zone (thousands)", 
     pch = 19, xlim = c(0, 40), ylim = c(0, max(filtered_data$Individuals_Out, na.rm = TRUE)), cex.axis = 0.9, cex.lab = 0.9)
abline(lm(Individuals_Out ~ Population, data = filtered_data), col = "blue")
legend("topright", legend = paste("r = ", sprintf("%.3f", cor(filtered_data$Population, filtered_data$Individuals_Out, use = "complete.obs"))),
       bty = "n", cex = 0.8, text.col = "black")

# Provide an overview of the Borough and Month the plots are displaying
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes = FALSE)
text(5, 7, Borough, cex = 2)
text(5, 3, paste(month_name, "2018"), cex = 2)



# Close the PDF device
dev.off()
}

# Example usage with your data
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
