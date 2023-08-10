#TR_stats.R
 
library(readr)
library(ggplot2)
library(tidyr)
library(irr)
library(psych)
library(rsconnect)
library(dplyr)
library(ICC)

# set the directory path
file_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Graduate/NCL Lab/T_RT/T_RT_Stats/data.txt"

TRdata <- read.table(file_path, sep = "\t", header = TRUE)
 
# Get the current column names
col_names <- colnames(TRdata)
# Shorten the column names
new_col_names <- gsub("^X", "", col_names)  # Remove "X" at the beginning
new_col_names <- gsub("(\\.[0-9]{2})[0-9]*", "\\1", new_col_names)  # Shorten the numeric values
# Update the column names in TRdata
colnames(TRdata) <- new_col_names
 
#remove the lowest frequency bin, 0.4:
TRdata <- TRdata[, -5]
 
#find the ZIP average in a new column mean ZIP
TRdata$meanZIP <- rowMeans(TRdata[, -(1:4)], na.rm = TRUE)
 
## Convert the date column to Date type
TRdata$date <- as.Date(TRdata$date, format = "%d-%b-%Y")
## Change column name from "freq" to "laterality" and "trial" to "condition"
colnames(TRdata)[colnames(TRdata) == "freq"] <- "laterality"
colnames(TRdata)[colnames(TRdata) == "trial"] <- "condition"
 
# Sort the data by subject and date
TRdata <- TRdata %>%
  arrange(subject, date)
 
# Create the VisitNum column
TRdata <- TRdata %>%
  group_by(subject) %>%
  mutate(VisitNum = match(date, unique(date)))
 
# # Create a new dataframe with rows for TR1290466
# TR1290466 <- TRdata[TRdata$subject == "TR1290466", ]
# # Subset the dataframe for subject TR1290466
# subject_data <- TRdata[TRdata$subject == "TR1290466", ]
# # Convert date to a Date object
# subject_data$date <- as.Date(subject_data$date, format = "%d-%b-%Y")
# # Reshape data from wide to long format
# subject_data_long <- gather(subject_data, key = "x_value", value = "zIP_height", matches("^\\d+\\.\\d+"))
#
# # Create a color palette for freq values
# color_palette <- c("Left" = "red", "Right" = "blue", "Both" = "green")
# # Plotting
# ggplot(subject_data_long, aes(x = x_value, y = zIP_height, color = factor(freq))) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~ trial) +
#   scale_color_manual(values = color_palette) +
#   labs(title = "Plots for TR1290466",
#        x = "X Column Names",
#        y = "zIP Height")
 
 
 
# ICC 3,1
# Subset the dataframe based on the specified conditions
subset_data <- subset(TRdata, condition == 1 & laterality == "Both" & VisitNum %in% c(1, 2))
min_subset_data <- subset(subset_data, select = c(subject, condition, meanZIP, laterality, VisitNum))
min_subset_data <- subset(min_subset_data, subject != "TR1300466") #incomplete dataset for TR1300466, need to look into this
# Create a matrix with the 'meanZIP' values for VisitNum 1 and 2
icc_matrix <- matrix(c(min_subset_data$meanZIP[min_subset_data$VisitNum == 1], min_subset_data$meanZIP[min_subset_data$VisitNum == 2]), ncol = 2)
 
# Calculate the ICC(3,1)
icc_result <- ICC(icc_matrix)$results
 
# Print the ICC result
print(icc_result)
 
###loop through all conditions
condition_values <- c(1, 3, 5, 7, 9, 11)
 
for (condition_value in condition_values) {
  # Subset the dataframe based on the specified conditions
  subset_data <- subset(TRdata, condition == condition_value & laterality == "Both" & VisitNum %in% c(1, 2))
  min_subset_data <- subset(subset_data, select = c(subject, condition, meanZIP, VisitNum))
  min_subset_data <- subset(min_subset_data, subject != "TR1300466")
 
  # Create a matrix with the 'meanZIP' values for VisitNum 1 and 2
  icc_matrix <- matrix(c(min_subset_data$meanZIP[min_subset_data$VisitNum == 1], min_subset_data$meanZIP[min_subset_data$VisitNum == 2]), ncol = 2)
 
  # Calculate the ICC(3,1)
  icc_result <- ICC(icc_matrix)$results
 
  # Print the ICC result
  cat("Condition #:", trial_value, "\n")
  print(icc_result)
  cat("\n")
}