# Load necessary libraries
library(tidyverse)

setwd("../HW3/")
#load data
data <- read.table("raw_data/PlateletHW.tsv", header = TRUE, sep = "\t")

#summary statistics for ADP
summary(data$ADP)

# Boxplot to visualize outliers in ADP
ggplot(data, aes(x = "", y = ADP)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 5) +
  labs(title = "ADP-Induced Platelet Aggregation Levels", y = "ADP Level") +
  theme_minimal()

# Identify outliers using IQR
Q1 <- quantile(data$ADP, 0.25)
Q3 <- quantile(data$ADP, 0.75)
IQR <- Q3 - Q1
lower_outlier <- Q1 - 1.5 * IQR
upper_outlier <- Q3 + 1.5 * IQR


outliers <- data %>%
  filter(ADP < lower_outlier | ADP > upper_outlier | ADP < 0)

cat("Lower Outlier Bound:", lower_outlier, "\n")
cat("Upper Outlier Bound:", upper_outlier, "\n")

# Clean the data by removing outliers and any values below zero
cleaned_data <- data %>%
  filter(ADP >= 0 & ADP >= lower_outlier & ADP <= upper_outlier) %>%
  drop_na()

# Summary of cleaned data
summary(cleaned_data$ADP)

# Save the cleaned data
write.table(cleaned_data, "clean_data/PlateletHW_cleaned.tsv", sep = "\t", row.names = FALSE)
