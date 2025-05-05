# -------------------------------
#  1. Library Installation and Import
# -------------------------------

# Install necessary libraries 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

# Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# -------------------------------
#  2. Read and Explore the Dataset
# -------------------------------

# Read the CSV file
data <- read.csv("C:/Users/Lenovo/Downloads/r-project/mobile_sales_data.csv", stringsAsFactors = TRUE)

# Preview the data
head(data)
str(data)
summary(data)
colnames(data)

# -------------------------------
#  3. Select Relevant Columns
# -------------------------------

# Select only the relevant columns for analysis
selected_data <- data %>%
  select(Brand, Product, Region, Price, Quantity.Sold, RAM, ROM, SSD)
head(selected_data)

# -------------------------------
#  4. Convert RAM, ROM and SSD to Numeric
# -------------------------------

# Clean RAM and ROM by removing "GB"
# Convert SSD values: "2TB" → 2048, "512GB" → 512
selected_data <- selected_data %>%
  mutate(
    RAM = as.numeric(gsub("GB", "", RAM)),
    ROM = as.numeric(gsub("GB", "", ROM)),
    SSD = case_when(
      grepl("TB", SSD) ~ as.numeric(gsub("TB", "", SSD)) * 1024,
      grepl("GB", SSD) ~ as.numeric(gsub("GB", "", SSD)),
      TRUE ~ NA_real_
    )
  )

# -------------------------------
#  5. Remove Missing Values
# -------------------------------

# Drop rows with NA values
selected_data <- na.omit(selected_data)

# -------------------------------
#  6. ANOVA Tests (no normalization)
# -------------------------------

# Test 1: Is price significantly different across brands?
anova_price_brand <- aov(Price ~ Brand, data = selected_data)
summary(anova_price_brand)

# Test 2: Is SSD capacity different across regions?
anova_ssd_region <- aov(SSD ~ Region, data = selected_data)
summary(anova_ssd_region)

# Test 3: Is quantity sold different across regions?
anova_qty_region <- aov(Quantity.Sold ~ Region, data = selected_data)
summary(anova_qty_region)

# -------------------------------
#  7. Visualizations (raw values)
# -------------------------------

# Boxplot: Price by Brand
ggplot(selected_data, aes(x = Brand, y = Price)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Price Distribution by Brand",
       x = "Brand",
       y = "Price") +
  theme_minimal()

# Histogram: Quantity Sold
ggplot(selected_data, aes(x = Quantity.Sold)) +
  geom_histogram(binwidth = 2000, fill = "orange", color = "black") +
  labs(title = "Distribution of Quantity Sold",
       x = "Quantity Sold",
       y = "Frequency") +
  theme_minimal()

# Scatter Plot: RAM vs Price (more meaningful than SSD)
ggplot(selected_data, aes(x = RAM, y = Price)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "RAM vs Price",
       x = "RAM (GB)",
       y = "Price") +
  theme_minimal()