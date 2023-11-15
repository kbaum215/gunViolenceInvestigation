# Load required library
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Load the dataset "raw_sales"
data_sales <- read_csv("C:\\Users\\keevi\\OneDrive\\Documents\\SDU\\ADS-506\\Week 1\\Assignment 1.2\\raw_sales.csv")

# Transform the "datesold" column to ensure it is in date format
data_sales$datesold <- as.Date(data_sales$datesold, format = "%m/%d/%Y %H:%M")


# Create a new column "YearMonth" with the first day of each month
data_sales <- data_sales %>%
  mutate(YearMonth = floor_date(datesold, unit = "month"))

# Aggregate data into monthly time index
monthly_data <- data_sales %>%
  group_by(YearMonth) %>%
  summarise(
    TotalPrice = sum(price),
    AvgBedrooms = mean(bedrooms)
  )

# Create a new column "Quarter" to represent the quarter
data_sales <- data_sales %>%
  mutate(Quarter = quarter(datesold, with_year = TRUE))

# Aggregate data into quarterly time index
quarterly_data <- data_sales %>%
  group_by(Quarter) %>%
  summarise(
    TotalPrice = sum(price),
    AvgBedrooms = mean(bedrooms)
  )

# View the first few rows of the aggregated data
head(monthly_data)
head(quarterly_data)

# Plot Monthly Total Price Trend
ggplot(monthly_data, aes(x = YearMonth, y = TotalPrice)) +
  geom_line() +
  labs(
    title = "Monthly Total Price Trend",
    x = "Year-Month",
    y = "Total Price"
  ) +
  theme_minimal()

# Check for missing values in monthly_data
sum(is.na(monthly_data$TotalPrice))
sum(is.na(monthly_data$YearMonth))

library(zoo)  # For the na.locf function

monthly_data$YearMonth <- zoo::na.locf(monthly_data$YearMonth)

# Plot Quarterly Total Price Trend
ggplot(quarterly_data, aes(x = Quarter, y = TotalPrice)) +
  geom_line() +
  labs(
    title = "Quarterly Total Price Trend",
    x = "Quarter",
    y = "Total Price"
  ) +
  theme_minimal()

# Additional Analysis and Visualization

# Example: Histogram of property types
ggplot(data_sales, aes(x = propertyType)) +
  geom_bar() +
  labs(
    title = "Distribution of Property Types",
    x = "Property Type",
    y = "Count"
  ) +
  theme_minimal()

# Create a box-and-whisker plot for the "price" column
ggplot(data_sales, aes(y = price)) +
  geom_boxplot() +
  labs(
    title = "Box-and-Whisker Plot of Price",
    y = "Price"
  ) +
  theme_minimal()

# EDA

# Highlight potential outliers
# Calculate the lower and upper bounds for potential outliers
q1 <- quantile(data_sales$price, 0.25)
q3 <- quantile(data_sales$price, 0.75)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Identify potential outliers
outliers <- data_sales[data_sales$price < lower_bound | data_sales$price > upper_bound,]

# Print the identified outliers
cat("Identified Outliers:\n")
print(outliers)

# Load required libraries
library(mice)

# Create a missing data pattern plot
md.pattern(data_sales)

# Summary statistics for "price"
summary_price <- summary(data_sales$price)
mean_price <- mean(data_sales$price)
median_price <- median(data_sales$price)
min_price <- min(data_sales$price)
max_price <- max(data_sales$price)
sd_price <- sd(data_sales$price)

# Summary statistics for "bedrooms"
summary_bedrooms <- summary(data_sales$bedrooms)
mean_bedrooms <- mean(data_sales$bedrooms)
median_bedrooms <- median(data_sales$bedrooms)
min_bedrooms <- min(data_sales$bedrooms)
max_bedrooms <- max(data_sales$bedrooms)
sd_bedrooms <- sd(data_sales$bedrooms)

# Create a bar plot to show the distribution of sales by "postcode"
postcode_counts <- table(data_sales$postcode)

ggplot(data = as.data.frame(postcode_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Sales by Postcode",
    x = "Postcode",
    y = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create scatterplots to explore location-specific trends
ggplot(data_sales, aes(x = postcode, y = price)) +
  geom_point() +
  labs(
    title = "Price vs. Postcode",
    x = "Postcode",
    y = "Price"
  ) +
  theme_minimal()

ggplot(data_sales, aes(x = postcode, y = bedrooms)) +
  geom_point() +
  labs(
    title = "Bedrooms vs. Postcode",
    x = "Postcode",
    y = "Bedrooms"
  ) +
  theme_minimal()

# Calculate the Pearson correlation between "price" and "bedrooms"
correlation_price_bedrooms <- cor(data_sales$price, data_sales$bedrooms)

# Print the correlation coefficient
cat("Pearson's Correlation between Price and Bedrooms: ", correlation_price_bedrooms, "\n")

# Install required library
library(corrplot)

# Select the variables for correlation plot
selected_vars <- c("price", "bedrooms")

# Calculate the correlation matrix for these variables
correlation_matrix <- cor(data_sales[, selected_vars])

# create correlation plot
corrplot(correlation_matrix, method = "color", tl.cex = 0.8, tl.col = "black")

# Decomposing a time series into its components, to include random, seasonal, trend and observed, can provide insights into the underlying patterns. Here we use a monthly frequency.
# Create a time series object
ts_data <- ts(data_sales$price, frequency = 12)  # Set frequency to 12 for monthly data

# Decompose the time series
decomposed_ts <- decompose(ts_data)

# Plot the decomposed components
plot(decomposed_ts)
