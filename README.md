# R-CLIMATE-CHANGE-ANALYSIS

# Load the dataset

data <- read.csv("C:\\Users\\SYAM SELVAKUMAR\\Documents\\python projects\\GlobalLandTemperaturesByCity.csv")


install.packages("ggplot2")


#DATA EXPLORATION

# Explore the structure of the dataset
str(data)

# Check summary statistics
summary(data)

# Check for missing values
sum(is.na(data))


# Check the first few rows of the dataset
head(data)

# Visualize the distribution of AverageTemperature
library(ggplot2)
ggplot(data, aes(x = AverageTemperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Temperature",
       x = "Average Temperature", y = "Frequency")

# Convert the date column to Date type
data$dt <- as.Date(data$dt)

# Plot average temperature over time
ggplot(data, aes(x = dt, y = AverageTemperature)) +
  geom_line() +
  labs(title = "Average Temperature Over Time",
       x = "Date", y = "Average Temperature")

# Plot average temperature on a map
install.packages("ggmap")


# Explore correlations between variables
correlation_matrix <- cor(data[, c("AverageTemperature", "Latitude", "Longitude")])
print(correlation_matrix)

# Calculate average temperature for each country
library(dplyr)
avg_temp_by_country <- data %>%
  group_by(Country) %>%
  summarize(Avg_Temperature = mean(AverageTemperature, na.rm = TRUE))

# Plot average temperature by country
ggplot(avg_temp_by_country, aes(x = Country, y = Avg_Temperature, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Temperature by Country",
       x = "Country", y = "Average Temperature")

# Perform seasonal decomposition for a specific city
library(forecast)
city_data <- filter(data, City == "Your_City_Name")
ts_data <- ts(city_data$AverageTemperature, start = c(1743, 1), frequency = 12)
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

# Plot a time series of average temperature for a specific city
library(ggplot2)
ggplot(filter(data, City == "Your_City_Name"), aes(x = dt, y = AverageTemperature)) +
  geom_line() +
  labs(title = "Average Temperature Over Time in Your_City_Name",
       x = "Date", y = "Average Temperature")

# Perform clustering analysis to identify groups of cities with similar temperature patterns
# Example: K-means clustering
# Prepare data
features <- data[, c("AverageTemperature", "Latitude", "Longitude")]
# Scale the features
scaled_features <- scale(features)
# Perform K-means clustering
set.seed(123)  # for reproducibility
k <- 3  # number of clusters
kmeans_result <- kmeans(scaled_features, centers = k)
# Add cluster labels to the original data
data$Cluster <- as.factor(kmeans_result$cluster)
# Visualize clusters
ggplot(data, aes(x = Longitude, y = Latitude, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering of Cities Based on Temperature Patterns",
       x = "Longitude", y = "Latitude")

# Fit a linear regression model to predict average temperature
lm_model <- lm(AverageTemperature ~ Latitude + Longitude, data = data)
summary(lm_model)

# Summarize key findings and provide insights and recommendations based on the analysis
cat("In conclusion, the analysis provides valuable insights into the spatial and temporal patterns of average temperatures. Further research could focus on exploring additional factors affecting temperature variations and developing predictive models to forecast future temperatures.")


