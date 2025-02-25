# ------------------------------------------------------------------------------
# Activity: Explore descriptive statistics in R
# ------------------------------------------------------------------------------

# Introduction:
# Data professionals use descriptive statistics to summarize and understand data.
# In this lab, you are part of an analytics team at the U.S. EPA.
# You will analyze air quality data with respect to carbon monoxide levels.
# The dataset "c4_epa_air_quality.csv" contains data from over 200 monitoring sites,
# including information such as state, county, city, and local site names, as well as AQI values.
# The goal is to compute descriptive statistics and share insights with stakeholders.

# ------------------------------------------------------------------------------
# Step 1: Imports and Data Loading
# ------------------------------------------------------------------------------

# Import relevant libraries. In R, we can use base functions for most tasks,
# but we'll load the "dplyr" package for data manipulation (if not installed, run install.packages("dplyr"))

library(dplyr)


# Load the dataset.
# Note: If needed, adjust the file path to where "c4_epa_air_quality.csv" is located.

epa_data <- read.csv("/Users/viviansabga/Desktop/Desktop - Vivian’s MacBook Pro - 1/NORTHEASTERN/Econ/c4_epa_air_quality.csv", stringsAsFactors = FALSE)
# If you want to set the first column as row names (similar to index_col=0 in pandas), you can use:
# epa_data <- read.csv("c4_epa_air_quality.csv", row.names = 1, stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Step 2: Data Exploration
# ------------------------------------------------------------------------------

# 2a. Display the first 10 rows of the data.
head(epa_data, 10)

# -------------------------------------------------------------------------------
# Questions:
# 1. What does the "aqi" column represent?
#    ANSWER: 
#       The "aqi" column represents the Air Quality Index, which is a numerical scale used to gauge 
#        how polluted the air is at a particular location.
#
# 2. In what units are the aqi values expressed?
#    ANSWER:
#       AQI values are unitless numbers that correspond to levels of air pollution; for carbon monoxide, 
#        an AQI of 100 corresponds to about 9 parts per million.
# -------------------------------------------------------------------------------

# 2b. Get a table of descriptive statistics for the numeric columns.
# In R, we use the summary() function to see key statistics.
summary(epa_data)

# -------------------------------------------------------------------------------
# Questions:
# 3. Based on the descriptive statistics table, what do you notice about the count for the "aqi" column?
#    ANSWER:
#       The count for "aqi" is 260, which matches the total number of rows in the dataset. This confirms that there are no missing values. 
#       (e.g., Check if there are any missing values and note the number of non-NA observations.)
#
# 4. What do you notice about the 25th percentile for the "aqi" column?
#    ANSWER:
#       This value gives insight into the lower quartile of air quality measurements. The 25th percentile (Q1) for the "aqi" column is 2.0, 
#       meaning that 25% of the AQI values are at or below 2
#
# 5. What do you notice about the 75th percentile for the "aqi" column?
#    ANSWER:
#       This value indicates the upper quartile and helps to understand where most values lie. The 75th percentile (Q3) for the "aqi" column is 9.0, 
#       meaning that 75% of the AQI values are at or below 9.
# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 3: Statistical Tests and Further Exploration
# ------------------------------------------------------------------------------

# 3a. Get descriptive statistics about the states in the data.
# Assuming there is a column for state names (e.g., "state" or "State"). 
# If your column is named differently (like "STATENAME"), adjust accordingly.
# First, convert the state column to a factor.
epa_data$state <- as.factor(epa_data$state_name)  # Change "state" to the appropriate column name if needed.
summary(epa_data$state_name)

# -------------------------------------------------------------------------------
# Question:
# 6. What do you notice while reviewing the descriptive statistics for the state data?
#    ANSWER:
#       The dataset contains 52 unique states (which likely includes Washington, D.C.).California has the highest
#       number of air quality measurements, appearing 66 times. Arizona: 14 observations, Ohio: 12 observations,
#       Florida: 12 observations, Texas: 10 observations.
# -------------------------------------------------------------------------------

# 3b. Alternatively, calculate individual statistics for the "aqi" column using base R functions.
# Calculate the mean, median, min, max, and standard deviation.

# Count (number of non-missing values)
aqi_count <- sum(!is.na(epa_data$aqi))

# Mean
aqi_mean <- mean(epa_data$aqi, na.rm = TRUE)

# Median
aqi_median <- median(epa_data$aqi, na.rm = TRUE)

# Minimum value
aqi_min <- min(epa_data$aqi, na.rm = TRUE)

# Maximum value
aqi_max <- max(epa_data$aqi, na.rm = TRUE)

# Standard Deviation (Note: R's sd() uses sample standard deviation, equivalent to ddof = 1)
aqi_sd <- sd(epa_data$aqi, na.rm = TRUE)

# Calculate the range (max - min)
aqi_range <- aqi_max - aqi_min

# Print out the computed statistics.
cat("Descriptive Statistics for 'aqi':\n")
cat("Count:", aqi_count, "\n")
cat("Mean:", aqi_mean, "\n")
cat("Median:", aqi_median, "\n")
cat("Minimum:", aqi_min, "\n")
cat("Maximum:", aqi_max, "\n")
cat("Range:", aqi_range, "\n")
cat("Standard Deviation:", aqi_sd, "\n")

# -------------------------------------------------------------------------------
# Questions:
# 7. What do you notice about the mean value of the "aqi" column?
#    ANSWER:
#       The mean represents the average air quality index across all sites, giving an overall idea 
#        of air pollution levels. The mean (average) AQI value is 6.76. Since AQI values can range from 0 to 500+,
#         an average of 6.76 suggests that air quality is generally very good across the dataset.
#
# 8. What do you notice about the median value of the "aqi" column?
#    ANSWER:
#       The median gives a robust measure of central tendency that isn’t as affected by outliers.The median (5.0) is lower than the mean (6.76).
#       This suggests that the distribution of AQI values is right-skewed, meaning there are some higher AQI values (outliers) pulling the mean upward.
#
# 9. What do you notice about the minimum value of the "aqi" column?
#    ANSWER:
#       The minimum value shows the best air quality observed in the data. The minimum value for the aqi column is 0.
#       This means that the smallest aqi value in the data is 0 parts per million.
#
# 10. What do you notice about the maximum value of the "aqi" column?
#     ANSWER:
#       The maximum value shows the worst air quality observed in the data. The maximum AQI value is 50,
#       which represents the worst air quality observed in the dataset. This suggests that even the worst recorded air quality in this dataset 
#       falls within the "Good" (0–50) or "Moderate" (51–100) range, based on standard air quality classifications.
#
# 11. What do you notice about the standard deviation of the "aqi" column?
#     ANSWER:
#       A larger standard deviation indicates that the aqi values are more spread out, suggesting higher variability.
#       The standard deviation of the AQI column is 7.06, which measures how much the AQI values deviate from the mean.
#       Since the mean AQI is 6.76, a standard deviation of 7.06 means that many AQI values deviate significantly from the average.
# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 4: Results and Evaluation
# ------------------------------------------------------------------------------

# Reflect on the analysis by answering these questions:
#
# 12. What are some key takeaways you learned during this lab?
#     ANSWER:
#       Descriptive statistics provide a quick summary of the distribution of data. The central 
#        tendency measures (mean and median) and measures of spread (range and standard deviation) are essential 
#        for understanding overall trends and variability in the air quality data. The mean (6.76) gives the average AQI,
#       while the median (5.0) shows the midpoint of the data. The difference between mean and median suggests a right-skewed
#       distribution (some higher values pulling the mean up). The standard deviation (7.06) shows moderate variability in AQI values.
#
# 13. How would you present your findings from this lab to others?
#    ANSWER:
#       Most AQI values fall within a specific range, with a certain percentage 
#        of sites showing satisfactory air quality. Any sites with extreme values and explain 
#        their implications for public health.) 
# The findings from this lab indicate that most AQI values fall within a relatively low range, with the majority of locations experiencing good air quality.
# The median AQI value of 5.0 suggests that at least half of the observations have minimal air pollution, and the maximum recorded AQI of 50 still falls within
# the "Good" air quality category. This indicates that, overall, the dataset represents areas with satisfactory air conditions. However, the standard deviation of
# 7.06 suggests some variability, meaning that while most locations have clean air, certain sites exhibit slightly higher pollution levels.
# State-level trends reveal that California has the highest number of observations (66), which may influence the overall results.
#
# 14. What summary would you provide to stakeholders (considering that "AQI values at or below 100 are generally
#     thought of as satisfactory" and an AQI of 100 for carbon monoxide corresponds to about 9 parts per million)?
#     ANSWER:
# I would summarize that while the average AQI across monitoring sites is 6.76, which is well below the threshold of 100 and generally considered satisfactory,
# it is important to examine site-specific trends. The majority of sites have AQI values that indicate good air quality, with the median AQI at 5.0 and the 75th percentile at 9.0,
# suggesting that most locations in the dataset experience minimal pollution levels. Additionally, the maximum recorded AQI is 50, meaning that no sites in this dataset exceed the
# AQI threshold of 100, which is where air quality concerns typically arise.
#
# References:
# - Air Quality Index - A Guide to Air Quality and Your Health.
#   https://www.airnow.gov/sites/default/files/2018-04/aqi_brochure_02_14_0.pdf (2014, February)
#
# - NumPy Std Documentation (for context on standard deviation differences):
#   https://numpy.org/doc/stable/reference/generated/numpy.std.html
#
# - US EPA, OAR. (2014, July 8). Air Data: Air Quality Data Collected at Outdoor Monitors Across the US.
#   https://www.epa.gov/outdoor-air-quality-data

# ------------------------------------------------------------------------------
# End of Lab
# ------------------------------------------------------------------------------
