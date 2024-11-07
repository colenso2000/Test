#-----------------------------------------------------------------------------
# This script simulates and inertial version of the Taylor (99) rule from 
# 1987Q3 to 2009:Q4 using public available data.



#-----------------------------------------------------------------------------
# Housekeeping - Clear work space:
rm(list=ls())


#-----------------------------------------------------------------------------
# Load required packages:
library(fredr)
library(dplyr)
library(ggplot2)
library(readxl)
library(zoo)



#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# PART A - Simulation based on FRED and Blue Chips data.
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

# Variables and data sources:
# R*: Real Blue Chip estimate for nominal R*. Converted into real terms using 
# the Blue Chip estimate for GDP price deflator.
# cpi: Inflation is core CPI from FRED.
# gap: constructed using the estimate of potential GDP from the CBO in FRED.


#-----------------------------------------------------------------------------
# Retrieve data:

# Load Blue Chip R* forecast:
rstar <- read_xlsx("/Users/dv/Dropbox/Learning/11_taylor_rule/blue_chip_forecast.xlsx")

# Ensure the column names and date format are correct
colnames(rstar)[colnames(rstar) == "Median of natural rate estimates"] <- "value"
rstar$date <- as.Date(rstar$Date)

# Remove the "Date" column and reposition "date" as the first column
rstar <- rstar %>%
  select(-Date) %>%        # Remove the "Date" column
  select(date, everything())  # Move "date" to the first column




#-----------------------------------------------------------------------------
# Load FRED data:

# Set your FRED API key
fredr_set_key("711d25f225b04d86b2159bca07e18cf8")

# FredR units:	
# A string indicating the data value transformation. Defaults to "lin". Possible values are:
# "lin" - Levels (No transformation)
# "chg" - Change
# "ch1" - Change from 1 year ago
# "pch" - Percent change
# "pc1" - Percent change from 1 year ago
# "pca" - Compounded annual rate of change
# "cch" - Continuously compounded rate of change
# "cca" - Continuously compounded annual rate of change
# "log" - Natural log



# Define the start date for the data
start_date <- as.Date("1987-07-01")
start_date_pi <- as.Date("1985-07-01")
end_date<-as.Date("2009-12-01")


# Download the Federal Funds Rate (FEDFUNDS), CPI Inflation Rate, and Real GDP data from FRED
fed_funds_rate <- fredr(series_id = "FEDFUNDS", observation_start = start_date, observation_end = end_date)
#cpi <- fredr(series_id = "CPIAUCSL", observation_start = start_date_pi, observation_end = end_date, frequency = "q") # headline inflation
cpi <- fredr(series_id = "CPILFESL", observation_start = start_date_pi, observation_end = end_date, frequency = "q") # core inflation
gdp <- fredr(series_id = "GDPC1", observation_start = start_date, observation_end = end_date)
pot <- fredr(series_id = "GDPPOT", observation_start = start_date, observation_end = end_date)

# Create output gap variable:
gap <- gdp %>% mutate(value = 100*(gdp$value-pot$value)/pot$value )


# Calculate the quarterly inflation rate (percentage change from previous quarter)
cpi <- cpi %>%
   arrange(date) %>%                            # Ensure data is sorted by date
   mutate(inflation_rate = 100 * (value / lag(value, 4) - 1))  # Calculate quarterly inflation rate

# Compute the 4-quarter moving average of the inflation rate
 cpi <- cpi %>%
   mutate(inflation_rate_ma = rollmean(inflation_rate, k = 4, fill = NA, align = "right"))  # 4-quarter moving average


# Merge the datasets by date
data <- fed_funds_rate %>%
  rename(fed_funds = value) %>%
  inner_join(cpi %>% select(date, inflation_rate), by = "date") %>%
  inner_join(gap %>% rename(gap = value), by = "date") %>%
  #inner_join(xgap2 %>% rename(gap = value), by = "date") %>%
  inner_join(rstar %>% rename(r_star = value), by = "date")


# Specify the target inflation rate
pi_star <- 2  # Target inflation rate (e.g., 2% for the Fed)


#-----------------------------------------------------------------------------
# Baseline rule: Taylor rule with no asymmetric term (i.e.: baseline rule)

# Initialize Taylor rule in 1987:Q3
data$taylor_rate <- NA  # Initialize the column with NA values
# Step 2: Set the first value of 'taylor_rate' equal to the first value of 'fed_funds_rate'
data$taylor_rate[1] <- data$fed_funds[1]
# View the updated data
head(data)  

# Number of rows in the data
n <- nrow(data)

# Step 1: Loop through the rows, starting from the second row
for (i in 2:n) {
  # Check if the previous value of 'taylor_rate' is not NA
  if (!is.na(data$taylor_rate[i - 1])) {
    # Apply the formula for the current row
    data$taylor_rate[i] <- 0.85 * data$taylor_rate[i - 1] + 
      0.15 * (data$r_star[i] + 
              data$inflation_rate[i] + 
              0.5 * (data$inflation_rate[i] - pi_star) + 
              1.0 * data$gap[i] )
  }
}

# View the updated data
head(data)  # Display the first few rows to verify

# Plot Baseline Taylor Rule vs. the actual Federal Funds Rate:
g1 <- ggplot(data, aes(x = date )) +
  geom_line(aes(y = fed_funds, color = "Actual FFR")) +
  geom_line(aes(y = taylor_rate, color = "Baseline Rule")) +
  labs(title = "Taylor Rule vs. Actual Federal Funds Rate",
       y = "Interest Rate (%)",
       x = "Date") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "bottom")  # Move legend to the bottom



# Save simulation values:
taylor_1 <-data[,c("date","taylor_rate")]




#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# PART B - Simulation Comparison
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

# Plot Baseline Taylor Rule vs. the actual Federal Funds Rate:
g3<- ggplot(data, aes(x = date )) +
  geom_line(aes(y = fed_funds, color = "Actual FFR")) +
  geom_line(aes(y = taylor_rate, color = "Taylor (99)")) +
  labs(title = "Taylor Rule vs. Actual Federal Funds Rate",
       y = "Interest Rate (%)",
       x = "Date") +
  theme_minimal() +
  scale_color_manual(name = "Legend:", values = c("Actual FFR" = "blue", "Taylor (99)" = "red")) +
  theme(legend.position = "bottom")

print(g3)









