rm(list = ls())
library(tidyverse)
library(lubridate)
library(tseries)
library(sf)

# Read in dataset with energy consumption
charger_data <- read.csv("datasets/evdata.csv", sep = ",")
# Select only column with the monthly energy consumption of the selected datapoint
charger_data_kwh_m <- charger_data[,12]
charger_data_kwh_m
# Set which is the minimal value to set the treshold for generation
min(charger_data_kwh_m)
# Set a seed for reproducibility
set.seed(42)  
# Take mean and sd for reporducibility
mean_kwh <- mean(charger_data_kwh_m)
sd_kwh <- sd(charger_data_kwh_m)
# TNumber of total forecasted observations in months we have in the final dataset
num_values <- 28677
# Generate random kWh usage values with a minimum threshold
min_threshold <- 93.5
charger_data_kwh_generated <- pmax(abs(rnorm(28677, mean = mean(charger_data_kwh_m), sd = sd(charger_data_kwh_m))), min_threshold)

# Add the generated values to the export_data dataset
export_data$charger_data_kwh_generated <- charger_data_kwh_generated


# Add the generated values to the export_data dataset
export_data$charger_data_kwh_generated <- charger_data_kwh_generated

# Round the generated values to two decimal places
charger_data_kwh_generated <- round(charger_data_kwh_generated, 2)

# Print the first 10 values of the generated vector
print(charger_data_kwh_generated[1:10])
# Read in full dataset
hist_data <- read.csv('datasets/full_data_wide_am.csv')[,c(2:6)]
hist_data$value_EV_optimistic <- hist_data$value_EV
hist_data$value_EV_pesimistic <- hist_data$value_EV
hist_data$value_CP_optimistic <- hist_data$value_CP
hist_data$value_CP_pesimistic <- hist_data$value_CP
pred_EV <- read.csv('datasets/prediction_ev_prophet.csv')[,c(2,18,17,14,13)]
pred_CP <- read.csv('datasets/prediction_cp_prophet.csv')[,c(2,18,17,14,13)]
pred_price <- read.csv('datasets/prediction_gas.csv')
# Convert them to dates
pred_EV$month_year <- ymd(pred_EV$month_year)
pred_CP$month_year <- ymd(pred_CP$month_year)
hist_data$month_year <- ymd(hist_data$month_year)
pred_price$month_year <- ymd(pred_price$month_year)
# Add predictions to the main dataset
pred_EV <- pred_EV[pred_EV$month_year > as.Date("2022-12-01"),]
pred_CP <- pred_CP[pred_CP$month_year > as.Date("2022-12-01"),]
pred_price <- pred_price[pred_price$month_year > as.Date("2022-12-01"),]
pred_EV <- pred_EV %>% arrange(month_year, postal_code)
pred_CP <- pred_CP %>% arrange(month_year, postal_code)
pred_price <- pred_price %>% arrange(month_year, postal_code)
forecast_data <- tibble(postal_code = pred_EV$postal_code, month_year = pred_EV$month_year, 
                        value_EV = pred_EV$predicted_ev, value_EV_optimistic = pred_EV$yhat_upper, 
                        value_EV_pesimistic = pred_EV$yhat_lower, value_CP = pred_CP$predicted_cp,
                        value_CP_optimistic = pred_CP$yhat_upper, value_CP_pesimistic = pred_CP$yhat_lower, 
                        monthly_price = pred_price$predicted_price)
export_data <- rbind(hist_data, forecast_data)

geojson <- st_read("datasets/georef-netherlands-postcode-pc4.geojson")

# For now we drop the geometry
geojson <- st_set_geometry(geojson, NULL)
geojson$pc4_code <- as.numeric(geojson$pc4_code)

export_data <- export_data %>% left_join(geojson[,c(1,2,5)], by = join_by('postal_code' == 'pc4_code'))
export_data$year <- year(export_data$month_year)
export_data[,c(3,4,6,7,8,9)] <- round(export_data[,c(3,4,6,7,8,9)])
export_data[,5] <- round(export_data[,5],2)


str(export_data)
# Replace negative predicted values pf charging points with 0
export_data[c(9195, 11091), 9] <- 0
# Calculate the predicted usage
export_data$predicted_usage <- export_data$value_CP * export_data$charger_data_kwh_generated

# Calculate the pessimistic usage
export_data$pessimistic_usage <- export_data$value_CP_pesimistic * export_data$charger_data_kwh_generated

# Calculate the optimistic usage
export_data$optimistic_usage <- export_data$value_CP_optimistic * export_data$charger_data_kwh_generated

# Rename column 13
colnames(export_data)[13] <- "kwh_used_month"

write.csv(export_data, 'output/Tableu_data.csv')
