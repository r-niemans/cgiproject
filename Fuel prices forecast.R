library(dplyr)
library(lubridate)
library(tidyr)
library(forecast)
# Predict monthly prices for gas prices for each postal code
df <- read.csv("datasets/full_data_wide_am.csv")
# Remove variables we wont need
df <- df[,-c(1,4,5,7,9:23)]
df <- df[,-c(4)]
str(df)
# assuming your dataframe is named df and the columns are 'date', 'price', and 'postcode'
df$month_year <- as.Date(df$month_year) # convert to Date type
# Initialize an empty data frame to store the final results
final_df_arima <- data.frame()

# Get the unique postal codes
postal_codes <- unique(df$postal_code)

# Loop through each unique postal code
# Loop through each postal code
# Loop through each postal code
# Loop through each postal code
for (postal_code in postal_codes) {
  
  print(paste("Processing postal code:", postal_code))
  
  # Subset the data for the current postal code
  df_postcode <- df %>% filter(postal_code == postal_code)
  
  # Convert the dataframe to a ts object
  ts_postcode <- ts(df_postcode$monthly_price, start = c(2020, 1), frequency = 12)
  
  # Fit the ARIMA model
  model <- auto.arima(ts_postcode)
  
  # Create a new data frame for future dates
  future_dates <- data.frame(month_year = seq(max(df_postcode$month_year), as.Date('2030-01-01'), by = 'month'))
  
  # Calculate the number of months to forecast
  n_months <- length(future_dates$month_year)
  
  # Predict the prices for the future dates
  future_forecast <- forecast(model, h = n_months)
  
  # Add the forecasted prices to future_dates
  future_dates$predicted_price <- future_forecast$mean
  
  # Add postal_code to future_dates
  future_dates$postal_code <- postal_code
  
  # Rename 'monthly_price' to 'predicted_price' in df_postcode
  df_postcode <- df_postcode %>% rename(predicted_price = monthly_price)
  
  # Combine the future dataframe with the original dataframe
  final_df_arima <- rbind(final_df_arima, df_postcode, future_dates)
}
# remove duplicates
final_df_arima_f <- distinct(final_df_arima, postal_code, month_year, .keep_all = TRUE)

write.csv(final_df_arima_f, file = "datasets/prediction_gas.csv")
