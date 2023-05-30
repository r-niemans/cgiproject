library(dplyr)
library(lubridate)
library(prophet)


df1 <- read.csv("datasets/full_data_wide_am.csv")
df1 <- df1[,-c(1,6,7,8,9:23)]
str(df1)
df1$month_year <- as.Date(df1$month_year) # convert to Date type


# Initialize an empty data frame
final_df_prophet <- data.frame()

# Get unique postal codes
postal_codes <- unique(df1$postal_code)

# Loop through each postal code
for (postal_code in postal_codes) {
  
  print(paste("Processing postal code:", postal_code))
  
  # Subset the data for the current postal code
  df_postcode <- df1 %>% filter(postal_code == postal_code)
  
  # Rename columns to ds and y
  df_postcode <- df_postcode %>% rename(ds = month_year, y = value_EV)
  
  # Make sure ds is Date type
  df_postcode$ds <- as.Date(df_postcode$ds)
  
  # Initialize the Prophet model
  m <- prophet()
  
  # Add additional regressor (charging point)
  m <- add_regressor(m, 'value_CP')
  
  # Fit the model
  m <- fit.prophet(m, df_postcode)
  
  # Create a data frame for future dates
  future <- make_future_dataframe(m, periods = as.numeric(difftime(as.Date('2030-01-01'), max(df_postcode$ds), units = "weeks") / 4), freq = 'month')
  
  # Assuming the value_CP (charging points) will remain the same as the latest available data
  future$value_CP <- tail(df_postcode$value_CP, n=1)
  
  # Predict future values
  forecast <- predict(m, future)
  
  # Filter out the forecasted data
  forecast_future <- forecast %>% filter(ds > max(df_postcode$ds))
  
  # Add postal_code to forecast_future
  forecast_future$postal_code <- postal_code
  
  # Combine the future dataframe with the original dataframe
  final_df_prophet <- rbind(final_df_prophet, forecast_future)
}

final_df_prophet_ev <- final_df_prophet_ev %>% rename(predicted_ev = yhat)


write.csv(final_df_prophet_ev, file = "output/prediction_ev_prophet.csv")

