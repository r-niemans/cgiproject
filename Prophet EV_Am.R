## Prophet script for predictions of EV's considering amenitieis and charging points
library(prophet)
library(dplyr)

df1 <- read.csv("datasets/full_data_wide_am.csv")
df1 <- df1[,-c(1,6,7,8)]
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
  df_postcode <- df1[df1$postal_code == postal_code, ]
  
  # Remove the postal_code column
  df_postcode <- df_postcode[, !(colnames(df_postcode) %in% "postal_code")]
  
  # Rename columns to ds and y
  df_postcode <- df_postcode %>% rename(ds = month_year, y = value_EV)
  
  # Make sure ds is Date type
  df_postcode$ds <- as.Date(df_postcode$ds)
  
  # Initialize the Prophet model
  m <- prophet()
  
  # Add additional regressors (amenities) to the model
  amenities <- colnames(df_postcode)[4:ncol(df_postcode)]
  for (amenity in amenities) {
    m <- add_regressor(m, amenity)
  }
  
  # Fit the model with data for the current postal code
  m <- fit.prophet(m, df_postcode)
  
  # Create a data frame for future dates specific to the current postal code
  future <- make_future_dataframe(m, periods = as.numeric(difftime(as.Date('2030-01-01'), max(df_postcode$ds), units = "weeks") / 4), freq = 'month')
  
  # Add values for the additional regressors in the future data frame
  for (amenity in amenities) {
    future[[amenity]] <- rep(max(df_postcode[[amenity]]), nrow(future))
  }
  
  # Predict future values
  forecast <- predict(m, future)
  
  # Filter out the forecasted data until 1 January 2030
  forecast_future <- forecast %>% filter(ds <= as.Date('2030-01-01'))
  
  # Add postal_code to forecast_future
  forecast_future$postal_code <- postal_code
  
  # Combine the forecasted data with the original dataframe
  final_df_prophet <- rbind(final_df_prophet, forecast_future)
}

final_df_prophet_ev_am <- final_df_prophet %>% rename(predicted_ev = yhat)
final_df_prophet_ev_am <- final_df_prophet_ev_am  %>% rename(month_year = ds)

write.csv(final_df_prophet_ev_am, file = "datasets/prediction_ev_am_prophet.csv")

