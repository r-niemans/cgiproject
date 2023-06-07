
## Prophet script for predictions of CP's considering eletrical vehicles
rm(list = ls())
library(dplyr)
library(prophet)
library(MLmetrics)
library(lubridate)
df1 <- read.csv("datasets/full_data_wide_am.csv")
df1 <- df1[,-c(1,6,7,8,9:23)]

df1$month_year <- as.Date(df1$month_year) # convert to Date type

# Initialize an empty data frame
final_df_prophet <- data.frame()

df_train <- df1[df1$month_year < as.Date('2021-12-02'),]
df_test <- df1[df1$month_year > as.Date('2021-12-02'),]
# Initialize an empty data frame
final_df_prophet <- data.frame()
# Get unique postal codes
postal_codes <- unique(df_train$postal_code)

# Loop through each postal code
for (postal_code in postal_codes) {
  
  print(paste("Processing postal code:", postal_code))
  
  # Subset the data for the current postal code
  df_postcode <- df_train[df_train$postal_code == postal_code, ]
  
  # Remove the postal_code column
  df_postcode <- df_postcode[, !(colnames(df_postcode) %in% "postal_code")]
  
  # Rename columns to ds and y
  df_postcode <- df_postcode %>% rename(ds = month_year, y = value_CP)
  
  # Make sure ds is Date type
  df_postcode$ds <- as.Date(df_postcode$ds)
  
  # Initialize the Prophet model
  m <- prophet()
  
  # Fit the model with data for the current postal code
  m <- fit.prophet(m, df_postcode)
  
  # Create a data frame for future dates specific to the current postal code
  future <- make_future_dataframe(m, periods = as.numeric(difftime(as.Date('2022-12-01'), max(df_postcode$ds), units = "weeks") / 4), freq = 'month')
  
  # Predict future values
  forecast <- predict(m, future)
  
  # Filter out the forecasted data 
  forecast_future <- forecast %>% filter(ds <= as.Date('2030-01-01'))
  
  # Add postal_code to forecast_future
  forecast_future$postal_code <- postal_code
  
  # Combine the forecasted data with the original dataframe
  final_df_prophet <- rbind(final_df_prophet, forecast_future)
}

final_df_prophet_cp <- final_df_prophet %>% rename(predicted_cp = yhat)
final_df_prophet_cp <- final_df_prophet_cp %>% rename(month_year = ds)


preds <- final_df_prophet_cp$predicted_cp[ymd(final_df_prophet_cp$month_year) > ymd('2022-01-02')]
dates <- final_df_prophet_cp$month_year[ymd(final_df_prophet_cp$month_year) > ymd('2022-01-02')]
eval <- tibble(dates, preds, real = df_test$value_CP)
eval$error <- eval$preds - eval$real

# MAFE and RMFE
eval %>% group_by(dates) %>% summarise(RMSE = RMSE(preds, real), MAE = mean(abs(error)))




# Calculate the total sum of squares (TSS)
TSS <- sum((df_test$value_CP - mean(df_test$value_CP))^2)

# Calculate the residual sum of squares (RSS)
RSS <- sum((df_test$value_CP - preds)^2)

# Calculate R^2
R_squared <- 1 - (RSS / TSS)


### Hit rate within the Confidence interval ###
filtered_df <- final_df_prophet_cp[ymd(final_df_prophet_cp$month_year) > ymd('2022-01-02'),]

CI <- tibble(dates, min = filtered_df$yhat_lower, max = filtered_df$yhat_upper)
CI$hit <- ifelse(df_test$value_CP > CI$min & df_test$value_CP < CI$max, 1,0)
CI %>% group_by(dates) %>% summarise(hit_rate = mean(hit))
df_test$value_CP[999]
