rm(list = ls())

df <- read.csv('datasets/wide_data_modelling.csv')

# Order the data by month_year
df <- df[order(df$month_year),]
df$postal_code <- factor(df$postal_code)

# add lagged values -> for the last month

df$first_diff <- 0
for (i in unique(df$postal_code)) {
df[df$postal_code == i, 7 ] <- df %>% filter(postal_code==i) %>% reframe(first_diff = lag(value_CP))
}

df[is.na(df)] <- 0



# Split the dataset into training and testing sets
train_df <- df[df$month_year < as.Date("2022-08-01"),]
test_df <- df[df$month_year >= as.Date("2022-08-02"),]

# Convert the training set's value_CP column to a time series object with frequency 12
train_ts <- ts(train_df$value_CP, frequency = 12)


library(dplyr)
library(forecast)

# Preprocessing and splitting the data

# Fit the ARIMA model for each postal_code
postal_codes <- unique(df$postal_code)
results <- list()

for (postal_code in postal_codes) {
  train_subset <- train_df[train_df$postal_code == postal_code,]
  train_ts <- ts(train_subset$value_CP, frequency = 12)
  
  arima_model <- auto.arima(train_ts)
  future_months <- length(test_df$month_year)
  predictions <- forecast(arima_model, h = future_months)
  
  results[[as.character(postal_code)]] <- list(model = arima_model, predictions = predictions)
}

# Evaluate the model's performance using mean absolute error (MAE)
mae <- function(actual, predicted) {
  return(mean(abs(actual - predicted)))
}

mae_results <- data.frame(postal_code = character(), mae = numeric())

for (postal_code in postal_codes) {
  test_subset <- test_df[test_df$postal_code == postal_code,]
  predictions <- results[[as.character(postal_code)]]$predictions$mean
  
  postal_code_mae <- mae(test_subset$value_CP, predictions)
  mae_results <- rbind(mae_results, data.frame(postal_code = postal_code, mae = postal_code_mae))
}

# Print the mean absolute error (MAE) results for each postal_code
print(mae_results)









