library(dplyr)

real_data <- read.csv("datasets/full_data_wide_am.csv", sep = ",")

ev_2023_data <- real_data %>%
  filter(month_year == as.Date("2022-01-01")) 

ev_2023 <- ev_2023_data %>%
  summarise(total_real_EV = sum(value_EV)) 

cp_2023 <- ev_2023_data %>%
  summarise(total_reav_CP = sum(value_CP))

ratio_2023 <- ev_2023/cp_2023

ratio_2023

prophet_cp_am <- read.csv("output/prediction_cp_prophet_am.csv", sep = ",")
prophet_cp <- read.csv("output/prediction_cp_prophet.csv", sep = ",")

prophet_ev <- read.csv("output/prediction_ev_prophet.csv", sep = ",")
prophet_ev_am <- read.csv("output/prediction_ev_am_prophet.csv", sep = ",")

# Assuming your dataset is named 'df'
export_data <- cbind(export_data, charger_data_kwh_generated)

# Print the updated dataset
print(df)

filtered_data <- prophet_ev %>%
  filter(month_year == as.Date("2029-12-01")) 

filtered_data_cp <- prophet_cp %>%
  filter(month_year == as.Date("2029-12-01")) 



sum_predicted_EV <- filtered_data %>%
  summarise(total_predicted_EV = sum(predicted_ev))

sum_predicted_CP <- filtered_data_cp %>%
  summarise(total_predicted_cp = sum(predicted_cp))


ratio_2030 <- sum_predicted_EV/sum_predicted_CP

