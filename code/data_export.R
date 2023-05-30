rm(list = ls())
library(tidyverse)
library(lubridate)
library(tseries)


hist_data <- read.csv('datasets/full_data_wide_am.csv')[,c(2:6)]

hist_data$value_EV_optimistic <- hist_data$value_EV
hist_data$value_EV_pesimistic <- hist_data$value_EV

hist_data$value_CP_optimistic <- hist_data$value_CP
hist_data$value_CP_pesimistic <- hist_data$value_CP
pred_EV <- read.csv('output/prediction_ev_prophet.csv')[,c(2,18,17,14,13)]
pred_CP <- read.csv('output/prediction_cp_prophet.csv')[,c(2,18,17,14,13)]
pred_price <- read.csv('output/prediction_gas.csv')

pred_EV$month_year <- ymd(pred_EV$month_year)
pred_CP$month_year <- ymd(pred_CP$month_year)
hist_data$month_year <- ymd(hist_data$month_year)
pred_price$month_year <- ymd(pred_price$month_year)


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


# geojson file is too large to be uploaded on github
geojson <- st_read("datasets/georef-netherlands-postcode-pc4.geojson")

# For now we drop the geometry
geojson <- st_set_geometry(geojson, NULL)
geojson$pc4_code <- as.numeric(geojson$pc4_code)

export_data <- export_data %>% left_join(geojson[,c(1,2,5)], by = join_by('postal_code' == 'pc4_code'))
export_data$year <- year(export_data$month_year)
export_data[,c(3,4,6,7,8,9)] <- round(export_data[,c(3,4,6,7,8,9)])
export_data[,5] <- round(export_data[,5],2)


write.csv(export_data, 'datasets/Tableu_data.csv')

