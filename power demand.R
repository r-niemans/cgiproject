library(dplyr)
library(readxl)
library(ggplot2)
library(e1071)
library(caret)
setwd("/Users/raf/Documents")
energydemand <- read_excel("charging_profile_regular.xlsx")

energydemand$year <- as.numeric(format(energydemand$date_time, "%Y"), origin = energydemand$date_time)
energydemand$month <- as.numeric(format(energydemand$date_time, "%m"), origin = energydemand$date_time)
energydemand$day <- as.numeric(format(energydemand$date_time, "%d"), origin = energydemand$date_time)
energydemand$hour <- as.numeric(format(energydemand$date_time, "%H"), origin = energydemand$date_time)
energydemand$minute <- as.numeric(format(energydemand$date_time, "%M"), origin = energydemand$date_time)

# group by month with average power demand 
grouped_month <- energydemand %>%
  group_by(month) %>% 
  summarise(avg_pubpower = mean(power_demand_public))
  
grouped_year <- energydemand %>%
  group_by(year) %>% 
  summarise(avg_pubdemand = mean(power_demand_public))

energydemand <- data.frame(energydemand)

# change to timeseries 
vectordemand <- as.vector(energydemand$power_demand_public)
vectordemand <- as.vector(grouped_month$avg_pubpower)

# create a timeseries, based on 12 months
energydemand_ts <- ts(vectordemand, start = c(2021, 1), end= c(2022,1), frequency = 12)

#plot(energydemand, xlab = "date_time", ylab = "power_demand_public", 
   #  main = "daily power_demand, insert days", type ="b")

# this plot shows that the power demand goes up quite a lot in the last months, could be due to increase in electrical vehicles  or the fact 
# that its winter and people take the cars more often 
ggplot(grouped_month, aes(x = month, y = avg_pubpower)) +
  geom_line() +
  labs(x = "Month", y = "Power Demand (kW)", title = "Power Demand of Charging Station")

# this is the normal amount of demand shown over a year
ggplot(energydemand, aes(x = date_time, y = power_demand_public)) +
  geom_line() +
  scale_x_datetime(date_labels = "%m", date_breaks = "1 month") +
  labs(x = "Time: Month", y = "Power Demand (kW)", title = "Power Demand of Charging Station")

plot(energydemand_ts)
plot(decompose(energydemand_ts))
str(energydemand)

lr <- lm(power_demand_public ~ month + year + day, data= energydemand)
summary(lr)

logm <- glm(power_demand_public ~ month + year + day, data= energydemand)
summary(logm)

