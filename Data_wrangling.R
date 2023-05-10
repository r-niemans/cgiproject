rm(list = ls())
library(tidyverse)
library(ggmap)
library(jsonlite)
library(sf)
library(readr)
library(lubridate)
library(httr)
library(credentials)
library(reshape2)
library(tidyr)



# TODO: ADD ELECTRICITY - GAS as predictor (or sth like this)

# postcodes data 
# The file is to big to be uploaded to Github :(
# needs to be downloaded from - https://public.opendatasoft.com/explore/dataset/georef-netherlands-postcode-pc4/download/?format=geojson&timezone=Europe/Berlin&lang=en
geojson <- st_read("datasets/georef-netherlands-postcode-pc4.geojson")

# For now we drop the geometry
geojson <- st_set_geometry(geojson, NULL)
geojson$pc4_code <- as.numeric(geojson$pc4_code)


EV_2020 <- read.csv("datasets/BEV 2020.csv", sep = ";", row.names = NULL)
EV_2021 <- read.csv("datasets/BEV 2021.csv", sep = ";", row.names = NULL)
EV_2022 <- read.csv("datasets/BEV 2022.csv", sep = ";", row.names = NULL)

PHEV_2020 <- read.csv("datasets/PHEV 2020.csv", sep = ";", row.names = NULL)
PHEV_2021 <- read.csv("datasets/PHEV 2021.csv", sep = ";", row.names = NULL)
PHEV_2022 <- read.csv("datasets/PHEV 2022.csv", sep = ";", row.names = NULL)


colnames(EV_2020) <- c('code', paste0('m',c(1:12)))
colnames(EV_2021) <- c('code', paste0('m',c(1:12)))
colnames(EV_2022) <- c('code', paste0('m',c(1:12)))

colnames(PHEV_2020) <- c('code', paste0('m',c(1:12)))
colnames(PHEV_2021) <- c('code', paste0('m',c(1:12)))
colnames(PHEV_2022) <- c('code', paste0('m',c(1:12)))

data_chargers <- read.csv('datasets/Chargers_limburg.csv', row.names = NULL, sep = ';')



# changing postal codes to have only numbers
data_chargers$Postal.Code <- readr::parse_number(data_chargers$Postal.Code)
# counting chargers per postal code (for 2023)
chargers_postal <- data_chargers %>% count(Postal.Code)


chargers_postal <- merge(chargers_postal, geojson[,c(2,5)], by.x ='Postal.Code', by.y =  'pc4_code')

colnames(chargers_postal) <- c('Postal.Code', 'n', 'City')
# chargers_postal <- merge(chargers_postal, data_chargers[,c(2,3)], by = 'Postal.Code')
chargers_postal <- unique(chargers_postal)

# Our assumption (as we only have data for charging points over time at gemeente level) 
# is that the growth rate of cp remains the same for the whole gemeente

EV_month <- cbind(postal_code = EV_2020[,1], y2020 = EV_2020[,-1]+PHEV_2020[,-1], y2021 = EV_2021[,-1]+PHEV_2021[,-1],
                  y2022 = EV_2022[,-1]+PHEV_2022[,-1])

EV_month[is.na(EV_month)] <- 0

# We need to drop the 'postcode unknown' column
EV_month <- EV_month[-4067,]



EV_month$postal_code <-readr::parse_number(EV_month$postal_code)







#### Chargers!! ####

public_regular_2020 <- read.csv('datasets/Public regular 2020.csv', sep = ";", row.names = NULL)
public_regular_2021 <- read.csv('datasets/Public regular 2021.csv', sep = ";", row.names = NULL)
public_regular_2022 <- read.csv('datasets/Public regular 2022.csv', sep = ";", row.names = NULL)


public_fast_2020 <- read.csv('datasets/Public fast 2020.csv', sep = ";", row.names = NULL)
public_fast_2021 <- read.csv('datasets/Public fast 2021.csv', sep = ";", row.names = NULL)
public_fast_2022 <- read.csv('datasets/Public fast 2022.csv', sep = ";", row.names = NULL)


semi_public_regular_2020 <- read.csv('datasets/Semi-Public regular 2020.csv', sep = ";", row.names = NULL)
semi_public_regular_2021 <- read.csv('datasets/Semi-Public regular 2021.csv', sep = ";", row.names = NULL)
semi_public_regular_2022 <- read.csv('datasets/Semi-Public regular 2022.csv', sep = ";", row.names = NULL)

semi_public_fast_2020 <- read.csv('datasets/Semi-Public fast 2020.csv', sep = ";", row.names = NULL)
semi_public_fast_2021 <- read.csv('datasets/Semi-Public fast 2021.csv', sep = ";", row.names = NULL)
semi_public_fast_2022 <- read.csv('datasets/Semi-Public fast 2022.csv', sep = ";", row.names = NULL)


colnames(public_regular_2020) <- c('City', paste0('m',c(1:12)))
colnames(public_regular_2021) <- c('City', paste0('m',c(1:12)))
colnames(public_regular_2022) <- c('City', paste0('m',c(1:12)))

colnames(public_fast_2020) <- c('City', paste0('m',c(1:12)))
colnames(public_fast_2021) <- c('City', paste0('m',c(1:12)))
colnames(public_fast_2022) <- c('City', paste0('m',c(1:12)))

colnames(semi_public_regular_2020) <- c('City', paste0('m',c(1:12)))
colnames(semi_public_regular_2021) <- c('City', paste0('m',c(1:12)))
colnames(semi_public_regular_2022) <- c('City', paste0('m',c(1:12)))

colnames(semi_public_fast_2020) <- c('City', paste0('m',c(1:12)))
colnames(semi_public_fast_2021) <- c('City', paste0('m',c(1:12)))
colnames(semi_public_fast_2022) <- c('City', paste0('m',c(1:12)))



chargers_month <- cbind(City = semi_public_fast_2020[,1], y2020 = public_regular_2020[,-1] + public_fast_2020[,-1] + semi_public_fast_2020[,-1] +
                          semi_public_regular_2020[,-1],  y2021 = public_regular_2021[,-1] + public_fast_2021[,-1] + semi_public_fast_2021[,-1] +
                          semi_public_regular_2021[,-1],  y2022 = public_regular_2022[,-1] + public_fast_2022[,-1] + semi_public_fast_2022[,-1] +
                          semi_public_regular_2022[,-1])



chargers_postal_sum <- chargers_postal %>% group_by(City) %>% summarise(sum = sum(n))

chargers_postal <- merge(chargers_postal, chargers_postal_sum, by = 'City')


chargers_postal$percentage <- chargers_postal$n/chargers_postal$sum

postal_codes <- read.csv("datasets/postal_codes.csv", sep = ';')








#chargers_postal <- chargers_postal %>% left_join(geojson[,c(2,5)], by =join_by(Postal.Code == pc4_code))

chargers_postal <- left_join(chargers_postal, geojson[, c(2, 5)], 
                             by = c("Postal.Code" = "pc4_code"))

chargers_month[30,1] <- 'Bergen (L)'


Chargers_month_final <- chargers_postal %>% left_join(chargers_month, by = join_by(gem_name == City))



Chargers_month_final[,c(7:42)] <-Chargers_month_final$percentage * Chargers_month_final[,c(7:42)]



Chargers_month_final[,c(7:42)] <- round(Chargers_month_final[,c(7:42)])


# Creating a subset of EV df that captures only postcodes from Limburg area

EV_month <- subset(EV_month, postal_code %in% Chargers_month_final$Postal.Code)




Fuel_prices <- read.csv("datasets/Fuel_prices.csv", sep = ';')
Fuel_prices$Perioden <- ymd(Fuel_prices$Perioden)

Fuel_prices <- Fuel_prices %>% group_by(year(Perioden), month(Perioden)) %>% summarise(monthly_price = mean(BenzineEuro95_1))



# Mileage from cars - we need to wait for 2022 data (should be avaliable soon)
mileage_2020 <- read.csv("datasets/mileage 2020.csv", sep = ';', row.names = 1)
mileage_2021 <- read.csv("datasets/mileage 2021.csv", sep = ';', row.names = 1)

total_mileage <- cbind(y2020 = mileage_2020[,1], y2021 = mileage_2021[,1])
rownames(total_mileage) <- rownames(mileage_2020)
mean_mileage <- cbind(y2020 = mileage_2020[,2], y2021 = mileage_2021[,2])
rownames(mean_mileage) <- rownames(mileage_2020)


full_data <- cbind(postal_code = Chargers_month_final[,2], CP = Chargers_month_final[,c(7:42)], EV = EV_month[,-1])


cars_data <- read.csv('datasets/cars_limburg.csv')
colnames(cars_data) <- c('postal_code', 'year', 'value_cars')
write.csv(full_data, 'datasets/Full_data.csv')




EV_month_long <- gather(EV_month, key = "month_year", value = "value", -1)
CP_month_long <- gather(Chargers_month_final[,c(2,7:42)], key = "month_year", value = "value", -1)
colnames(CP_month_long) <- c("postal_code", "month_year",  "value")
merged_df <- merge(EV_month_long, CP_month_long, by = c("postal_code", "month_year"),
                   all.x = TRUE,suffixes = c("_EV", "_CP"))

Fuel_prices$month_year <- paste0('y',Fuel_prices$`year(Perioden)`,'.m',Fuel_prices$`month(Perioden)`)
full_data_wide <- merge(merged_df, Fuel_prices[,c(3,4)], by = c('month_year'))
# changing y2020.m1 to y2020.m01
full_data_wide$month_year <- str_replace(full_data_wide$month_year, "(?<=\\.m)\\d(?=$)", sprintf("%02d", as.integer(str_extract(full_data_wide$month_year, "(?<=\\.m)\\d(?=$)"))))

full_data_wide <- full_data_wide %>% arrange(postal_code, month_year)

full_data_wide$year  <- substr(full_data_wide$month_year, 2, 5)
full_data_wide <- merge(full_data_wide, cars_data, by = c('year', 'postal_code'))

full_data_wide <- full_data_wide[,-1]



# Replace 'y' with ''
full_data_wide$month_year <- gsub("y", "", full_data_wide$month_year)
full_data_wide$month_year <- gsub(".m", "-", full_data_wide$month_year)
# Add '-01' to the end of each date to create a full date format (YYYY-MM-DD)
full_data_wide$month_year <- paste(full_data_wide$month_year, "-01", sep="")

# Convert the column to Date format
full_data_wide$month_year <- as.Date(full_data_wide$month_year, format = "%Y-%m-%d")

write_csv(full_data_wide, 'datasets/wide_data_modelling.csv')

# TODO: Create mock data for monthly observations that will continue the trend

# We use interpolation to simulate monthly data where we have only yearly


# First we need to delete all the value in between the Januaries
df <- full_data_wide
df<- df %>% mutate(value_cars = ifelse(row_number() %% 12 == 1, value_cars, NA))

# Then we interpolate the values in between

df <- df %>%
  group_by(postal_code) %>%
  mutate(value_cars = approx(x = month_year, y = value_cars, xout = month_year, method = "linear", rule = 2)$y) %>%
  ungroup()
df$value_cars <- round(df$value_cars)

full_data_wide <- df
write_csv(full_data_wide, 'datasets/wide_data_modelling.csv')



