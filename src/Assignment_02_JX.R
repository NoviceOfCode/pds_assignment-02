install.packages("dplyr")
install.package("stringr")
library(dplyr)
install.package("readr")
library(readr)
train <- read_csv("data_raw/train.csv")

#Question a
#Finding counts for missing values
column_na_values_count_df <- data.frame(colnames(train), colSums(is.na(train)))

#Finding the rows with na for mileage
missing_milieage_values <- train %>%
  filter(is.na(Mileage))

matching_mahindra <- train %>%
  filter(Name == "Mahindra E Verito D4" & Year == 2016)
matching_toyota <- train %>%
  filter(Name == "Toyota Prius 2009-2016 Z4" & Year == 2011)

#Finding rows missing engine
grouping_by_name_year <- train %>%
  group_by(Name, Year) %>%  # Group by Name and Year
  mutate(count_identical_name_and_year = n() - 1)

missing_engine <- grouping_by_name_year %>%
  filter(is.na(Engine))

#Question b
library(stringr)

adjusted <- train %>%
  mutate(Mileage = gsub(" kmpl| km/kg", "", Mileage))
adjusted_engine <- adjusted %>%
  mutate(Engine = gsub(" CC", "", Engine)) #Removing CC
adjusted_power <- adjusted_engine %>%
  mutate(Power = gsub(" bhp", "", Power)) # Removing bhp
final_adjusted <- adjusted_power %>%
  mutate(New_Price = gsub(" Lakh", "", New_Price)) #Removing lakh

#Question C
library(tidyr)
#One hot encoding for Fuel Types
unique_fuel_types <- unique(train$Fuel_Type)
fuel_type_one_hot <- train %>%
  mutate(Petrol = ifelse(Fuel_Type == "Petrol", 1, 0),
         Diesel = ifelse(Fuel_Type == "Diesel", 1, 0),
         Electric = ifelse(Fuel_Type == "Electric", 1, 0))
final_fuel_type_one_hot <- fuel_type_one_hot %>%
  select(-Fuel_Type)

# One Hot Recording for Transmission
unique_transmission <- unique(train$Transmission)
transmission_one_hot_encoded <-final_fuel_type_one_hot %>%
  mutate(Manual = ifelse(Transmission == "Manual", 1, 0),
         Automatic = ifelse(Transmission == "Automatic", 1, 0))
final_transmission <- transmission_one_hot_encoded %>%
  select(-Transmission)


#Question D adding a feature
current_year_car_train <- train %>%
  mutate( Car_Age = 2024 -Year)

#Question E
library(tidyverse)

car_location_seats <- train %>%
  drop_na(Seats) %>%
  group_by(Location) %>%
  summarise('Average Number of Seats' = mean(Seats)) %>%
  view
car_location_price <- train %>%
  drop_na(Price) %>%
  group_by(Location) %>%
  arrange(desc(Price)) %>%
  rename(Current_Market_Price = Price) %>%
  select(Location, Current_Market_Price) %>%
  view
bad_estimated_km_per_year <- train %>%
  mutate(Estimated_Mileage_Per_Year = (2024 - Year) / Kilometers_Driven) %>%
  group_by(Name) %>%
  select(Name, Estimated_Mileage_Per_Year) %>%
  view