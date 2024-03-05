install.packages("dplyr")
install.package("stringr")
library(dplyr)
train <- read_csv("train.csv")

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
