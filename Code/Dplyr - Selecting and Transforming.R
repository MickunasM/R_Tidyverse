# Load the package
library(tidyverse)

# Import the 2015 US Census County dateset
counties <- read_csv("Data/acs2015_county_data.csv")

## 'select' ********************************
# can 'select' a range using ':'
# 'select' helpers are below
# 'contains', 'starts_with', 'ends_with', 'last_col', 'matches' - need to use ""
# can also use 'select' to remove colums using the '-' sign

# Glimpse the counties table
glimpse(counties)

counties %>%
  # Select state, county, population, and industry-related columns
  select(State, County, TotalPop, Professional:Production) %>%
  # Arrange service in descending order 
  arrange(desc(Service))

counties %>%
  # Select the state, county, population, and those ending with "work"
  select(State, County, TotalPop, ends_with("work")) %>%
  # Filter for counties that have at least 50% of people engaged in public work
  filter(PublicWork > 50)


## 'rename' ********************************
# changes the name of a column
# can also rename using 'select' - this is good for renaming multiple columns

counties %>%
  # Count the number of counties in each state
  count(State) %>% 
  # Rename the n column to num_counties
  rename(num_counties = n)

counties %>%
  # Select state, county, and poverty as poverty_rate
  select(State, County, Poverty_Rate = Poverty)

## 'transmute' ********************************
# combination of 'select' and 'mutate'
# returns a subset of columns but also transforms and changes the columns

counties %>%
  # Keep the state, county, and populations columns, and add a density column
  transmute(State, County, TotalPop, Poverty_Percent = Poverty / TotalPop * 100) %>%
  # Filter for counties with a population greater than one million 
  filter(TotalPop > 1000000) %>%
  # Sort density in ascending order 
  arrange(Poverty_Percent)

# Change the name of the unemployment column
counties %>%
  rename(unemployment_rate = Unemployment)
# Keep the state and county columns, and the columns containing poverty
counties %>%
  select(State, County, contains("Poverty"))
# Calculate the fraction_women column without dropping the other columns
counties %>%
  mutate(fraction_women = Women / TotalPop)
# Keep only the state, county, and employment_rate columns
counties %>%
  transmute(State, County, employment_rate = Employed / TotalPop)
