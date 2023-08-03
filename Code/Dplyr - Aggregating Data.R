# Load the package
library(tidyverse)

# Import the 2015 US Census County dateset
counties <- read_csv("Data/acs2015_county_data.csv")

## 'count' *** 'sort' ******* 'wt' ****************
# 'count' produces a new variable = the number of observations
# 'sort' arranges the data based on the most common observation
# 'wt' is weight - the new column will be weighted by the selected obesrvation

# Modify counties dataset
counties_selected <- counties %>%
  select(County, State, TotalPop, Citizen)

# Use count to find the number of counties in each State
counties_selected %>%
  count(State, sort = TRUE)

# Find number of counties per state, weighted by citizens, sorted in descending order
counties_selected %>%
  count(State, wt = Citizen, sort = TRUE)

# Modify counties dataset
counties_selected_2 <- counties %>%
  select(County, State, TotalPop, Walk)

counties_selected_2 %>%
  # Add population_walk containing the total number of people who walk to work 
  mutate(population_walk = (TotalPop / 100) * Walk) %>%
  # Count weighted by the new column, sort in descending order
  count(State, wt = population_walk, sort = TRUE)


## 'group_by' *** 'summarize' ******** 'ungroup' ***************
# 'summarize' - takes many observations and turns them into 1
# 'group_by' - groups by a chosen variable
# 'ungroup' - used when multiple variables are grouped by

# Modify counties dataset
counties_selected_3 <- counties %>%
  select(County, TotalPop, Income, Unemployment)

counties_selected_3 %>%
  # Summarize to find minimum population, maximum unemployment, and average income
  summarise(min_population = min(TotalPop),
            max_unemployment = max(Unemployment),
            average_income = mean(Income))

# Modify counties dataset
counties_selected_4 <- counties %>%
  select(County, TotalPop, Women, Unemployment)

counties_selected_4 %>%
  # Group by state 
  group_by(County) %>%
  # Find the total women and population
  summarise(total_women = sum(Women),
            total_population = sum(TotalPop))

counties_selected_4 %>%
  group_by(County) %>%
  summarize(total_women = sum(Women),
            total_population = sum(TotalPop)) %>%
  # Add a proportion column
  mutate(prop_women = total_population / total_women)%>%
  # Sort by proportion in descending order
  arrange(desc(prop_women))

# Modify counties dataset
counties_selected_5 <- counties %>%
  select(County, State, TotalPop, Unemployment)

counties_selected_5 %>%
  # Group and summarize to find the total population
  group_by(County, State) %>%
  summarize(total_pop = sum(TotalPop)) %>%
  # Calculate the average_pop and median_pop columns 
  summarize(average_pop = mean(total_pop),
            median_pop = median(total_pop))


## 'slice_min' *** 'slice_max' ***********************
# 'slice_max' - returns the largest observation in each group
# 'slice_min' - returns the smallest observation in each group
# 'n=_' - need to specify the number of observations to extract

# Modify counties dataset
counties_selected_6 <- counties %>%
  select(County, State, TotalPop, Income, Walk, Men, Women)

counties_selected_6 %>%
  # Group by region
  group_by(State) %>%
  # Find the county with the highest percentage of people who walk to work
  slice_max(Walk, n=1)

counties_selected_6 %>%
  group_by(State, County) %>%
  # Calculate average income
  summarise(average_income = mean(Income)) %>%
  # Find the lowest income state in each region
  slice_min(average_income, n=3)

# Modify counties dataset
counties_selected_7 <- counties %>%
  select(County, State, TotalPop, PublicWork)

counties_selected_7 %>%
  # Find the total population for each combination of state and county
  group_by(County, State) %>%
  summarise(total_public_work = sum(PublicWork)) %>% 
  # Extract the most populated row for each state
  slice_max(total_public_work, n=1) %>% 
  # Count the states with more people in public work
  ungroup() %>% 
  count(State)


