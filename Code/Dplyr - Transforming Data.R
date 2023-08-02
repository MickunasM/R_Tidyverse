# Load the package
library(tidyverse)

# Import the US Census dateset
census <- read_csv("Data/census.csv")
glimpse(census)

## 'select' *** 'arrange' ***********************
# 'select' particular variables
census %>%
  # Select the columns
  select(STATE, REGION, STNAME, CTYNAME)

census %>%
  # Add a verb to sort in descending order of 2010 population
  arrange(desc(CENSUS2010POP))


## 'filter' *************************************
census %>%
  # Filter for 2010 population above 1000000
  filter(CENSUS2010POP > 1000000)

census %>%
  # Filter for California cities with a 2010 population above 1000000
  filter(STNAME == "California",
         CENSUS2010POP > 1000000)

census %>%
  select(STNAME, CTYNAME, CENSUS2010POP, DEATHS2010) %>% 
  # Filter for Texas and more than 10000 people in 2010
  filter(STNAME == "Texas",
         CENSUS2010POP > 10000) %>%
  # Sort in descending order of 2010 Deaths
  arrange(desc(DEATHS2010))


## 'mutate' *************************************
census_2 <- census %>%
  select(STNAME, CTYNAME, CENSUS2010POP, DEATHS2010)

census_2 %>%
  # Add a new column percentage_deaths with the number of people that died in 2010 as a percentage
  mutate(percentage_deaths =  DEATHS2010 / CENSUS2010POP * 100)

census_2 %>%
  mutate(percentage_deaths =  DEATHS2010 / CENSUS2010POP * 100) %>%
  # Sort in descending order of the percentage_deaths column
  arrange(desc(percentage_deaths))

census_2 %>%
  # Calculate proportion_deaths as the fraction of the 2010 population
  mutate(proportion_deaths = DEATHS2010/ CENSUS2010POP)


## Bring it all together **************************
census %>%
  # Select the 4 columns 
  select(STNAME, CTYNAME, CENSUS2010POP, DEATHS2010) %>% 
  # Add the proportion_deaths variable
  mutate(proportion_deaths = DEATHS2010/ CENSUS2010POP) %>% 
  # Filter for population of at least 10,00 in 2010
  filter(CENSUS2010POP > 10000) %>% 
  # Arrange proportion_deaths in descending order 
  arrange(desc(proportion_deaths))
