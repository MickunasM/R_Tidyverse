# Load the necessary packages.
library(tidyverse)
library(skimr)

# Import the datasets.
library(gapminder)
data(gapminder) # The gapminder dataset tracks social and economic indicators (GDP per capita and life expectancy) in countries over time.

library(nycflights13)
data(flights) # The nycfilghts13 dataset contains information on flights departing New York City in 2013.
# This package also provides the following data:
# weather --> hourly meterological data for each airport
# planes --> construction information about each plane
# airports --> airport name and locations
# airlines -- translatoin between two letter carrier code and names

# Now that we have the package and datasets loaded, lets inspect them.
glimpse(gapminder) # Contains 1704 observations (rows) and 6 variables (columns) such as "country", "continent", "year", "lifeExp", "pop" and "gdpPercap".

glimpse(flights) # Contains 336,776 observations and 19 variables such as "year", "month", "tailnum", "origin", "carrier", "dep_delay", "arr_delay" etc.


# Now that we have some familiarity with the datasets, lets put the tidyverse package to use.

# What Countries had the highest `lifeExp` in 2007 in gapminder?
gapminder %>% # The pipe (%>%) operator takes the information on its left and passes it to the function on its right - "and then"
  # Extract only the year of interest using filter
  filter(year == 2007) %>% # 
  # Identify only the columns of interest using select
  select(country, continent, lifeExp) %>% 
  # Arrange in descending order to find the highest life expectancy
  arrange(desc(lifeExp))


# What is the total GDP, the maximum population and median life expectancy?
# We can use summarise() and its built in sum, max, median functions to aggregate across rows and reduce multiple values down to a single summary.
gapminder %>% 
  summarise(total_GDP = sum(gdpPercap), 
            max_pop = max(pop),
            median_lifeExp = median(lifeExp))


# We can also use summarise() to count using n() and n_distinct().
gapminder %>% 
  summarise(count = n()) # Counts the number of rows in the dataset

gapminder %>% 
  summarise(count = n(), # Creates a new 
            distinct_continents = n_distinct(country))


# We can group cases by common values of one or more columns using group_by().
gapminder %>% 
  group_by(continent)

gapminder %>% 
  group_by(continent, year)

# We can also combine functions for more detailed analysis, for example group_by()  and summarise().
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_lifexp = mean(lifeExp))

gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifexp = mean(lifeExp))

# Lets explore the flights data set
# Count the number of flights departing from each `origin` airport
flights %>% 
  count(origin, sort = TRUE) %>%  # `sort=TRUE` gives table in descending order
  mutate(prop = n/sum(n)) # mutate() generates a new column called `prop` which is the proportion of flights calculated as number of flights `n` divided by the `sum(n)`

# What was the longest arr delay?
flights %>% 
  filter(year == "2013")
  arrange(desc(arr_delay))

# We can also ask more specific questions, such as what was the average arr_delay for UA carrier?
flights %>% 
  filter(carrier == 'UA') %>% #Choose only the UA carrier
  summarise(mean_arrival_delay = mean(arr_delay, na.rm = TRUE)) # na.rm = TRUE will disregard any missing values (NA)

# What if we wanted to know the mean dep_delay and arr_delay for all airlines?
flights %>% 
  group_by(carrier) %>%
  summarise(
    count = n(),
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(count))
      












