library(tidyverse)
# Load the gapminder package
install.packages('gapminder')
library(gapminder)

# Load the dplyr package
library(dplyr)

# Look at the gapminder dataset
gapminder

# Filter the gapminder dataset for the year 1957
gapminder %>% filter(year == 1957)

# Filter for China in 2002
gapminder%>%
  filter(country == 'China', year == 2002)

# Sort in ascending order of lifeExp
gapminder%>%
  arrange(lifeExp)

# Sort in descending order of lifeExp
gapminder%>%
  arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
gapminder%>%
  filter(year == 1957) %>%
  arrange(desc(pop))

# Use mutate to change lifeExp to be in months
gapminder%>%
  mutate(lifeExp = lifeExp * 12)

# Use mutate to create a new column called lifeExpMonths
gapminder%>%
  mutate(lifeExpMonths = lifeExp * 12)

#Filter, mutate, and arrange the gapminder dataset
gapminder%>%
  filter(year == 2007)%>%
  mutate(lifeExpMonths = 12 * lifeExp)%>%
  arrange(desc(lifeExpMonths))

