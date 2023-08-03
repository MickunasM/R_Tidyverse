# Load the package
library(tidyverse)

# Import the babynames dateset
load("Data/babynames.rda")

# rename the n column to number
babynames <- babynames %>% 
  rename(number = n)

glimpse(babynames)

babynames %>%
  # Filter for the year 1990
  filter(year == 1990) %>%
  # Sort the number column in descending order 
  arrange(desc(number))

babynames %>%
  # Find the most common name in each year
  group_by(year) %>%
  slice_max(number, n=1)

selected_names <- babynames %>%
  # Filter for the names Steven, Thomas, and Matthew 
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Plot the names using a different color for each name
ggplot(selected_names, aes(x = year, y = number, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Year",
       y = "Number",
       color = "Name:") 

## combining 'group_by' and 'mutate' ********************************
# Calculate the fraction of people born each year with the same name
babynames_fraction <- babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>% 
  # Find the year each name is most common
  group_by(name) %>%
  slice_max(fraction, n = 1)

names_normalised <- babynames %>%
  # Add columns name_total and name_max for each name
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  # Ungroup the table 
  ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum 
  mutate(fraction_max = number / name_max)

names_filtered <- names_normalised %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name == "Steven" | name == "Thomas" | name == "Matthew")

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, colour = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Year",
       y = "Fraction of Names",
       color = "Name:") 


## window function ********************************
# takes a vector and returns another vector of the same lenght
# using 'lag' - moving each vector to the right by 1

babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio of fraction between each year 
  mutate(ratio = fraction / lag(fraction))

babynames_ratios_filtered <- babynames_fraction %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(ratio = fraction / lag(fraction)) %>%
  filter(fraction >= 0.00001)

babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  slice_max(ratio) %>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)