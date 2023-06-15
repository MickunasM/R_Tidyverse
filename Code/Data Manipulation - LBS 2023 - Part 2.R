# Load the necessary packages 
library(tidyverse)
library(nycflights13)
library(skimr)

glimpse(flights)

# Data Manipulation:
# Problem 1 - Use logical operators to find flights that:

# Had an arrival delay of two or more hours (> 120 minutes)
flights %>% 
  filter(arr_delay >= 120)
  
# Flew to Houston (IAH or HOU)
flights %>% 
  filter(dest %in% c('IAH', 'HOU'))

# Were operated by United (`UA`), American (`AA`), or Delta (`DL`)
flights %>% 
  filter(carrier %in% c('UA', 'AA', 'DL'))

# Departed in summer (July, August, and September)
flights %>% 
  filter(month %in% c(7,8,9))

# Arrived more than two hours late, but didn't leave late
flights %>% 
  filter(dep_delay == 0, arr_delay > 120)

# Were delayed by at least an hour, but made up over 30 minutes in flight
flights %>% 
  filter(dep_delay >= 60, arr_delay <= -30)

# Problem 2: What months had the highest and lowest proportion of cancelled flights?
flights %>% # Create a new variable containing just the result
  mutate(cancelled = ifelse(is.na(dep_time), TRUE, FALSE)) %>% 
  group_by(month, cancelled) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count))

# Lets plot the cancellations by month:
flights %>% 
  mutate(cancelled = ifelse(is.na(dep_time), TRUE, FALSE)) %>% 
  group_by(month, cancelled) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count)) %>% 
  filter(cancelled == TRUE) %>% #  # only show those flights that were cancelled and pipe the resulting dataframe to ggpplot
  ggplot() +  # add global aesthetics
  aes(x=factor(month), y = prop) +  #just use a bar chart
  geom_col() + # change the theme, to theme_light()
  theme_light() + 
  scale_y_continuous(labels = scales::percent) # format y-axis labels as, e.g., 15% and not 0.15

# What if we also wanted to look at the cancellations by airport and then plot them?
flights %>%
  mutate(cancelled = ifelse(is.na(dep_time), TRUE, FALSE)) %>% 
  group_by(origin, month, cancelled) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count)) %>% 
  filter(cancelled == TRUE) %>% 
  ggplot() +
  aes(x=factor(month), y = prop)+
  geom_col()+
  theme_light()+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~origin, ncol = 1)
  
  
  
  
  
  
  
  
  
  

