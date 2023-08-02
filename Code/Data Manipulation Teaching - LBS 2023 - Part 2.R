# Load the necessary packages 
library(tidyverse)
library(nycflights13)
library(skimr)
library(GGally)

data(flights)
data(planes)
data(weather)
data(airlines)

### Data Manipulation:
## Problem 1 - Use logical operators to find flights that:

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

## Problem 2: What months had the highest and lowest proportion of cancelled flights?
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


## Problem 3: What plane (specified by the tailnum variable) traveled the most times from New York City airports in 2013?
# Will need to join flights with planes first

glimpse(flights)
glimpse(planes)

flights %>% 
  left_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)
# Tailnum N725MQ has travelled the most times from NY airports in 2013


## Problem 4 For the plane with the greatest number of flights and that had more than 50 seats, create a table where it flew to during 2013.
which_plane <- flights %>% 
  left_join(planes, by="tailnum") %>% 
  filter(seats >= 50) %>% 
  count(tailnum, sort = TRUE) %>% 
  select(tailnum) %>% 
  slice(1) %>% # slice(1) will pull the first, highest number of flights
 pull() # pull() creates a vector; if we dont use pull() we get a dataframe

which_plane
# The plane with the greatest number of flights had tailnum N328AA

flights %>% 
  filter(tailnum == which_plane) %>% 
  count(origin, dest, sort=TRUE)
# The result is a table of N328AA destinations and the number of times it flew there


## Problem 5 What is the distribution of temperature (temp) in July 2013? 
# Identify any important outliers in terms of the wind_speed
weather %>% 
  filter(month == 7) %>% 
  ggplot() +
  aes(x= temp)+
  geom_histogram()+
  labs(x = "Temperature (°F)", y = "Count", title = "Temperature Distribution - July 2013")+
  theme(plot.title = element_text(hjust = 0.5))
  
# looking at all months
weather %>% 
  # filter(month == 7) %>% 
  ggplot() +
  aes(x= temp)+
  geom_histogram() +
  facet_wrap(~month, nrow=4)+
  theme_light() +
  labs(x = "Temperature (°F)", y = "Count", title = "Temperature Distribution - 2013")+
  theme(plot.title = element_text(hjust = 0.5))
  
# facet_wrap() for all months' wind_speed
weather %>% 
  ggplot() +
  aes(x= wind_speed)+  
  geom_histogram()+
  facet_wrap(~month, 
             ncol = 3, # to get 3 columns
             scales = "free")+ # easy to check outliers, by having ggplot automatically adjust scales on axes
  theme_light()+
  theme(text=element_text(size=12))+
  labs(x = "Wind Speed", y = "Count", title = "Wind Speed Distributions - 2013")+
  theme(plot.title = element_text(hjust = 0.5))
  

## Problem 6 What is the relationship between dewp and humid?
## Problem 6 What is the relationship between precip and visib?
weather %>% 
  select(dewp, humid, precip, visib, temp) %>% 
  ggpairs()+
  theme_bw()+
  labs(x = NULL, y = NULL, title = "Correlation Matrix - Weather 2013")+
  theme(plot.title = element_text(hjust = 0.5))


## Problem 7 How many planes have a missing date of manufacture?
planes %>% 
  filter(is.na(year)) # to get the dataframe

planes %>% # to get the counts
  count(is.na(year))


## Problem 8 what are the five most common manufacturers?
planes %>% 
  count(manufacturer, sort = TRUE)

# Re-code the manufacturer name and collapse rare vendors into a category called "Other"
planes_2 <- planes %>% 
  mutate(recode_manufacturer = case_when(
    manufacturer %in% c("BOEING") ~ "Boeing",
    manufacturer %in% c("AIRBUS INDUSTRIE", "AIRBUS") ~ "Airbus",
    manufacturer %in% c("EMBRAER") ~ "Embraer",
    manufacturer %in% c("MCDONNELL DOUGLAS", "MCDONNELL DOUGLAS AIRCRAFT CO", "MCDONNELL DOUGLAS CORPORATION" ) ~ "McDonnell Douglas",
    TRUE ~ "Other")) # everything else will be "Other"

planes_2 %>% 
  count(recode_manufacturer, sort = TRUE)


## Problem 9 Has the distribution of manufacturer changed over time as reflected by the airplanes flying from NYC in 2013?
flights %>% 
  left_join(planes_2, by = "tailnum") %>% 
  filter(!is.na(recode_manufacturer)) %>% 
  group_by(month, recode_manufacturer) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot()+
  aes(x=factor(month), 
      y = prop, 
      group = recode_manufacturer, 
      colour = recode_manufacturer)+
  geom_line()+
  theme_light()+
  theme(text=element_text(size=12))+
  scale_y_continuous(labels = scales::percent)+
  NULL+
  labs(x = "Month", y = "Proportion", title = "No major change in manufacturer distribution over 2013", color = "Manufacturer:")+
  theme(plot.title = element_text(hjust = 0.5))


## Problem 10 What is the oldest plane (specified by tailnum) that flew from NYC in 2013?
flights %>% 
  left_join(planes, by = "tailnum") %>%
  rename(year_manufactured = year.y) %>% 
  select(tailnum, year_manufactured) %>% 
  distinct(tailnum, .keep_all = TRUE) %>% 
  arrange(year_manufactured)


## Problem 11 How many airplanes that flew from New York City are included in the planes table?
flights %>% 
  distinct(tailnum) %>%
  semi_join(planes) # semi_join() return all rows from x with a match in y

# Another way
planes %>% 
  semi_join(flights, by="tailnum")


## Problem 12 What is the median arrival delay on a month-by-airport basis?
flights %>% 
  group_by(month, origin) %>% 
  summarise(mean_arrival_delay = mean(arr_delay, na.rm=TRUE),
            median_arrival_delay = median(arr_delay, na.rm=TRUE))


## Problem 13 Plot the median arrival delay foe each month and origin airport
flights %>% 
  group_by(carrier, month, origin) %>% 
  summarise(mean_arrival_delay = mean(arr_delay, na.rm=TRUE),
            median_arrival_delay = median(arr_delay, na.rm=TRUE)) %>% 
  left_join(airlines, by = "carrier") %>% 
  rename(carrier_name = name) %>% 
  ggplot()+
  aes(x= factor(month),
      y = median_arrival_delay,
      colour = origin, 
      group = origin) +
  geom_line()+
  facet_wrap(~carrier_name, scales = 'free', ncol=3)+
  theme_light() +
  theme(
    text=element_text(size=10))+
  labs(x = "Month", y = "Median Arrival Delay", colour = "Origin Airport")
