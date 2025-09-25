# 0.1 Recap: Homework assignment

# Background: calculating airplane departure delays gives us strange results
flights %>% 
  select(year:day, # the variables year, month and day
         ends_with("delay"),  # all variables ending with delay
         ends_with("time"),
         time_hour) %>% 
  mutate(dep_delay2 = dep_time - sched_dep_time) %>% 
  select(dep_delay2, dep_time, sched_dep_time, time_hour) %>% 
  arrange(dep_delay2)
# Some flights have a delay stretching over midnight


# Let's visualize the problem:
flights %>% 
  sample_n(10000) %>% # get a sample of 10000 observations
  select(year:day,  # select the meaningful variables
         ends_with("delay"),
         ends_with("time"),
         time_hour) %>% 
  mutate(dep_delay2 = dep_time - sched_dep_time) %>%
  filter(!is.na(dep_delay) & !is.na(dep_delay2)) %>% 
  ggplot(mapping = aes(x = dep_delay, y = dep_delay2)) +
  geom_point() + 
  labs(x = "departure delay in minutes",
       y = "departure delay in HHMM format (calculated)",
       title = "Departure delay consistency check")
# Looks strange


# Exercise 2a: Try to fix the problem
# We want a subset of data without such distortions to plot

## Departure delay
flights %>% 
  # First: select the variables
  select(year:day,  
         ends_with("delay"),
         ends_with("time"),
         time_hour) %>% 
  # Second: create new variables
  mutate(dep_delay2 = dep_time - sched_dep_time) %>%
  # Third: exclude NAs
  filter(!is.na(dep_delay) & !is.na(dep_delay2)) %>%
  # Fourth: exclude delays stretching over midnight
  filter(dep_time > sched_dep_time) %>% # Option 1
  #filter(sched_dep_time < 2200 & dep_time > 600) %>% # Option 2, not so good
  # Fifth: draw a random sample
  sample_n(10000) %>% 
  # Sixth: create a scatter plot
  ggplot(mapping = aes(x = dep_delay, y = dep_delay2)) +
  geom_point() + 
  labs(y = "departure delay in HHMM format (calculated)",
       x = "departure delay in minutes",
       title = "Departure delay consistency check")


## Homework assignment
# Make a similar plot for arrival delay


# Arrival data: 

# Arrival data: 
# Same issue with 
flights %>% 
  select(year:day, # the variables year, month and day
         ends_with("delay"),  # all variables ending with delay
         ends_with("time"),
         time_hour) %>% 
  mutate(arr_delay2 = arr_time - sched_arr_time) %>% 
  select(arr_delay2, arr_time, sched_arr_time, time_hour) %>% 
  arrange(arr_delay2) # big delays stretching over midnight


# Apply the same solution

flights %>% 
  # First: select the variables
  select(year:day,  
         ends_with("delay"),
         ends_with("time"),
         time_hour) %>% 
  # Second: create new variables
  mutate(arr_delay2 = arr_time - sched_arr_time) %>%
  # Third: exclude NAs
  filter(!is.na(arr_delay) & !is.na(arr_delay2)) %>%
  # Fourth: exclude delays stretching over midnight
  filter(arr_time > sched_arr_time) %>% # only flights arriving after nominal departure
  # Fifth: draw a random sample
  sample_n(10000) %>% 
  # Sixth: create a scatter plot
  ggplot(mapping = aes(x = arr_delay, y = arr_delay2)) +
  geom_point() + 
  labs(y = "arrival delay in HHMM format  (calculated)",
       x = "arrival delay in minutes",
       title = "Arrival delay consistency check")


# There is yet another problem:
# Negative delay in minutes, almost 1 day in HHMM format

flights %>% 
  select(year:day, # the variables year, month and day
         ends_with("delay"),  # all variables ending with delay
         ends_with("time"),
         time_hour) %>% 
  mutate(arr_delay2 = arr_time - sched_arr_time) %>% 
  select(arr_delay2, arr_time, sched_arr_time, time_hour) %>% 
  arrange(desc(arr_delay2)) # smallest calculated delays
# Those flights arrived early, before midnight

flights %>% 
  # First: select the variables
  select(year:day,  
         ends_with("delay"),
         ends_with("time"),
         time_hour) %>% 
  # Second: create new variables
  mutate(arr_delay2 = arr_time - sched_arr_time) %>%
  # Third: exclude NAs
  filter(!is.na(arr_delay) & !is.na(arr_delay2)) %>%
  # Fourth: exclude delays stretching over midnight
  filter(arr_time > sched_arr_time
           & arr_time < 2300 # exclude flights arriving after 23:00
           & sched_arr_time > 100
         ) %>%  # exclude flights scheduled to arrive before 1:00
  # Fifth: draw a random sample
  sample_n(10000) %>% 
  # Sixth: create a scatter plot
  ggplot(mapping = aes(x = arr_delay, y = arr_delay2)) +
  geom_point() + 
  labs(y = "arrival delay in HHMM format (calculated)",
       x = "arrival delay in minutes",
       title = "Arrival delay consistency check")

# This is quick-and-dirty and not very sophisticated.
# We can do this more elegantly when we systematically engage with dates and times



