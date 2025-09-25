
##############################################
#### F24_MDSSB-MET-01-A_Data Science Tools in R
####
#### Session 3: Dates and time
####
#### Recommended Reading: R for Data Science
#### URL: https://r4ds.had.co.nz/
#### Chapter 11: Data Import
#### Chapter 16: Dates and Times
####
#### Armin Müller, Constructor University
##############################################



library(tidyverse)
library(nycflights13)




# 1. Creating dates and times

class(flights$time_hour)

head(flights$time_hour)
# Date & Time 
# Time-Zone

# Different options in R
library(readr) # basics
library(lubridate) # advanced
?lubridate

# Rule of thumb:
# always use the simplest data format that will do the job

# Note:
# Focus here is on dates and date-times
# R does not have a native format for storing times
# --> see hms package

# convenience functions:
today() # system date
now() # system time



# 1.1 Background: parsing

# Parsing, syntax analysis, or syntactic analysis 
# is the process of analyzing a string of symbols, 
# either in natural language, computer languages or data structures, 
# conforming to the rules of a formal grammar. 
# The term parsing comes from Latin pars (orationis), 
# meaning part (of speech).
# https://en.wikipedia.org/wiki/Parsing


# Parsing functions take a character vector ...
# (characters are the most general vectors, everything 
# can be represented as character vectors)
# ... and return a more specialized vector 
# (like logical, integer or date).

?parse_logical() # readr-package in Tidyverse
?as.logical() # functional equivalent

c("TRUE", "FALSE", "NA") %>% # create character vector with TRUE & FALSE
  parse_logical() %>% # Tidyverse
  #as.logical() %>% # base-R alternative
  str() # check output

c("1", "2", "3") %>% 
  parse_integer() %>% # Tidyverse
  #as.integer() %>% #base-R
  str()

# Not anything goes ...
c("TRUE", "FALSE", "NA") %>%
  #parse_date() %>% 
  as.Date()
  str()



# 1.2 Creating dates and times from strings / characters


# a) Parsing 
# Converting character-representations of date and time 
# into a standardized format

?parse_date() # readr package
# useful for data import

"2010-10-01 20:10:01" %>%
  parse_datetime() %>%
  str()

"2010-10-01" %>%
  parse_date() %>%
  str()

## but not:
"2010-10-01 20:10:01" %>%
  parse_date() %>%
  str()


"20:10:01" %>%
  parse_time() %>%
  str()

"01:10 am" %>%
  parse_time()

"01:10 pm" %>%
  parse_time()


## Also not:
"21 Jan 2001" %>%
  parse_date() %>%
  str()

# For this to work, we need to let R know about the format

# Year
# %Y (4 digits). 
# %y (2 digits); 00-69 -> 2000-2069, 70-99 -> 1970-1999. 

# Month
#  %m (2 digits). 
#  %b (abbreviated name, like “Jan”). 
#  %B (full name, “January”). 

# Day
# %d (2 digits). 
 

"21 Jan 2001" %>%
  parse_date("%d %b %Y") %>%
  str()

# careful how you parse, you can switch the order
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")


# adapt to local formats:
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
parse_date("1 mart 2015", "%d %B %Y", locale = locale("tr"))
?locale()
# It is less straightforward in other scenarios.


# Time
# %H 0-23 hour. 
# %I 0-12, must be used with %p. 
# %p AM/PM indicator. 
# %M minutes. 
# %S integer seconds. 
# %OS real seconds. 
# %Z Time zone (as name, e.g. America/Chicago). 
# %z (as offset from UTC, e.g. +0800). 

?parse_time()
parse_time("23:04 America/Chicago", "%H:%M %Z")
parse_time("11:04 pm +0700", "%H:%M %p %z")


# Non-digits
# %. skips one non-digit character. 
# %* skips any number of non-digits. 

parse_time("7 am Good Morning Vietnam +07:00", "%H %p %* %z")
# but: 
parse_time("7 am Good Morning Vietnam Asia/Hanoi", "%H %p %* %Z")
# we may need to get into strings to fix this ...


# Multiple functional equivalents for parsing and changing classes
?as_date() # Lubridate, rather basic
?as.Date() # base R, pretty flexible
?parse_date() # readr package; pretty flexible

# We already saw: 
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")



# Lubridate
as_date("01/02/15")
as_date("01/02/15", "%y/%m/%d") 
as_date("01/02/15", "%m/%d/%y") # specification does not work


# Base-R 
as.Date("01/02/15") # early starter 
as.Date("01/02/15", "%y/%m/%d")
as.Date("01/02/15", "%m/%d/%y") # specification does work!



# b) Lubridate package (chapter 16)
?ymd()
?hms()
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

ymd_hms("2017-01-31 20:11:59") # time zone UTC as default
mdy_hm("01/31/2017 08:01", tz = "GMT") # Greenwhich time


# but
hms("20:11:59")
hms("20:11:59", tz = "UTC") # error

# That was creating dates from strings.



# 1.3 creating dates from integers

# Base-R and readr functions do not do this very well

20101001 %>%
  parse_date() %>%
  class()

# this works (NOT)
20101001 %>%
  as_date()

# neither this
20101001 %>%
  as_date(tz = "UTC")



# It works with Lubridate
20170131 %>% 
  ymd(tz = "UTC")




# 1.4 Creating dates and times from individual components

# 1.4.1 simple integer formats
?flights
# Elements of the scheduled departure time
flights %>% 
  select(year, month, day, hour, minute)

# Remember: 
# Use the easiest data format that does the trick ...

# How to convert those?
?make_datetime()

make_datetime(year = 1999, month = 12, day = 22, sec = 10)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute, tz = "America/New_York"))



# 1.4.2 strange time formats
flights %>% 
  select(year, month, day, dep_time)
?flights

# How can we convert those to date-times? 
# Option 1: arithmetic operators
?flights
flights %>% 
  select(year, month, day, dep_time) %>% 
  mutate(dep_time2 = make_datetime(year, month, day, 
                                   (dep_time %/% 100),
                                   (dep_time %% 100),
                                    tz = "America/New_York"))
help("%/%")
?"%/%"
5 %/% 2 # indicates integer division. 
5 %% 2 # x mod y (“x modulo y”), i.e., computes the ‘remainder’
# It is guaranteed that x == (x %% y) + y * (x %/% y)



# Option 2: function
# Rule of thumb: If you repeat an operation 3 times or more,
# you can consider writing a function

#
# define the function as you learned in the lecture
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100, tz = "America/New_York")
}


# Let's make a subset of the data for calculations
# with only observations from October
flights_dt <- flights %>% 
  filter(month==10) %>%  # just flights from October for speed of calculation
  filter(!is.na(dep_time), !is.na(arr_time) &  # # no missing values for times
           !is.na(sched_dep_time) & !is.na(sched_arr_time) & # no missing values for schedule
           !is.na(arr_delay) & !is.na(dep_delay)) %>% # no missing values for delays
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time), # transform to date-time
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"), day)
flights_dt


# This is how we make date-times



# 2. Extracting dates and times

# There are various intuitive functions to extract meaningful components from date-times

# get the year
year(flights_dt$dep_time)

# get the month
month(flights_dt$dep_time) # as an integer (default)
month(flights_dt$dep_time, label = TRUE) # as abbreviated name
month(flights_dt$dep_time, label = TRUE, abbr = FALSE) # as full name

# but not:
month(flights_dt$dep_time, label = TRUE, abbr = FALSE, locale = locale("fr"))
# we probably have to adjust the factor levels or change the system locale ...

# Reconvert into a string
stringdate <- paste(month(flights_dt$dep_time, label = T, abbr = F), 
      day(flights_dt$dep_time), 
      year(flights_dt$dep_time)) 
stringdate
# Reconvert to date-time
parse_date(stringdate, "%B %d %Y")
as.Date(stringdate, "%B %d %Y")


# the day
day(flights_dt$dep_time) # of the month
wday(flights_dt$dep_time) # of the week, as integer (default)
wday(flights_dt$dep_time, label = TRUE) # as abbreviated name
wday(flights_dt$dep_time, label = TRUE, abbr = FALSE) # as full name


# the fiscal quarter or semester
quarter(flights_dt$dep_time)
semester(flights_dt$dep_time)

# hours and minutes
hour(flights_dt$dep_time)
minute(flights_dt$dep_time)





# 3. Plotting with date-times

# Now we can use the new date-times variable to plot.


# 3.1 Simple scatter plots

# Let's check departures arriving before scheduled time
flights_dt %>% 
  filter(sched_dep_time>dep_time) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = dep_time)) +
  geom_point() + 
  labs(x = "Scheduled departure time",
       y = "Actual departure time",
       title = "Departure delay consistency check")

# We still have the problem with delays stretching over midnight


# Let's check Departures on October 4
flights_dt %>% 
  filter(day==4) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = dep_time)) +
  geom_point() + 
  labs(x = "Scheduled departure time",
       y = "Actual departure time",
       title = "Departure delay consistency check")
# We will get back to it later



# 3.2 Histograms
# not really all that handy, best avoid them



# 3.3 Bar and column plots:
# Work well with extracted time components

# Example 1: total number of flights by weekdays in October
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

# Example 2: average delay by weekday
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarise(average_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = average_delay)) +
  geom_col() +
  labs(x = "Weekday in October",
       y = "Average arrival delay",
       title = "Average arrival delays by weekday")
# A little bit similar to Deutsche Bahn ...



# 3.4 Line plots

# Get the average arrival delay by hour 
# for every day of the month
flights_dt %>% 
  group_by(wday(dep_time, label = TRUE), hour(dep_time)) %>% 
  mutate(
    avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = dep_time, avg_delay)) +
  geom_line() +
  labs(x = "Day and hour",
       y = "Average arrival delay",
       title = "Average arrival delays in October")

# What are those spikes?
# Let's calculate delays just by hour of the day
flights_dt %>% 
  mutate(hour = hour(dep_time)) %>% 
  group_by(hour) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  ggplot(aes(hour, avg_delay)) +
  geom_line() +
  labs(x = "Hour",
       y = "Average arrival delay",
       title = "Average arrival delays in October")
# It looks like delays are rising in the evening and spiking after midnight






# 4. Time spans

# There are different ways to record time spans in base R and lubridate

# A - durations: represent an exact number of seconds.
# B - periods: represent human units like weeks and months.
# C - intervals: which represent a starting and ending point.



# 4.1 Difftime in base-R
# How old is Hadley?
h_age <- today() - ymd(19791014)
h_age
class(h_age)
# A difftime class object (base R) records a time span of seconds, minutes, hours, days, or weeks. 
# This ambiguity can make difftimes a little painful to work with. 



# 4.2 Lubridate durations
# have a fixed time-span recorded in seconds

#?as.duration()
h_age2 <- as.duration(h_age)
h_age2
class(h_age2)
typeof(h_age2)


# Durations always record the time span in seconds

# constructing durations
dseconds(15)
dminutes(15)
dhours(2)
ddays(3)
dmonths(1)
dyears(0.5)

# mathematical operations
## add and multiply durations
2 * dyears(0.5)
2 * dyears(0.5) == dyears(1)
dyears(1) == ddays(365) # 

dyears(1) + dweeks(12) + dhours(15)

# add to and subtract from days
tomorrow <- today() + ddays(1)
tomorrow
class(tomorrow) # Date

last_year <- today() - dyears(1)
last_year
class(last_year) # POSIXct

# careful with those operations
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + ddays(1) # different time, different time zone
# ... because of daylight savings time: March 12 has only 23 hours
# How to avoid this problem?


# 4.2 Periods
# human times like days and months
# with no fixed length in seconds

one_pm 
one_pm + days(1) # note the difference
one_pm
class(days(1))

# constructing periods
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)
# output in human units


# mathematical operations
## add and multiply
10 * (months(6) + days(1))
days(50) + hours(60) + minutes(2) # means different things in different contexts

days(50) + hours(160) + minutes(200)

## add them to dates: more reliable
# A leap year
ymd("2016-01-01") + dyears(1) # Duration --> Dec 31
ymd("2016-01-01") + years(1) # Period --> Jan 1

# Daylight Savings Time
one_pm + ddays(1) # Duration
one_pm + days(1) # Period


# 4.3 Intervals
# Solve another problem with durations:
dyears(1) / ddays(365) # Duration of a year divided by 365 days: something left beind the comma
years(1) / days(1) # same here: 2015 is shorter than 2016 for example

# Intervals = duration with starting point
# allow for more accurate measurement

# How many periods are in an interval?
next_year <- today() + years(1)
next_year
class(next_year)

(today() %--% next_year) / ddays(1)
# In lubridate, %--% is used to create an intervall between two dates
(as_date("2024-01-01") %--% as_date("2024-08-15"))

abs((today() - next_year)) / ddays(1)




# 5. Time Zones (just the essentials)

# a. they are complex in real life
# see also: 
# https://en.wikipedia.org/wiki/Time_zone

# b. Naming conventions
# Time zones have abbreviations:
## UTC: Temps universel coordonné (Coordinated Universal Time) 
## GMT: Greenwich Mean Time, 1 hour before UTC
## EST: Eastern Standard Time, 5 hours before UTC
## CEST: Central European Standard Time, 1 hour after UTC

# But these abbreviations may be ambiguous.
# For example, there is an EST zone in the US, Canada and Australia.

# The typical naming scheme in R is: 
# “<continent>/<city>”
ymd_hms("2023-02-06 12:00:00", tz = "Asia/Shanghai")
ymd_hms("2023-02-06 12:00:00", tz = "Europe/Berlin")
# some cities are missing though
ymd_hms("2023-02-06 12:00:00", tz = "Europe/Bremen")

# c. In R, the time zone is an attribute of the date-time that only controls printing. 
### For example, these three objects represent the same instant in time:

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x1 == x2
x1 == x3


# d. Unless otherwise specified, lubridate always uses UTC.

# e. Operations that combine date-times, like c(), will often drop the time zone.
x4 <- c(x1, x2, x3)
x4


# f. Changing the time zone

## change how the time zone is displayed
?with_tz()
x4a <- with_tz(x4, tzone = "Pacific/Auckland")
x4a
x4a == x4
x4a == c(x1, x2, x3)

## change the underlying instant in time
?force_tz()
x4b <- force_tz(x4, tzone = "Europe/Copenhagen")
x4b
x4b == x4




# 6. Exercise / Homework assignment:
# Recap:

# Operators:
# & = AND
# | = OR


library(tidyverse)
library(nycflights13)

# a. we turned the various time variables into date-time objects
# define the function as you learned in the lecture

?make_datetime()
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100, tz = "America/New_York")
}


# apply it to the dataset
flights_dt <- flights %>% 
  filter(month==10) %>%  # just flights from October for speed of calculation
  filter(!is.na(dep_time), !is.na(arr_time) &  # # no missing values for times
           !is.na(sched_dep_time) & !is.na(sched_arr_time) & # no missing values for schedule
           !is.na(arr_delay) & !is.na(dep_delay)) %>% # no missing values for delays
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time), # transform to date-time
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)) %>% 
  mutate( # calculate the delays by subtracting departure time from arrival time
         dep_delay2 = dep_time - sched_dep_time,
         arr_delay2 = arr_time - sched_arr_time,
         dep_delay2 = minutes(dep_delay2), # format them as minutes (periods)
         arr_delay2 = minutes(arr_delay2)) %>% 
  select(origin, dest, ends_with("delay"), ends_with("delay2"), ends_with("time"), day)

glimpse(flights_dt)

class(flights_dt$dep_delay)
class(flights_dt$dep_delay2)


# b. there are inconsistencies in the data

flights_dt %>% 
  filter(!is.na(dep_delay) & !is.na(sched_dep_time)) %>% 
  ggplot(mapping = aes(x = dep_delay, y = dep_delay2)) +
  geom_point() + 
  labs(x = "departure delay in minutes",
       y = "departure delay in seconds (calculated)",
       title = "Departure delay consistency check")

flights_dt %>% 
  filter(!is.na(arr_delay) & !is.na(sched_arr_time)) %>% 
  ggplot(mapping = aes(x = arr_delay, y = arr_delay2)) +
  geom_point() + 
  labs(x = "Arrival delay in minutes",
       y = "Arrival delay in seconds (calculated)",
       title = "Arrival delay consistency check")

# The dates came out wrong because we just added the components.
# Fix the issue first for departure and then for arrival delays.


# 6.1 Departure delays

# Problem: high negative departure delays
flights_dt %>% 
  select(dep_delay2, dep_time, sched_dep_time) %>% 
  filter(dep_time - sched_dep_time < -(60*60*10)) %>%  # Use "<" because we want negative delays!
  arrange(dep_delay2)
# 15 flights with less than -10 hours 

1:10
length(flights_dt$dep_time)
flights_dt$dep_time[15]


# 6.1.1 base-R: use a Loop

for (i in 1:length(flights_dt$dep_time)){
  print(paste("commencing observation ", i))
  # If departure in observation i is more than 10 hours before schedule
  if(flights_dt$dep_time[i]-flights_dt$sched_dep_time[i] <= -hours(10)){ # Use "<" because we want negative delays!
    # Then add 1 day to the departure time
    flights_dt$dep_time[i] <-  flights_dt$dep_time[i] + days(1)
    # and notify me of the correction
    print(paste("correcting dep_time observation ", i))
  }
}

# check the results
flights_dt %>% 
  filter(!is.na(dep_delay) & !is.na(sched_dep_time)) %>% 
  mutate(dep_delay2 = dep_time - sched_dep_time) %>% 
  ggplot(mapping = aes(x = dep_delay, y = dep_delay2)) +
  geom_point() + 
  labs(x = "departure delay in minutes",
       y = "departure delay in seconds (calculated)",
       title = "Departure delay consistency check")
# nice

# Pros and cons of Loops
## Pro: dual-use technology - Loops work in R AND Python
## Contra: Loops in R are slow!


# 6.1.2 Tidverse

## same as above
?if_else()
flights_dt <- flights_dt %>%
  mutate(dep_time = if_else(dep_time - sched_dep_time <= -hours(10), # Use "<" because we want negative delays!
                            dep_time + days(1), dep_time))
# very fast

# check the results
flights_dt %>% 
  filter(!is.na(dep_delay) & !is.na(sched_dep_time)) %>% 
  mutate(dep_delay2 = dep_time - sched_dep_time) %>% # result does not show without this line
  ggplot(mapping = aes(x = dep_delay, y = dep_delay2)) +
  geom_point() + 
  labs(x = "departure delay in minutes",
       y = "departure delay in seconds (calculated)",
       title = "Departure delay consistency check")




# 6.2 Arrival delays

# Exercise for you

# Step 1: plot the problem (as above)
flights_dt %>% 
  filter(!is.na(arr_delay) & !is.na(sched_arr_time)) %>% 
  ggplot(mapping = aes(x = arr_delay, y = arr_delay2)) +
  geom_point() + 
  labs(x = "Arrival delay in minutes",
       y = "Arrival delay in seconds (calculated)",
       title = "Arrival delay consistency check")

# Problem: high negative arrival delays
flights_dt %>% 
  select(arr_delay2, arr_time, sched_arr_time) %>% 
  filter(arr_time - sched_arr_time < -(60*60*10)) %>%  # Use "<" because we want negative delays!
  arrange(arr_delay2)

# and also: high positive arrival delays - many seem to have arrived early crossing midnight
flights_dt %>% 
  select(arr_delay2, arr_time, sched_arr_time) %>% 
  filter(arr_time - sched_arr_time > (60*60*10)) %>%  # Use "<" because we want negative delays!
  arrange(arr_delay2)



## same as above
?if_else()
flights_dt <- flights_dt %>%
  mutate(arr_time = if_else(arr_time - sched_arr_time <= -hours(10), # Use "<" because we want negative delays!
                            arr_time + days(1), arr_time), # add a day because they arrived after midnight
         arr_time = if_else(arr_time - sched_arr_time > hours(10), # Use ">" because we want positive delays!
                            arr_time - days(1), arr_time),) # subtract a day because they arrived before midnight

# Or: more convenient for multiple conditions:
?case_when()

flights_dt <- flights_dt %>%
  mutate(arr_time = case_when(arr_time - sched_arr_time <= -hours(8) ~ arr_time + days(1),
                              arr_time - sched_arr_time > hours(10) ~ arr_time - days(1),
                              .default = arr_time))




# check the results
flights_dt %>% 
  filter(!is.na(arr_delay) & !is.na(sched_arr_time)) %>% 
  mutate(arr_delay2 = arr_time - sched_arr_time) %>% # result does not show without this line
  ggplot(mapping = aes(x = arr_delay, y = arr_delay2)) +
  geom_point() + 
  labs(x = "arrival delay in minutes",
       y = "arrival delay in seconds (calculated)",
       title = "arrival delay consistency check")








# 7. Time Series Analysis in Tidyverse

# The Tidyquant package is now beyond the beta stage.
# It offers convenient and intuitive tools for downloading and analyzing stock data
#install.packages("tidyquant")
library(tidyquant)
# https://cran.r-project.org/web/packages/tidyquant/index.html

# Also, let's explore the use of themes some more
#install.packages("ggthemes")
library(ggthemes)


# 7.1 Get data for stocks:

?tq_get() # get stock data from the internet for VW
# ... From Yahoo Finance
VW <- tq_get("VOW3.DE", from = "2000-01-01", to = "2025-09-01")

head(VW)

glimpse(VW)

library(skimr)
skim(VW) # no missing data, how convenient!

# 7.2 Plot the data

## barebones plot
VW %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line()
  

## let's try a theme
?theme_set()
theme_set(ggthemes::theme_economist()) # The Economist style

# Let's adjust the scales 
VW %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  scale_y_continuous(# set unit to €
    labels = scales::label_dollar( # for USD, you can just leave the function empty
      prefix = "",
      suffix = "\u20ac",
      big.mark = ".", # adjust to European decimal marks
      decimal.mark = ","), 
    position = "right"
  ) +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Stock Closing Price",
      caption = "Data from Yahoo Finance",
      x = "Date",
      y = "Closing value")
# looks nicer already



# 7.3 Aggregation


# Stock data is highly volatile, 
# so we may want to calculate a moving average, aggregate the values etc.
# There are various tidyquant functions that can help us ...

?tq_transmute()
?tq_mutate()
?tq_mutate_fun_options()

?to.period() # Tidyquant can call functions from other packages for Time Series analysis

VW %>% 
  tq_transmute(select = close, # select the variable
               mutate_fun = to.period, # select the period
               period = "years",
               col_rename = "Yearly_Avg" # name the new variable
  ) %>% 
  ggplot(aes(x = date, y = Yearly_Avg)) +
  geom_line() +
  scale_y_continuous(
    # set unit to €
    labels = scales::label_dollar(
      prefix = "", suffix = "\u20ac", big.mark = ".",
      decimal.mark = ","
    ), 
    position = "right" # put it on the right side
  ) +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Stock Closing Price",
       caption = "Data from Yahoo Finance",
       x = "Date",
       y = "Closing value (yearly average)")


VW %>% 
  tq_transmute(select = close, # select the variable
               mutate_fun = to.period, # select the period
               period = "quarters",
               col_rename = "Quarterly_Avg" # name the new variable
  ) %>% 
  ggplot(aes(x = date, y = Quarterly_Avg)) +
  geom_line() +
  scale_y_continuous(
    # set unit to €
    labels = scales::label_dollar(
      prefix = "", suffix = "\u20ac", big.mark = ".",
      decimal.mark = ","
    ), 
    position = "right" # put it on the right side
  ) +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Stock Closing Price",
       caption = "Data from Yahoo Finance",
       x = "Date",
       y = "Closing value (quarterly average)")


VW %>% 
  tq_transmute(select = close, # select the variable
               mutate_fun = to.period, # select the period
               period = "months",
               col_rename = "Monthly_Avg" # name the new variable
  ) %>% 
  ggplot(aes(x = date, y = Monthly_Avg)) +
  geom_line() +
  scale_y_continuous(
    # set unit to €
    labels = scales::label_dollar(
      prefix = "", suffix = "\u20ac", big.mark = ".",
      decimal.mark = ","
    ), 
    position = "right" # put it on the right side
  ) +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Stock Closing Price",
       caption = "Data from Yahoo Finance",
       x = "Date",
       y = "Closing value (monthly average)")



# 7.4 Missing data

# Let's assume some data is missing
# This is an example for demonstration.
# Normally, if data is missing, it should be just occasionally and at random.

# Before missingness
VW %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()

# add missing values
VW$adjusted[3000:4000] = NA

# After missingness
VW %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()

# We can perform a backward fill to replace missing values with the next observed value

VW %>% 
  fill(adjusted, .direction = "up") %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()


# A forward fill replaces missing values with the last observed value
VW %>% 
  fill(adjusted, .direction = "down") %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()



## 7.5 Time shifts

?lag()
?lead()

# Let's create and plot variables that lead and lag the rate by 252 days each,
# which approximates a business year, given that we have daily data.
VW %>% 
  mutate(close_lead_252 = lead(close, n = 252),
         close_lag_252 = lag(close, n = 252)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = close)) +
  geom_line(aes(x = date, y = close_lead_252), color = "red") +
  geom_line(aes(x = date, y = close_lag_252), color = "blue")
  

# We can aggregate on a monthly basis (which more precisely matches the year)

my("Jan 2017")


VW %>% 
  group_by(year(date), month(date, label = TRUE)) %>%  # group the data by year and month
  summarize(mean_closing_value = mean(close, na.rm = TRUE)) %>%  # get the average closing value
  ungroup() %>%  # dissolve the grouping
  mutate(date = my(paste(`month(date, label = TRUE)`, `year(date)`))) %>% # recreate a data (my(): month, year)
  mutate(one_year_lead = lead(mean_closing_value, n = 12),
         one_year_lag = lag(mean_closing_value, n = 12)) %>%
  pivot_longer(c("one_year_lead", "one_year_lag", "mean_closing_value"),
               names_to = "Time_series", values_to = "closing_value") %>%
  arrange(date) %>% # sort by date, just to be sure the data is in good order
  ggplot() +
  geom_line(aes(x = date, y = closing_value, color = Time_series)) +
  scale_y_continuous( # set to €
    labels = scales::label_dollar(prefix = "", suffix = "\u20ac", big.mark = ".",
      decimal.mark = ","), position = "right") +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Stock Closing Price",
       caption = "Data from Yahoo Finance",
       x = "Date",
       y = "Closing value (monthly average)")
  
# Not perfect, but it does the trick for now.


## 7.6 Return of Investment

# Let's calculate the monthly and yearly return on investment.
# Formula is: (Investment T2 - Investment T1)/Investment T1

VW %>% 
  tq_transmute(select = close, # select the variable
               mutate_fun = periodReturn, # select the period
               period = "monthly",
               col_rename = "monthly_returns" # name the new variable
  ) %>% 
  ggplot(aes(x = date, y = monthly_returns)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(), 
                     position = "right") +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Return of Investment (ROI)",
       x = "Date",
       y = "Monthly return of investment")

  labs(title = "Volkswagen Monthly ROI")


VW %>% 
  tq_transmute(select = close, # select the variable
               mutate_fun = periodReturn, # select the period
               period = "yearly",
               col_rename = "yearly_returns" # name the new variable
  ) %>% 
  ggplot(aes(x = date, y = yearly_returns)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(), 
                     position = "right") +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Return of Investment (ROI)",
       x = "Date",
       y = "Yearly return of investment")



# 7.7 Moving averages

# you can plot different types of moving averages directly ...
?geom_ma()

# ... or calculate them first:
?SMA()

# Let's try simple moving averages (SMA) over 30 days 

## quick-and-dirty: with geom_ma()
VW %>% 
  ggplot(aes(x = date, y = close)) +
  geom_ma(ma_fun = SMA, n = 30, size = 1.2, color = "red") +
  scale_y_continuous(
    labels = scales::label_dollar(),
    position = "right"
  ) +
  labs(title = "Volkswagen Stock Price (monthly moving average)")


# create the variable before plotting (recommended)
VW %>% 
  mutate(SMA_30 = SMA(close, n = 30)) %>% 
  ggplot(aes(x = date, y = SMA_30)) +
  geom_line() +
  scale_y_continuous( # set to €
    labels = scales::label_dollar(prefix = "", suffix = "\u20ac", big.mark = ".",
                                  decimal.mark = ","), position = "right") +
  scale_x_date(
    date_breaks = "2 years", # Set major breaks to every second year
    date_labels = "%Y"       # Format labels to show only the year
  ) +
  labs(title = "Volkswagen Stock Closing Value",
       caption = "Data from Yahoo Finance",
       x = "Date",
       y = "Closing value (30-day moving average)")
  labs(title = )

  
