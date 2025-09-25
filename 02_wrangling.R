
##############################################
#### F24_MDSSB-MET-01-A_Data Science Tools in R
####
#### Session 3: Data Structures and Wrangling 
#### ... in base-R and Tidyverse
####
#### Recommended Reading: R for Data Science
#### URL: https://r4ds.hadley.nz/
#### Chapter 3: Data Transformation
#### Chapter 5: Data Tidying
#### Chapter 7: Data Import
#### Chapter 12: Tidy data
#### Chapter 13: Relational Data
#### Chapter 19: Joins
####
#### Armin Müller, Constructor University
##############################################


# In this session we are working in a actual R file
# All text that is not code to be executed should be commented out with "#".

# Today we will do some heavy lifting.
# 1. Data import
# 2. Basic data structures
# 3. Data transformation
# 4. Pivoting
# 5. Relational data



# 1. Data import

# 1.1 install and load the packages
# install tidyverse if necessary
# install.packages("tidyverse") 
# install.packages("nycflights13") 
## execute the lines by pressing "control" + "return" (after uncommenting)
## you might need to confirm a few steps in the console
## this may take a while


# load single package
library(tidyverse)
library(nycflights13)


# 1.2 load the data from the environment

# load the data into the environment
flights <- flights

# get the information about the data set
?flights


# 1.3 Working directories

# NOTE: DO NOT USE WORKING DIRECTORIES WHEN YOU WORK WITH GIT PROJECTS!!!!!
# IN THIS COURSE, YOU WILL NOT NEED THEM, BUT THEY MAY BE USEFUL IN OTHER SETTINGS.

# ALSO: BEWARE NOT TO SAVE DATAFILES IN YOUR GIT REPOSITORIES, THOSE ARE FOR CODEFILES AND REPORTS!

# usually we work with a working directory
getwd() # check your working directory
# For this course, it is typically the project folder with the Git repository
# This is selected automatically when the project is opened
# in other scenarios, it may be another project folder on your harddrive

# you can set it in 2 steps:
# a. get the path by selecting a file in the project folder
#file.choose()
# in the window that opens, navigate to the folder & select a random file

# b. copy the path without the file name from the spelling convention
# this way, you can be sure to have the right one
WIN_PATH <- "D:\\Git_Repositories\\DST02023\\hw-01\\"
MAC_PATH <- "/Users/arminmuller/Github_repositories/R"


# c. use the path to set the working directory
setwd(MAC_PATH)
setwd("/Users/arminmuller/Github_repositories/R")



# 1.4 save and load datasets
# (chapter 11)

# Standard choice: .csv (comma-separated values) files

# let's save the flights dataset
write_csv(flights, "/Users/arminmuller/Github_repositories/flights.csv")

# see the file appear in your project folder??
# DO NOT PUSH DATA TO GITHUB REPOSITORIES !!!!!!!

# Let's import it again
flights2 <- read_csv("flights.csv")

#file.choose()
flights3 <- read_csv("/Users/arminmuller/Github_repositories/R/flights.csv")


# reliable alternative: .rds (R data set)
write_rds(flights, "flights.rds")
# check the difference in size

# and re-import
flights4 <- read_rds("flights.rds")




# These are just the very basics
# Be sure to read the documentation of the functions 
# once you get into importing your own data.
# It is not trivial!!!

# Also remember that by default, these functions operate in the working directory.
# For other directories, you may need to specify a PATH.





?flights

# 2. Basic data structures (fundamentals - see chapter 12 & 13)

# (This part contains lots of base-R)

# 2.1 Atomic Vectors
# 2.1.1 integer and double vectors
print(1:10) # : creates a sequence of integers
typeof(1:10)
typeof(flights$arr_time)
class(flights$arr_time)
attributes(flights$arr_time) # just for the record: no attributes

typeof(1)
typeof(1L)
typeof(flights$air_time)
class(flights$air_time)

# you can also use str() to get basic information about an object
?str()
str(1)
str(1:10)
str(flights$arr_time)
str(flights$air_time)

## Difference between integers and double vectors:
## doubles are approximations
## 4 special values: NA, -Inf, Nan, Inf
?c()
c(-1, 0, 1, NA) / 0

str(c(-1, 0, 1, NA) / 0)

1/(-0)

## Integers are precise values
## one special value: NA (missing value, also in other vectors)




# 2.1.2 Logical vectors

# Logical vectors can have 2 values:
# TRUE and FALSE (or NA)

# They can be used as basic data (yes/no questions),
# or to specify conditions in functions.

## ... as restuls of mathematical equations
3 == 3 # == means equals (do not apply to double vectors)
3 != 4 # != does not equal (do not apply to double vectors)

3 < 4 #
3 > 5

3 * 3 == 6
6 + 3 == 9
2^3 == 8

1:10 - 3 == 0

str(1:10 - 3)
str(1:10 - 3==0)
?as.numeric()
str(as.numeric(1:10 - 3==0)) # conversion of logical to numerical class

## ... as a result of logical statements
TRUE == TRUE
TRUE != FALSE
TRUE > FALSE # How so?
as.numeric(c(TRUE, TRUE, FALSE, TRUE)) # c() creates a vector
?sum()
sum(c(TRUE, TRUE, FALSE, TRUE)) # conversion: TRUE = 1, FALSE = 0
FALSE > TRUE





# 2.1.3 character vectors & factors (very briefly)
letters <- c("a", "b", NA , "d", "e", NA, "g")
print(letters)
typeof(letters)
typeof(flights$carrier)
class(flights$carrier)
str(flights$carrier)

# Characters are rather complex data
# Text is complex data
# Usually, it's a good choice to set the standard text encoding to UTF-8
# --> Tools / Global Options / Code / Saving --> Default text encoding

greet <- c("Hello World!", "世界你好！","مرحبا بالعالم!", NA) 
typeof(greet)
print(greet)

# Character vectors / Strings are an extensive and complex subject.
# (see chapter 14)

# For simple tasks, we often use factors instead
table(flights$carrier)
typeof(as.factor(flights$carrier))
class(as.factor(flights$carrier))
attributes(as.factor(flights$carrier))
# Factors connect integer values to levels
# They help us store and use simple categorical variables effectively





# 2.1.4 Indexing
# How can you access single elements of a vector?

# create a sequence from 1 to 21 in steps of 2:
vector <- seq(1, 21, by = 2)
vector

# you can access the different elements by order
vector[1] # first value
vector[4] # fifth value
vector[7] # seventh value

# you can also access the elements by logical arguments
vector[vector>5] # all above 5
vector[vector==11] # all equal to 11
vector[vector!=11] # all not equal to 11

vector[vector/2>5]

# Logical vector:
vector_log <- c(T, F, T, T, T, F, NA)
class(vector_log)
vector_log

vector_log[is.na(vector_log)]
is.na(vector_log)
sum(is.na(vector_log))

vector_log[vector_log==T]
vector_log[vector_log!=F]

vector_log[vector_log==F]
vector_log[vector_log!=T]


# character vector
greet[greet=="Hello World!"]
greet[greet!="Hello World!"]
# you can do way more sophisticated things with regular expressions.
# if you are interested, check chapter 14



# 2.1.5 Missing and special values

# remember this one?
special_values <- c(-1, 0, 1, NA) / 0
# you can either use a stored vector or calculate directly in the functions



## 2.1.5.1 How to check for special values in single vectors?
is.na(special_values) # use the stored vector
is.finite(special_values)
is.infinite(c(-1, 0, 1, NA) / 0) # calculate in the function
is.nan(c(-1, 0, 1, NA) / 0)

str(is.nan(c(-1, 0, 1, NA) / 0))
## and
sum(is.na(c(-1, 0, 1, NA) / 0))
sum(is.nan(special_values))


## Example: 
# count missing values are there in a variable?
summary(is.na(flights$dep_delay))
## or 
sum(is.na(flights$dep_delay))
### PS:
sum(flights$dep_delay) # sum() cannot calculate the sum when NAs are present
### ... and 
sum(flights$dep_delay, na.rm = TRUE) # unless you specify na.rm = TRUE
?sum()

# FYI:
mean(flights$dep_delay, na.rm = TRUE)
median(flights$dep_delay, na.rm = TRUE)


### how many observations are there?
length(flights$dep_delay)

### what is the share of NAs in the departure delay variable?
sum(is.na(flights$dep_delay))/length(flights$dep_delay)



## 2.1.5.2 entire datasets for special values

# it's straightforward with is.na()
summary(is.na(flights))

summary(is.infinite(flights)) # does not work because
## a. dataset is saved as a list
## b. is.infinite() function does not accept a list as input

# for nan or infinite values, you need apply functions in base-R
?lapply() # returns a list

lapply(flights, function(x) summary(is.infinite(x)))
lapply(flights, function(x) summary(is.nan(x)))
typeof(lapply(flights, function(x) summary(is.nan(x))))
class(lapply(flights, function(x) summary(is.nan(x))))

# output: array / matrix
sapply(flights, function(x) summary(is.infinite(x)))

# in Tidyverse, you can use the map() function for the same purpose
?map()
map(flights, function(x) summary(is.infinite(x)))
typeof(map(flights, function(x) summary(is.infinite(x))))
class(map(flights, function(x) summary(is.infinite(x))))
# it returns a list


# How do we apply this with a pipe operator?
## Background: modular organization of code
## Tidyverse
flights %>% # Tidyverse pipe operator
  map(function(x) summary(is.infinite(x)))

# base-R
flights |> # base-R pipe operator
  lapply(function(x) summary(is.infinite(x)))


# lapply() and map() are very powerful multi-purpose tools
# use them carefully!
# the syntax takes some getting used to.


# Exercise 1:
# how can you get the minimum, maximum, median and mean of the variables
# using either lapply() or map()?

?min()
?max()
?median()
# ...

flights %>% 
  sapply(function(x) min(x, na.rm = TRUE))
# no error for character vectors


flights %>% 
  sapply(function(x) max(x, na.rm = TRUE))
# no error for character vectors

flights %>% 
  lapply(function(x) median(x, na.rm = TRUE))
# error for character values
#median(as.factor(flights$carrier), na.rm = TRUE)
glimpse(flights)

flights %>% 
  sapply(function(x) mean(x, na.rm = TRUE))
# error for character values

## simplest approach:
flights %>% 
  lapply(function(x) summary(x))




# 2.2 Lists (recursive vectors)

# Is an object class and a vector type
# more complex than atomic vectors
# - lists can contain other lists
# - lists can contain elements of different type


x <- list(1, 2, 3) # list with 3 numeric vectors of length 1
x

# compare to a regular numeric vector of length 3
c(1,2,3) 
# note the double brackets in list output
# those are important!!

# str() shows you the structure of lists and more complex objects
str(x)
attributes(x) # no attributes

# you can name the elements of a list
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)
attributes(x_named) # named lists have the attribute "names"


# lists can contain a mix of objects
y <- list("a", 1L, 1.5, TRUE)
str(y)

# lists can contain other lists
z <- list(list(1, 2), list(3, 4))
str(z)



## 2.2.1 Where do you encounter lists?

# both vector-type and object class:
# output of map() for example
typeof(map(flights, function(x) summary(is.infinite(x))))
class(map(flights, function(x) summary(is.infinite(x))))


### as a vector-type
# data frames:
typeof(flights)
class(flights)

### statistical models
linear_model <- lm(arr_delay ~ dep_delay, data = flights)
typeof(linear_model)
class(linear_model)
# see below



## 2.2.2 Why does it matter?
# Indexing for example
# How to access elements in a list?

## by number with [[]]
## named lists: by attribute names with $

x[[2]] # access vector number 2
x_named[[2]] # access vector number 2
x_named$b # access the vector named b
attributes(x_named)


flights[[3]] # access third column in data set
flights$day # access variable named day
attributes(flights)

str(linear_model)
summary(linear_model)
linear_model[[1]] # access first element of model output
linear_model$coefficients # access coefficients in your model
attributes(linear_model)
# important: some packages or functions may not have good summaries



## 2.2.3 unlist() can turn lists into atomic vectors (often character vectors)

# Data Frame: not so useful
flights %>% 
  unlist() %>% 
  typeof()
  #length()

# linear model: also not so useful
linear_model %>% 
  unlist() %>% 
  #length()
  typeof()

# More useful scenario:
# store the maxima of all variables in a vector
maxima <- flights %>% 
  lapply(function(x) max(x, na.rm = TRUE)) %>% 
  unlist()
maxima 
typeof(maxima)





# 2.3 Data Frames (base-R) and Tibbles (Tidyverse)

# the base-R data frame
df <- data.frame(x = 1:5, y = 5:1)
typeof(df) # storage
class(df) # object class: data frame
attributes(df) # more information

## example: output
as.data.frame(flights)


# the Tidyverse tibble (functionally enhanced data frame)
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb) 
class(tb) # object class: data frame and tibble
attributes(tb) 

## example: output
as.data.frame(flights) # or
tibble(flights)
attributes(flights)

# Distinctions:
# Tibbles are functionally enhanced data frames
# Data frames and tibbles contain vectors of equal length (but different type)
# Lists may contain vectors of differnet type and different length




## Indexing: as for lists

## a. by variable name: (most important for now)
flights$dep_time

# is the same as

flights %>% 
  .$dep_time

## b. by element number (useful to know)

flights[[4]]

# is equivalent to ...
flights %>% 
  .[[4]]

# You can also access individual elements, like the first element of the 4th column
flights %>%
  .$dep_time %>%
  .[1]

flights[[4]][1] # first element of the first vector
## equals
flights %>% 
  .[[4]] %>% 
  .[1]


## Finally, you can also access values in a tibble by coordinates
## here, we use single brackets and matrix notation

flights[1,4] # first line, fourth column
## equals
flights %>% 
  .[1,4]

# or just the first line
flights[1,]

# or the fourth column
flights[,4]
flights[[4]]


# The function select() and filter() are
# similar but enhanced versions of indexing
# (i.e.: selecting rows and columns).
# They help us subset the data (we will not do that in base-R for now).

# Congrats, we're about 50% through.



# 3. Data Transformation

# We are now switching from base-R syntax to Tidyverse syntax
# Tidyverse emphasizes modular organization of lines over nested code
# This makes trouble-shooting easier when code gets more complex

# There are some fundamental functions for wrangling in Tidyverse
## filter() - select observations (functional equivalent to indexing)
## select() - select variables (functional equivalent to indexing)
## arrange() - order observations
## mutate() - create a new variable


## they help us cut and tailor our data so that we can make nice plots and calculations.



# 3.1 filter(): select specific observations

# get all flights on January 1
## in classic base-R syntax
filter(flights, month == 1, day == 1)
## in Tidyverse syntax
flights |> 
  filter(month==1, day==1)

# save the results:
## base-R syntax
jan1 <- filter(flights, month == 1, day == 1)
jan1
class(jan1)

## Tidyverse syntax
jan1 <- flights %>% 
  filter(month==1, day==1)
jan1



# Non-metric variables:

# Character / Factor
## all flights to Hawai (Honolulu airport)
flights %>% 
  filter(dest == "HNL")


# Logical conditions
## all flights with scheduled arrival time lower than scheduled departure time
flights %>% 
  filter(sched_arr_time < sched_dep_time) # Logical, filter selects TRUE
# Times and dates are tricky
# This is why there are special object classes to handle them
# We will look at those later



# 3.2  reorder rows: arrange()

## order rows by the date in ascending order
flights %>% 
  arrange(year, month, day)

## try with a time variable
flights %>% 
  arrange(time_hour)

#  order rows by the date in decending order
?desc()
flights %>% 
  arrange(desc(dep_delay))

# convert biggest delay to hours
1301/60



# 3.3 variables by name: select()

# let's just select the year, month and date variables
flights %>% 
  select(year, month, day)

# since they are next to each other, we can also use
flights %>% 
  select(year:day)

# equals base-R
flights[1:3]

# we can use similar code to kick out the year, month and day variables
## Option 1: call every single variable
flights %>% 
  select(-year, -month, -day)

## Option 2: call neighboring variables
flights %>% 
  select(-(year:day))

##Option 3: call different variables by creating a vector of their names
flights %>% 
  select(-c("year", "month", "day"))

vars <- c("year", "month", "day")

# 
flights %>% 
  select(-all_of(vars))
?all_of()


# 3.4 create new variables with mutate()
# get a smaller dataset for a better overview
flights_sml <- flights %>% 
                select(year:day, # the variables year, month and day
                      ends_with("delay"),  # all variables ending with delay
                      ends_with("time"),
                      time_hour)
flights_sml


# create new variables; mutate()
# Here: let's check if recorded delays match the recorded flight times
# Option 1: mutate() will keep the original variables
flights_sml %>% 
  mutate(dep_delay2 = dep_time - sched_dep_time, 
         arr_delay2 = arr_time - sched_arr_time)

# Option 2: transmute() will delete the original variables
flights_sml %>% 
  transmute(dep_delay2 = dep_time - sched_dep_time, 
         arr_delay2 = arr_time - sched_arr_time)

# In this scenario, mutate() is more useful
flights_sml2 <- flights_sml %>% 
                  mutate(dep_delay2 = dep_time - sched_dep_time, 
                         arr_delay2 = arr_time - sched_arr_time)
flights_sml2


## So let's check a random sample of the data
# you could work with flights_sml2
# but you can also directly use the flights data
# the latter approach highlights the advantages
# of modular organization of code in Tidyverse

?flights

flights_test <- flights %>% 
  sample_n(10000) # get a sample of 10000 observations

flights_test %>% 
  select(year:day,  # select the meaningful variables
         ends_with("delay"),
         ends_with("time"),
         time_hour) %>% 
  mutate(dep_delay2 = dep_time - sched_dep_time, # create new delay variables
         arr_delay2 = arr_time - sched_arr_time) %>%
  filter(!is.na(dep_delay) & !is.na(dep_delay2)) %>% 
  ggplot(mapping = aes(x = dep_delay, y = dep_delay2)) +
    geom_point() + 
    labs(y = "departure delay in minutes (calculated)",
         x = "departure delay in minutes (original dataset)",
         title = "Departure delay consistency check")

# This looks weird:
## the upper part is alright
## The lower part seems to have high negative delays
## let's focus in on that:

flights_sml2 %>% 
  select(dep_delay2, dep_time, sched_dep_time, time_hour) %>% 
  arrange(dep_delay2)
# Some flights have a delay stretching over midnight
## so our calculation was distorted.




# Exercise 2: Try to fix the problem
# We want a subset of data without such distortions


## Departure delay
flights %>% 
  # First: select the variables
  select(year:day,  
         ends_with("delay"),
         ends_with("time"),
         time_hour) %>% 
  # Second: create new variable
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
  labs(x = "departure delay in minutes (original dataset)",
       y = "departure delay in minutes (calculated)",
       title = "Departure delay consistency check")




## Homework assignment
# Make a similar plot for arrival delay





# 3.5 Grouping and summaries with summarize() and group_by()

# 3.5.1 simple summary statistics

# with summarize, we can get summary statistics of the data set

flights %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))

# which is equivalent to base-R's
mean(flights$dep_delay, na.rm = TRUE)

# Summarize/summarise can also help you count special values with Tidyverse
# (see: section 2.1.5.2 above)
# you could also count NAs in tidyverse:
?across() # applies function over multiple columns
?everything() # selects all variables
flights %>%
  summarise(across(everything(), ~ sum(is.na(.))))

flights %>%
  summarise(across(everything(), ~ sum(is.nan(.))))

flights %>%
  summarise(across(everything(), ~ sum(is.infinite(.))))

# this is one exception where base-R is more practical in my opinion



# 3.5.2 simple grouped summary

# There are various options for grouped summaries

# a. pre-operation grouping in summarize()
flights %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE), 
            .by = origin)


# b. persistent grouping with group_by()
flights %>%                                                                                                                                                                                                             
  group_by(origin) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))
## group_by() will permanently group the data
## you may need to use ungroup() to get rid of it again





# In base-R, you could use the aggregate() function
?aggregate()
aggregate(dep_delay ~ origin, # specify formula
                    data = flights,  # specify data
                    FUN = function(x) mean(x, na.rm = TRUE)) # specify function

# Alternatively, you could also use one of the apply functions:
# (see also: above)
?tapply()
tapply(flights$dep_delay, 
       flights$origin, 
       FUN = function(x) mean(x, na.rm = TRUE))



# Let's continue with the tidyverse functions


# 3.5.3 groupings with 2 or more categorical variables

# a. pre-operation grouping with 2 categorical variables 
flights %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE), 
            .by = c("origin", "dest"))

## you get unique pairs of categories
flights %>% 
  filter(dest < "b") %>% # only destinations that start with an "a"
  summarise(delay = mean(dep_delay, na.rm = TRUE), 
            .by = c("origin", "dest"))

# b. persistent grouping with 2 variables

flights %>% 
  filter(dest < "b") %>% 
  group_by(origin, dest) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))
 
# same results as above, but in different order
# also unique pairs


# 3.5.4 grouping and mutate()

# Maybe we want to have mean arrival and departure delays
# for origin and destination?

flights %>% 
  # First: group by airport of origin
  group_by(origin) %>% 
  # Second: calculate the average departure delay for the 3 airports
  mutate(mean_dep_del = mean(dep_delay, na.rm = TRUE)) %>% 
  # Third: ungroup the data again
  ungroup() %>% # dissolves the previous grouping again
  # Fourth: regroup the data by airport of destination
  group_by(dest) %>% 
  # Fifth: calculate the average arrival delay for each destination
  mutate(mean_arr_del = mean(arr_delay, na.rm = TRUE)) %>% 
  # Sixth: ungroup the data again
  ungroup() %>% 
  # Seventh: inspect the top rows of the data set
  select(origin, mean_dep_del, dest, mean_arr_del)

flights %>% 
  group_by(origin) %>% 
  mutate(mean_dep_del = mean(dep_delay, na.rm = TRUE)) %>% 
  ungroup() %>% # dissolves the previous grouping again
  group_by(dest) %>% 
  mutate(mean_arr_del = mean(dep_delay, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(origin, mean_dep_del, dest,  mean_arr_del) %>% 
  sample_n(10000) %>% # careful not to crash the whole thing
  ggplot() +
    geom_boxplot(mapping = aes(x=origin, y = mean_arr_del), 
                 color = "darkblue", fill = "yellowgreen") +
    labs(title = "Average Delays by Airport of Origin",
         subtitle = "a random sample of 10000 flights",
         x = "airport of origin",
         y = "average arrival delay in minutes")




# 4. Pivoting 

# Sometimes we need to reorganize the data.
# Ordering some variables according to the 
# values of other variables is a common scenario.

# let's make a data set of the number of flights by origin and month

OrMo_fli <- flights %>% 
  group_by(origin, month) %>% 
  summarise(flights = n())

View(OrMo_fli)

# This data set is in long format.


## 4.1 Pivot Wider

# Say we want to compare how many flights leave from each airport every month.
# A wide format is more conducive to that
# We can transform the data set, so that origin is in the columns

OrMo_fli_wi <- OrMo_fli %>%
  pivot_wider(names_from = origin, values_from = flights)
OrMo_fli_wi

# This is much nicer

# Alternatively:
OrMo_fli_wi2 <- OrMo_fli %>%
  pivot_wider(names_from = month, values_from = flights) 
OrMo_fli_wi2

colnames(OrMo_fli_wi2) <-  c("origin", "jan", "feb", "mar", "apr", "may", "jun","jul", "aug", "sep", "oct", "nov", "dec")

OrMo_fli_wi2


## 4.2 Pivot Longer

# Pivot longer can do the reverse operation.
# This is often handy when we do panel data analyses

?pivot_wider()
OrMo_fli_lo <- OrMo_fli_wi2 %>%
  pivot_longer(c("jan", "feb", "mar", "apr", "may", "jun","jul", "aug", "sep", "oct", "nov", "dec"),
                names_to = "month", values_to = "flights") 
OrMo_fli_lo





# 5. Relational data

# 5.1 Inspect datasets
# In data science we often want to combine multiple data sets.
# In doing so, we refer back to principles from relational data bases.

# These principles include:
## 1) Preferably, each information is stored only once to minimize redundance
## 2) every observation needs to be identified by a unique key
## 3) relations: one-to-one, one-to-many, many-to-one

?flights
glimpse(flights) # 336776 flights
# no key --> create one

?airports # 1458 airports
glimpse(airports)
# we do not want to save longitude and latitude destination over 300,000 times ...
# when 1.458 times is enough
# key: faa
# relationship: many flights to one airport



?airlines
glimpse(airlines)
# key: carrier
# relationship: many flights to one airline

?planes
glimpse(planes)
# key: tail number
# relationship: many flights to one airplane

?weather
glimpse(weather)
# composite key: origin + date & time
# relationship: many flights to one state of weather at a given time and place


# 5.2 Keys

# 5.2.1 fundamentals
# uniquely identify an observation
# needed to merge data frames
# further information: any book on relational databases (such as MS Access)

# primary key: uniquely identifies an observation in its own table
planes$tailnum
glimpse(planes)
length(planes$tailnum) # tail number is a unique identifier for planes
length(unique(planes$tailnum))
# There are 3322 tailnumers for 3322 planes in the data set


# foreign key: uniquely identifies an observation in another table
flights$tailnum
glimpse(flights)
length(unique(flights$tailnum)) # 4044 unique tail numbers: more than in planes
summary(is.na(flights$tailnum))

# 5.2.2 create a surrogate key: 
# If we do not have a key we can create one with the row number
flights_key <- flights %>%
  mutate(key = row_number()) 
flights_key$key


# 5.2.3 checking keys
## let's get a subset for an easier overview
flights_key2 <- flights_key %>%
  select(year:day, hour, origin, dest, tailnum, carrier, key)
flights_key2

# 5.2.3.1 check for NAs

sapply(flights_key2, function(x) sum(is.na(x)))
# 2512 NAs in tail number



# 5.2.3.2 check how keys match
summary(planes$tailnum %in% flights_key2$tailnum)
## %in% operator: check if elements of vector 1 are present in vector 2
### all tail numbers from planes are present in flights

summary(flights_key2$tailnum  %in% planes$tailnum)
## Not all tail numbers from flights are present in planes
## look at the cases: 
flights_key2 %>%
  filter(tailnum  %in% planes$tailnum == FALSE)




# 5.3 Joins

## Let's create some mess in the data to make it more interesting
planes2 <- planes
summary(planes2$year)
# let's invent a fake tail number for all planes built until 1998 ...
planes2$tailnum[planes2$year<=1998] <- "N9999"

summary(planes2$tailnum %in% flights_key2$tailnum)


# 5.3.1 joining datasets
?inner_join()
# Many-to-one relation: many flights can connect to one plane

# Inner join: keep all observations present in both, flights and planes
flights_key2 %>%
  select(-origin, -dest) %>% 
  inner_join(planes2, by = "tailnum") # 217,525 observations

# Left-join: keeps all observations in x (flights)
flights_key2 %>%
  select(-origin, -dest) %>% 
  left_join(planes2, by = "tailnum") #%>% # 336,776 observations, but ...
  filter(tailnum  %in% planes$tailnum == FALSE)  # ... including 52,606  not corresponding to planes

# Right-join: keeps all observations in planes
flights_key2 %>%
  select(-origin, -dest) %>% 
  right_join(planes2, by = "tailnum") # 218,546 observations


# semi_join(x, y) keeps all observations in flights that have a match in planes.
flights_key2 %>%
  select(-origin, -dest) %>% 
  semi_join(planes2, by = "tailnum") # 217,525 observations
# here equivalent to inner_join()

# anti_join(x, y) drops all observations in flights that have a match in planes.
flights_key2 %>%
  select(-origin, -dest) %>% 
  anti_join(planes2, by = "tailnum") %>% #119,251 flights
  filter(tailnum  %in% planes$tailnum == FALSE) # compare to original planes data
# including 52,606 not corresponding to (original) planes


# Full join: keeps all observations present in both flights and planes
flights_key2 %>%
  select(-origin, -dest) %>% 
  full_join(planes2, by = "tailnum") # 337,797 observations
# keeping flights with no tail number or one not listed in planes ...



# 5.3.3 Defining key columns

# so far: 2 table joined by single variable which is the same in both tables
# now: more and different variables

# 5.3.3.1 default
# default: natural join matching on all columns
glimpse(weather)
flights_key2 %>% 
  left_join(weather)
#Joining, by = c("year", "month", "day", "hour", "origin")


# 5.3.3.2 defining keys to avoid default

# character vector by = "x"
glimpse(planes)
glimpse(flights)

#join flights & planes by tail numbers only (because years mean different things)
flights_key2 %>% 
  inner_join(planes, by = "tailnum") # 284,170

flights_key2 %>% 
  inner_join(planes) # 4,630 observations, matched on tailnum and year
## but year means something different in both data sets


# 5.3.3.3 keys with different names
# named character vector: by = c("a" = "b")
## One-to-many relationship: one airport can match both origin or destination
glimpse(airports)
glimpse(flights)

flights_key2 %>% 
  arrange(origin)

summary(airports$faa %in% flights_key2$dest) 
# 101 airports do not match any flight destinations in flights
summary(flights_key2$dest %in% airports$faa)
# 7602 flights do not have a destination airport listed in airports

class(c("dest" = "faa"))
attributes(c("dest" = "faa"))

flights_key2 %>% 
  inner_join(airports, by = c("dest" = "faa")) 
# 329,174 flights with destination airport info

flights_key2 %>% 
  inner_join(airports, by = c("origin" = "faa")) %>% 
  # 336,776 flights with origin airport info
  glimpse()




