#importing libraries
library(readxl)
library(tidyverse)

#importing the csv file
example02 <- read.csv("Example-02.csv")
example02 #displaying the object

#filtering the data by Nambour and RedlandBay locations
example02 |>
  filter(loc== 'Nambour' | loc== 'RedlandBay')

#filtering the data by genotypes (G01, G57, G58), Brookstead location for the year 1970
example02 |>
  filter(gen== 'G01' | gen== 'G57' | gen== 'G58', loc== 'Brookstead', year== 1970)

#filtering the data by Lawes location, yield between 2 and 3 inclusive and oil greater than 22
example02 |>
  filter(loc== 'Lawes', yield >= 2, yield <= 3, oil> 22)

#importing the excel file
example03 <- read_excel("Example-03.xlsx")

#Number of observations per location
example03 |>
  group_by(loc) |>
  summarise(n=n())

#Number of numeric and non-numeric variables
sum(sapply(example03, is.numeric))
sum(!sapply(example03, is.numeric))

#Coverting non-numeric variables to factors
example03 <- example03 |>
  mutate_if(~ !is.numeric(.), as.factor)

#List of locations and number of locations
locations <- example03 |>
  distinct(loc)
n_locations <- nrow(locations)
#List of genotypes and number of genotypes
genotypes <- example03 |>
  distinct(gen)
n_genotypes <- nrow(genotypes)

#filtering data with yield less than 150
example03 |>
  filter(yield < 150)

#mean of earht per genotype, sorted in descending order
example03 |>
  group_by(gen) |>
  summarise(mean_earht = mean(earht, na.rm = TRUE)) |>
  arrange(desc(mean_earht))

#Selecting loc, gen, yield, flower to be saved in a new object (example03.short)
example03.short <- example03 |>
  select(loc, gen, flower, yield) |>
  mutate(flower_new= flower - 10) #new variable: minus 10days from flower variable
example03.short

#Locations per year
example02 |>
  group_by(year) |>
  summarise(n_loc = n_distinct(loc))

# Number of observations, min, max, mean, variance and
# standard deviation of size by location and year
example02 |>
  group_by(loc,year)|>
  summarise(min_size=min(size, na.rm=TRUE), max_size=max(size, na.rm=TRUE),
            mean_size=mean(size, na.rm=TRUE), var_size= var(size, na.rm=TRUE),
            std_size= sd(size, na.rm=TRUE), Count=n())

#filtering the data where oil is greater than 20, lodging less than 3, and
#yield greater than 3 in Brookstead location with height in descending order
example02 |>
  filter(loc== 'Brookstead', oil> 20, lodging< 3, yield> 3) |>
  arrange(desc(height))
