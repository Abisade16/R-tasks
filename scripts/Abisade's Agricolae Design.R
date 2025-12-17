#Load libraries
library(tidyverse)
library(agricolae)
library(readxl)

#Importing data set
new_data <- read_excel("FINAL Olaleye's list.xlsx") 

trt <- unique(new_data$`Accn No`)
k <- 10
r <- 4
locations=c('Ibadan', 'Kano', 'Delta', 'Lagos', 'Abuja')

#location1 = Ibadan
location1_1 <- design.lattice(trt,r=2,serie=3,seed=1324)
location1_1$sketch
location1_11 <- location1_1$book
location1_11$Location <- "Ibadan"

location1_2 <- design.lattice(trt,r=2,serie=3,seed=314)
location1_2$sketch
location1_21 <- location1_2$book
location1_21$Location <- "Ibadan"

Ibadan <- rbind(location1_11,location1_21)

#location2 = Kano
location2_1 <- design.lattice(trt,r=2,serie=3,seed=1234)
location2_1$sketch
location2_11 <- location2_1$book
location2_11$Location <- "Kano"

location2_2 <- design.lattice(trt,r=2,serie=3,seed=402)
location2_2$sketch
location2_21 <- location2_2$book
location2_21$Location <- "Kano"

Kano <- rbind(location2_11,location2_21)

#location3 = Delta
location3_1 <- design.lattice(trt,r=2,serie=3,seed=300)
location3_1$sketch
location3_11 <- location3_1$book
location3_11$Location <- "Delta"

location3_2 <- design.lattice(trt,r=2,serie=3,seed=507)
location3_2$sketch
location3_21 <- location3_2$book
location3_21$Location <- "Delta"

Delta <- rbind(location3_11,location3_21)

#location4 = Lagos
location4_1 <- design.lattice(trt,r=2,serie=3,seed=5004)
location4_1$sketch
location4_11 <- location4_1$book
location4_11$Location <- "Lagos"

location4_2 <- design.lattice(trt,r=2,serie=3,seed=125)
location4_2$sketch
location4_21 <- location4_2$book
location4_21$Location <- "Lagos"

Lagos <- rbind(location4_11,location4_21)

#location5 = Abuja
location5_1 <- design.lattice(trt,r=2,serie=3,seed=324)
location5_1$sketch
location5_11 <- location5_1$book
location5_11$Location <- "Abuja"

location5_2 <- design.lattice(trt,r=2,serie=3,seed=769)
location5_2$sketch
location5_21 <- location5_2$book
location5_21$Location <- "Abuja"

Abuja <- rbind(location5_11,location5_21)

#combining all the locations
plan <- rbind(Ibadan,Kano,Delta,Lagos,Abuja)

#splitting into different sheets per location
sheets1 <- split(plan, plan$Location)

# Write to Excel with each location as a separate sheet
writexl::write_xlsx(sheets1, path = "Abisade's Agricolae Design.xlsx")
