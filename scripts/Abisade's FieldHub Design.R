library(FielDHub)
library(readxl)

#importing the dataset
dat <- read_excel("FINAL Olaleye's list.xlsx") 

entry <- 1:100

treatment_list <- data.frame(list(Entry = entry, Accession = dat$`Accn No`))

#Alpha lattice design
alpha1 <- alpha_lattice(t = 100,
                        k = 10,
                        r = 4,
                        l = 5,
                        seed = c(333, 112, 7689, 201, 1002),
                        plotNumber = c(100, 200, 300, 400, 500),
                        locationNames = c("Ibadan", "Ikenne", "Zaria", "Ilorin", "Kano"),
                        data = treatment_list)

#plots generated per location
plot(alpha1, l = 1)# Ibadan
plot(alpha1, l = 2) # Ikenne
plot(alpha1, l = 3) # Zaria
plot(alpha1, l = 4) # Ilorin
plot(alpha1, l = 5) # Kano

FieldBook1 <- alpha1$fieldBook

#splitting into different sheets per location
sheets <- split(FieldBook1, FieldBook1$LOCATION)

# Write to Excel with each location as a separate sheet
writexl::write_xlsx(sheets, path = "Abisade's FieldHub Design.xlsx")
