library(tidyverse)
library(readxl)
library(FielDHub)

#importing dataset
MSC_samples <- read_excel("OMolabke Msc samples.xlsx")

#extracting the treaments
entry <- MSC_samples[,2]
treatment_list <- data.frame(list(Entry = entry, TREATMENT = MSC_samples$`Accession no`))

#running the alpha lattice function
alphaDesign <- alpha_lattice(
  t= 50, k= 5, r= 3, l= 3, plotNumber = c(101,101,101),
  locationNames = c("Ubiaja", "Ikenne", "Ibadan"), seed= c(5684, 234, 9823), data=treatment_list
)

#plot designs for each location
plot(alphaDesign,l=1)#Ubiaja
plot(alphaDesign, l=2) #Ikenne
plot(alphaDesign, l=3) #Ibadan

writexl::write_xlsx(alphaDesign$fieldBook, "Abisade's FieldDesign.xlsx")
