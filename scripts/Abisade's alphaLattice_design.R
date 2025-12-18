library(tidyverse)
library(FielDHub)
library(readxl)

#importing dataset
data <- read_excel("data.xlsx")

entry <- 1:105
treatment_list <- data.frame(list(Entry = entry, TREATMENT = data$Name))

design <- alpha_lattice(t=105, k=7, r=3, plotNumber=101, seed=1234, data=treatment_list)

writexl::write_xlsx(design$fieldBook, path="Abisade's alphaLattice_design.xlsx")
