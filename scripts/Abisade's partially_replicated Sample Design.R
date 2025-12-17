library(tidyverse)
library(FielDHub)
library(readxl)
library(openxlsx)

#importing dataset
unreplicated <- read_excel("Copy EBITI_TARO INVENTORY.xlsx", sheet = "UNREPLICATED")
replicated <- read_excel("Copy EBITI_TARO INVENTORY.xlsx", sheet = "REPLICATED")

#separating the checks and treatments 
checks <- as.character(replicated$Samples)
treatments <- as.character(unreplicated$Samples)

sample_design <- partially_replicated(
  nrows = 9,
  ncols = 15,
  repGens = c(81,27),
  repUnits = c(1, 2),
  plotNumber = 1,
  seed = 1234,
)

# View field layout
head(sample_design$fieldBook)

all_names <- c(treatments, checks)

# Relabel ENTRY column with treatment names
sample_design$fieldBook$ENTRY <- factor(
  sample_design$fieldBook$ENTRY,
  levels = 1:length(all_names),
  labels = all_names
)

wb <- createWorkbook()

addWorksheet(wb, "Sample Design")
writeData(wb, "Sample Design", sample_design$fieldBook)

# Save the workbook
saveWorkbook(wb, file = "Abisade's partially_replicated Sample Design.xlsx")

#plot the layout
plot(sample_design)