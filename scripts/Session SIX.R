#importing libraries
library(tidyverse)
library(agricolae)

#1. RCBD. 15 gens, 3reps
trt <- 1:15
r <- 3
rcbd <- design.rcbd(trt,r,serie=1,seed=1234)
rcbd$book

#2. Alpha Design. 106+4 treatments, 3reps
alpha_trt <- 1:110
alpha_design <- design.alpha(trt = alpha_trt, k = 10, r = 3, serie = 2, seed = 123)
alpha_design$book

#3. ARCBD. 50 treatments, 3 checks
treaments <- paste("T", 1:50, sep="")
checks <- c("C1", "C2", "C3")
reps <- (12/(length(checks)-1))+1
ARCBD <- design.dau(trt1 <- checks, trt2 <- treaments, reps, serie = 1, seed = 456)
ARCBD$book

library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, "RCBD Design")
writeData(wb, "RCBD Design", rcbd$book)

addWorksheet(wb, "Alpha Design")
writeData(wb, "Alpha Design", alpha_design$book)

addWorksheet(wb, "ARCBD Design")
writeData(wb, "ARCBD Design", ARCBD$book)

# Save the workbook
saveWorkbook(wb, file = "Abisade's Experimental_Designs practice.xlsx")