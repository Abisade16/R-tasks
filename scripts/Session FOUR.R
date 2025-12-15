#importing library
library(tidyverse)

#importing dataset
example02 <- read_csv("Example-02.csv")

#filtering a subset of the data into an object
example02.70 <- example02|>
  filter(year==1970)
example02.70

#summary of env
table(example02.70$env)
#summary of loc
table(example02.70$loc)
#summary of gen
table(example02.70$gen)

#summary of first 3 quantitative variables
vars <- example02.70[5:7]
summary_table <- data.frame(
  Mean = sapply(vars, mean),
  Median = sapply(vars, median),
  Variance = sapply(vars, var),
  SD = sapply(vars, sd)
)
print(summary_table)

#importing dataset
survey <- readxl::read_excel("Farmers.Survey.xlsx", sheet="Farmers.Survey")
#changing data type
survey <- survey |>
  mutate(across(c(sex:Productivity), as.factor)) 

#logistic model
model <- glm(formula= Productivity~ EDUCATION+ YFE+ FARMSIZE+ CFACP, data= survey, family="binomial")
summary(model)

#odds ratio and confidence interval
cbind("Odds ratio" = round(exp(coef(model)),4),
      round(exp(confint.default(model, level = 0.95)),4))

#variable importance
caret::varImp(model)

# Load the nnet package
library(nnet)

# Fit the multinomial logistic regression model
multi <- multinom(INCOME ~ EDUCATION + YFE + MS+ AGE_Group, data = survey)
summary(multi)

#Odds ratio
print(round(exp(coef(multi)), 4))

#variable importance
caret::varImp(multi)
