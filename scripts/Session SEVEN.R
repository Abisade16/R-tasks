#importing libraries
library(tidyverse)
library(lmerTest)

#importing datasets
omer <- read_csv("Omer.Sorghum.E3.csv")
omer.1 <- read_csv("omer.sorghum.csv")

#ONE-WAY ANOVA
summary(omer)
model <- lmer(yield~gen+(1|rep), data=omer)

#ANOVA testing
anova_testing <- anova(model, ddf="Kenward-Roger",type=3)
anova_testing

#Adjusted means
ADmeans <- emmeans(model, ~gen)

#genotypes different to G05
emmeans(model, specs = trt.vs.ctrl ~ gen, ref = 05)

#TWO WAY ANOVA
summary(omer.1)

#gen and env as fixed effects
fixed_model <- lmer(yield ~ gen+env+gen:env+(1|env:rep), data=omer.1)

#ANOVA testing
Fixedtwo_anova <- anova(fixed_model, ddf="Kenward-Roger", type=3)
Fixedtwo_anova

#Adjusted means: 
ADmeans2 <- emmeans(fixed_model, specs=pairwise ~ gen|env)
ADmeans2

#gen and env as random effects
random_model <- lmer(yield ~ (1|gen)+(1|env)+(1|gen:env)+(1|env:rep), data=omer.1)
summary(random_model)

#Variance componenets
var_components <- as.data.frame(VarCorr(random_model))
var_components

#Extracting variances
gen_var <- var_components$vcov[var_components$grp == "gen"]
gxe_var <- var_components$vcov[var_components$grp == "gen:env"]
res_var <- var_components$vcov[var_components$grp == "Residual"]

# Define your design info
n_env <- length(unique(omer.1$env))
n_rep <- mean(table(omer.1$env, omer.1$rep) != 0)  # average reps per env

# Heritability
H2 <- gen_var / (gen_var + (gxe_var / n_env) + (res_var / (n_env * n_rep)))
H2
