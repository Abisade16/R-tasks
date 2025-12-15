#importing library
library(tidyverse)

#importing dataset
example02 <- read_csv("Example-02.csv")

#new variable with first 3 letters of loc and last 2 digits of year
example02 <- example02 |>
  mutate(loc_part = str_sub(loc, 1, 3),
         year_part = str_sub(year, 3, 4)) |>
  unite(col = "Env", loc_part, year_part)

#categorising yield values into high and low
example02 <- example02|>
  mutate(
    yield_grp= case_when(
      yield >= 3 ~ "High",
      TRUE ~ "Low"
    )
      )

#transforming some columns into ONE variable
example02|>
  select(Env,gen,yield,yield_grp,height,oil)|>
  pivot_longer(c(yield,height,oil), names_to = "Traits", values_to="values")

dat_longer <- example02 %>%
  pivot_longer(yield:lodging, names_to = "trait", values_to = "values")
               