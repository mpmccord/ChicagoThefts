library(fpp3)
library(tidyverse)
chicago_population <- read_csv("data/all_chicago_pop_estimates.csv") %>%
  as_tsibble(index = Year)

chicago_population %>%
  autoplot(Population)

