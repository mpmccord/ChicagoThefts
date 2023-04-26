library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_unemployment_population.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)
thefts_train <- thefts %>%
  filter(year(Month) < 2018)
unemployment.ets.model <- thefts_train %>%
  model(ets = ETS(`Unemployment Rate`))

unemployment.ets.model %>%
  components() %>%
  autoplot() %>%
  print()

unemployment.ets.model %>%
  forecast(h = "4 years") %>%
  autoplot(thefts) %>%
  print()

unemployment.ets.model %>%
  gg_tsresiduals() %>%
  print()
unemployment.ets.model %>%
  forecast(h = "4 years") %>%
  accuracy(thefts)
