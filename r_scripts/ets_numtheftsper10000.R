library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_unemployment_population.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)
thefts_train <- thefts %>%
  filter(year(Month) <= 2018)
thefts.ets.model <- thefts_train %>%
  model(ETS(NumTheftsPer10000))

thefts.ets.model %>%
  components() %>%
  autoplot() %>%
  print()

thefts.ets.model %>%
  forecast(h = "4 years") %>%
  autoplot(thefts) %>%
  print()

thefts.ets.model %>%
  gg_tsresiduals() %>%
  print()
