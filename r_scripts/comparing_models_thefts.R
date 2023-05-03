library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_unemployment_population.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)
thefts_train <- thefts %>%
  filter(year(Month) < 2018)

lambda <- thefts %>%
  features(NumTheftsPer10000, guerrero) %>%
  pull()
thefts.model <- thefts_train %>%
  model(ets = ETS(NumTheftsPer10000),
        arima = ARIMA(NumTheftsPer10000),
        arimatransformed = ARIMA(box_cox(NumTheftsPer10000, lambda)))


thefts.model %>%
  select(ets) %>%
  components() %>%
  autoplot()
thefts.model %>%
  select(ets) %>%
  gg_tsresiduals()

thefts.model %>%
  select(arimatransformed) %>%
  gg_tsresiduals()

thefts.model %>%
  select(arima) %>%
  gg_tsresiduals()

thefts.model %>%
  forecast(h = "4 years") %>%
  autoplot(thefts) %>%
  print()

thefts.model %>%
  forecast(h = "4 years") %>%
  accuracy(thefts) %>%
  print()