library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_unemployment_population.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

thefts %>%
  ACF(difference(NumTheftsPer10000, 12)) %>%
  autoplot() %>%
  print()

thefts %>%
  PACF(difference(NumTheftsPer10000, 12)) %>%
  autoplot() %>%
  print()



thefts_train <- thefts %>%
  filter(year(Month) <= 2018)
thefts.model <- thefts_train %>%
  model(ARIMA(NumTheftsPer10000))

thefts.model %>%
  forecast(h = "4 years") %>%
  autoplot(thefts) %>%
  print()

thefts.model %>%
  gg_tsresiduals(type = "partial") %>%
  print()

thefts %>%
  features(NumTheftsPer10000, unitroot_ndiffs)

