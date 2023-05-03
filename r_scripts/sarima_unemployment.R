library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_unemployment_population.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

thefts %>%
  ACF(`Unemployment Rate`) %>%
  autoplot() %>%
  print()

thefts %>%
  PACF(`Unemployment Rate`) %>%
  autoplot() %>%
  print()

thefts %>%
  features(`Unemployment Rate`, unitroot_ndiffs)

thefts %>%
  features(`Unemployment Rate`, unitroot_nsdiffs)
thefts %>%
  ACF(difference(`Unemployment Rate`, 1)) %>%
  autoplot() %>%
  print()

thefts %>%
  PACF(difference(`Unemployment Rate`, 1)) %>%
  autoplot() %>%
  print()

lambda <- thefts %>%
  features(`Unemployment Rate`, guerrero) %>%
  pull()

thefts_train <- thefts %>%
  filter(year(Month) < 2018)
thefts.model <- thefts_train %>%
  model(
    arimaauto = ARIMA(`Unemployment Rate`),
    arimatransformed = ARIMA(box_cox(`Unemployment Rate`, lambda)),
    arima100 = ARIMA(`Unemployment Rate` ~ 0 + pdq(1, 0, 0)),
    arima001 = ARIMA(`Unemployment Rate` ~ 0 + pdq(0, 0, 1)),
    arima101 = ARIMA(`Unemployment Rate` ~ 0 + pdq(1, 0, 1))
    )

thefts.model %>%
  forecast(h = "4 years") %>%
  autoplot(thefts) %>%
  print()

thefts.model %>%
  forecast(h = "4 years") %>%
  accuracy(thefts) %>%
  print()

thefts.model %>%
  select(arimaauto) %>%
  gg_tsresiduals()
