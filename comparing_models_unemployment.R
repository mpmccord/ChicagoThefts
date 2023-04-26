library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_unemployment_population.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)
thefts_train <- thefts %>%
  filter(year(Month) < 2022)
thefts.model <- thefts_train %>%
  model(ets = ETS(`Unemployment Rate`),
        arima = ARIMA(`Unemployment Rate`))


thefts.model %>%
  forecast(h = "4 years") %>%
  autoplot(thefts) %>%
  print()

thefts.model %>%
  forecast(h = "1 year") %>%
  accuracy(thefts) %>%
  print()

thefts.model %>%
  select(ets) %>%
  gg_tsresiduals()

thefts.model %>%
  select(ets) %>%
  components() %>%
  autoplot()
