library(fpp3)
library(tidyverse)
unemployment_illinois <- read_csv("data/illinois_unemployment.csv")
unemployment_chicago <- readxl::read_excel("data/Chicago-Naperville-ArlingtonHeights_MD_notseasadj.xls", skip = 6) %>%
  drop_na() %>%
  rename(Month = `Month/Year`) %>%
  mutate(`Month` = yearmonth(Month)) %>%
  as_tsibble(index = Month)

unemployment_chicago %>%
  features(difference(difference(`Unemployment Rate`, 1), 12), unitroot_kpss)

unemployment_chicago %>%
  autoplot(`Unemployment Rate`)

unemployment_chicago_train <- unemployment_chicago %>%
  filter(year(Month) < 2022)

unemployment.fit <- unemployment_chicago_train %>%
  model(
    ets = ETS(`Unemployment Rate`),
    arima = ARIMA(`Unemployment Rate`),
    snaive = SNAIVE(`Unemployment Rate`),
    tslm = TSLM(`Unemployment Rate`)
  )

unemployment.fit %>%
  forecast(h = "5 years") %>%
  autoplot(unemployment_chicago) %>%
  print()



unemployment.fit %>%
  forecast(h = "5 years") %>%
  accuracy(unemployment_chicago) %>%
  print()

unemployment.fit %>%
  select(ets) %>%
  components() %>%
  autoplot() %>%
  print()



unemployment.fit %>%
  select(ets) %>%
  gg_tsresiduals()
