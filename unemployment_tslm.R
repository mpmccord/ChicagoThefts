library(fpp3)
library(tidyverse)
unemployment_illinois <- read_csv("data/illinois_unemployment.csv")
unemployment_chicago <- readxl::read_excel("data/Chicago-Naperville-ArlingtonHeights_MD_notseasadj.xls", skip = 6) %>%
  drop_na() %>%
  rename(Month = `Month/Year`) %>%
  mutate(`Month` = yearmonth(Month)) %>%
  as_tsibble(index = Month)

lambda <- unemployment_chicago %>%
  features(`Unemployment Rate`, guerrero) %>%
  pull()
unemployment_chicago %>%
  autoplot(`Unemployment Rate`)
unemployment_chicago %>%
  autoplot(box_cox(`Unemployment Rate`, lambda))
recessions = yearmonth(c("February 2001", "December 2001", "December 2007", "June 2009", "January 2020", "May 2020"))
fit_trends <- unemployment_chicago |>
  model(
    exponential = TSLM(log(`Unemployment Rate`) ~ season() + trend(knots = recessions)),
    sqrt = TSLM(sqrt(`Unemployment Rate`) ~ season() + trend(knots = recessions)),
    linear = TSLM((`Unemployment Rate`) ~ season() + trend(knots = recessions)),
    boxcox = TSLM(box_cox(`Unemployment Rate`, lambda) ~ season() + trend(knots = recessions))
    
  )
fc_trends <- fit_trends |> forecast(h = 10)

unemployment_chicago |>
  autoplot(`Unemployment Rate`) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95)

fit_trends %>%
  accuracy()
