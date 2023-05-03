library(fpp3)
library(tidyverse)
thefts <- read_csv("data/thefts_by_month.csv")
print(thefts)
thefts <- thefts %>%
  mutate(month = month.name[month]) %>%
  mutate(Month = str_c(year, month, sep = " ")) %>%
  rename(NumThefts = `sum(Count)`, Year = year) %>%
  select(-c(month)) %>%
  drop_na(Month) %>%
  mutate(Month = yearmonth(Month)) %>%
  filter(year(Month) < 2023)

chicago_population <- read_csv("data/all_chicago_pop_estimates.csv")
print(head(thefts))
print(head(chicago_population))
chicago_population2 <- read_csv("data/Chicago_Population_Counts.csv") %>%
  filter(Year > 2019) %>%
  group_by(Year) %>%
  summarize(Population = sum(`Population - Total`)) %>%
  add_row(chicago_population)
thefts_pop <- thefts %>%
  left_join(chicago_population2, by = "Year")

thefts_pop_smaller = thefts_pop %>%
  select(c(NumThefts, Month, Population))

thefts_pop_smaller_ts <- thefts_pop_smaller %>%
  as_tsibble(index = Month)
thefts_prop_to_pop = thefts_pop_smaller %>%
  mutate(NumTheftsPer10000 = NumThefts / Population * 10000)

write_csv(thefts_prop_to_pop, "data/thefts_scaled_by_chicago_population.csv")