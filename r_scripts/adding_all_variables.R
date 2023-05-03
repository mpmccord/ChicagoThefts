library(tidyverse)
thefts <- read_csv("data/thefts_scaled_by_chicago_population.csv") %>%
  mutate(Month = yearmonth(Month))
unemployment_illinois <- read_csv("data/illinois_unemployment.csv")
unemployment_chicago <- readxl::read_excel("data/Chicago-Naperville-ArlingtonHeights_MD_notseasadj.xls", skip = 6) %>%
  drop_na() %>%
  rename(Month = `Month/Year`) %>%
  mutate(`Month` = yearmonth(Month))
print(unemployment_chicago)

thefts_unemployment <- thefts %>%
  left_join(unemployment_chicago, by = "Month")

thefts_unemployment_ts <- thefts_unemployment %>%
  as_tsibble(index = Month)

write_csv(thefts_unemployment, "data/thefts_unemployment_population.csv")