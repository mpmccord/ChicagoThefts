library(tidyverse)
data = read_csv("data/il_pop_estimates.csv")
chicago_data <- data %>%
  filter(NAME == "Chicago city")
print(chicago_data)
print(colnames(chicago_data))
pop_estimates = chicago_data %>%
  select(-starts_with("CENSUS")) %>%
  pivot_longer(
    cols = starts_with("POPESTIMATE"), names_to = "Year", 
    names_prefix = "POPESTIMATE", values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Population = as.numeric(Population))
census_data <- chicago_data %>%
  select(-starts_with("POPESTIMATE")) %>%
  pivot_longer(
    cols = starts_with("CENSUS"), names_to = "Year", names_prefix = "CENSUS", values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(str_extract(Year, "\\d+")))

pop_estimates %>%
  group_by(Year) %>%
  summarize(Population = sum(Population)) %>%
  write_csv("data/all_chicago_pop_estimates.csv")

pop_estimates %>%
  filter(NAME == "Chicago city") %>%
  group_by(Year) %>%
  summarize(Population = sum(Population)) %>%
  write_csv("data/chicago_city_pop_estimates.csv")
