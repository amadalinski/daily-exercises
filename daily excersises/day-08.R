# Ava Madalinski, 2/24/25 
# Daily Exercise 8, Joining and pivoting

library(tidyverse)
library(ggplot2)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

df = data.frame(region = state.region,
                state = state.name,
                abbr = state.abb)

inner_join(df, covid, by = "state") |>
  group_by(region, date) |>
  summarise(cases = sum(cases),
            deaths = sum(deaths)) |>
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count") |>
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(type~region, scales = "free") +
  labs(title = 'Cummulative Cases and Deaths by Region',
       x = "Date",
       y = "Daily Cummulative Count",
       color = "")


