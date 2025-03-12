# Ava Madalinski, 2/18/2025, Exercise 7

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
head(covid, 5)

# Question 1

top_states <- covid |>
  filter(date == max(date)) |>
  group_by(state) |>
  summarize(total_cases = sum(cases, na.rm = TRUE)) |>
  slice(1:6) |>
  pull(state) ->
  top_states
  
 covid |>
    filter(state %in% top_states) %>% 
    group_by(state, date) |>
    summarize(total_cases = sum(cases, na.rm = TRUE)) |>
    group_by(state, date) |>
    ungroup()
  
  
 ggplot(data=filtered_data,
        aes(x = date, y = total_cases, color = state)) +
   geom_line(size = 2) +
  labs(title = "COVID-19 Cases in Top Six States",
       x = "Dates",
       y = "Total Cases") +
    facet_wrap(~state) +
    theme(legend.position = 'NA' )
 
 # Question 2

covid |>
   group_by(date) |>
   summarize(cases = sum(cases))
   ggplot(data = covid,
     aes(x = date, y = cases)) +
   geom_col(fill = "blue", color = "blue", alpha = .25) +
   geom_line(color = 'blue', size = 3) +
   ggthemes::theme_gdocs() 
    