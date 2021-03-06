---
title: "Geography 176A"
author: "[Wesley Noble](wes-noble.github.io/)"
date: "08-12-2020"
subtitle: 'Lab 02: COVID-19 Pandemic'
output:
  html_document:
  theme: journal
---

#My Project

##Question 1


library(tidyverse)
library(knitr)
library(zoo)
library(readxl)

covid = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

PopulationEstimates = read_excel("/users/noblex/github/geog-176A-labs/data/PopulationEstimates.xls", skip = 2)

#Question 1

state.of.interest = "California"

(state_covid = covid %>%
  filter(state == state.of.interest) %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup())

(county_most_new_cases = state_covid %>%
    filter(date == max(date)) %>%
    slice_max(newCases, n = 5) %>%
    select(county, newCases))

(county_most_cumulative_cases = state_covid %>%
    filter(date == max(date)) %>%
    slice_max(cases, n = 5) %>%
    select(county, cases))

kable(county_most_new_cases, caption = "Most New Cases California Counties",
      col.names = c("County", "New Cases"),
      format.args = list(big.mark = ","))

kable(county_most_cumulative_cases, caption = "Most Cumulative Cases California Counties",
      col.names = c("County", "Total Cases"),
      format.args = list(big.mark = ","))

(pop_by_county = PopulationEstimates %>%
    filter(State == "CA") %>%
    select(pop19 = POP_ESTIMATE_2019, state = State, county = Area_Name, fips = FIPStxt) %>%
    group_by(county) %>%
    slice_max(pop19, n=1))

(covid_cum_pop = right_join(pop_by_county, state_covid, by = "fips") %>%
    filter(date == max(date)) %>%
    mutate(cases_p_cap = (cases/pop19)) %>%
    slice_max(cases_p_cap, n = 5) %>%
    select(county.y, cases_p_cap))

(covid_newcases_pop = right_join(pop_by_county, state_covid, by = "fips") %>%
    filter(date == max(date)) %>%
    mutate(newcases_p_cap = (newCases/pop19)) %>%
    slice_max(newcases_p_cap, n = 5) %>%
    select(county.y, newcases_p_cap))

kable(covid_cum_pop, caption = "Most Cumulative Cases Per Capita California Counties",
      col.names = c("County", "Cases Per Capita"),
      format.args = list(big.mark = ","))

kable(covid_newcases_pop, caption = "Most Daily New Cases Per Capita California Counties",
      col.names = c("County", "Daily New Cases Per Capita"),
      format.args = list(big.mark = ","))

covid_pop = right_join(pop_by_county, state_covid, by = "fips")

(newCases_last14days_per100000 = covid_pop %>%
  filter(date > max(date) - 14, date < max(date)) %>%
  select(county = county.y, newCases, pop19, date) %>%
  group_by(county, pop19) %>%
  summarise(newCases = sum(newCases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pop_per_100000 = (pop19/100000), newCases_per100000 = (newCases/pop_per_100000)))


(total_state_cases = state_covid %>%
  filter(date == max(date)) %>%
  group_by(county) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  pull(cases))


(total_state_newCases = state_covid %>%
  filter(date == max(date)) %>%
  group_by(county) %>%
  summarise(newCases = sum(newCases, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(newCases = sum(newCases, na.rm = TRUE)) %>%
  pull(newCases))


(healthy_counties = newCases_last14days_per100000 %>%
  filter(newCases_per100000 < 100) %>%
  pull(county))

#Question 2

data2 = covid %>%
  filter(state %in% c("California", "New York", "Louisiana", "Florida")) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases, na.rn = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases), roll7 = zoo::rollmean(newCases, 7, fill = NA, allign = "right")) %>%
  ungroup() %>%
  filter(newCases > 0)

  ggplot(data = data2, aes(x = date, y = newCases)) +
  geom_col(aes(y = newCases), col = NA, fill = "tomato1") +
  geom_line(aes(y = roll7), col = "violetred4", size = 1) +
  facet_grid(~state, scales = "free_y")+
  theme_update() +
  labs(title = paste("New Reported Cases by Day in Select States"), caption = "Lab 2 - Question 2", x = "Date",
       y = "New Cases")

newCases_pop = PopulationEstimates %>%
  select(pop19 = "POP_ESTIMATE_2019", areaname = "Area_Name") %>%
  right_join(data2, by = c("areaname" = "state")) %>%
  mutate(cum_cases_percapita = (cases/pop19), newCases_percapita = (newCases/pop19), roll7 = zoo::rollmean(newCases_percapita, 7, fill = NA, allign = "right"))


ggplot(data = newCases_pop, aes(x = date, y = newCases_percapita)) +
  geom_col(aes(y = newCases_percapita), col = NA, fill = "turquoise2") +
  geom_line(aes(y = roll7), col = "violetred4", size = 1) +
  facet_grid(~areaname, scales = "free_y")+
  theme_update() +
  labs(title = paste("New Reported Cases by Day in Select States Per Capita"), caption = "Lab 2 - Question 2", x = "Date",
       y = "New Cases Per Capita")

