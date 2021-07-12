library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
theme_set(theme_minimal())

covid19_raw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid19 <- covid19_raw %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()

covid19 <- covid19 %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()
#US
us <-covid19 %>% filter(country_region == "US")
us <- us[c('date','new_cases_n')]
colnames(us)[2]<-'US'
his<-us

#Fig
fig <- plot_ly(his)
fig<-fig %>% add_trace(type='histogram',
                       x=~US, name='US', marker=list(opacity=0.85))

fig <- fig %>% layout(
  title = "Distribute of Newcase",
  xaxis = list(title = "New cases confirmed each Day in US"),
  yaxis = list(title = "Days"),
  barmode="overlay",
  bargap=0.1)
fig