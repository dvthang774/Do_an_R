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
#India
india <- covid19 %>% filter(country_region == 'India')
india <- india[c('date','new_cases_n')]
colnames(india)[2]<-'India'
his<-merge(his,india,by='date')


#Fig

library(plotly)


density1 <- density(his$US)
density2 <- density(his$India)
fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'US', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'India', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'New cases'),
                      yaxis = list(title = 'Density'))

fig
