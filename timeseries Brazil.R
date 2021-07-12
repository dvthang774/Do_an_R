library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
theme_set(theme_minimal())
#ConfirmDataset
confirm_url<-'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirm <- read_csv(confirm_url)
confirm <- confirm %>%
  pivot_longer(-c(Province/State, Country/Region, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = Province/State,
    country_region = Country/Region
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()

confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  ungroup()
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()

Brazil <- confirm %>% filter(country_region == "Brazil")


#Recovered
recover_url<-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
recover<-read_csv(recover_url)
recover <- recover %>%
  pivot_longer(-c(Province/State, Country/Region, Lat, Long),
               names_to = "date",
               values_to = "recovered_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = Province/State,
    country_region = Country/Region
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(recovered_n = sum(recovered_n)) %>%
  ungroup()

recover <- recover %>%
  arrange(date) %>%
  group_by(country_region) %>%  ungroup()
recover <- recover %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_recover = recovered_n - lag(recovered_n, default = 0)) %>%
  ungroup()
recover <- recover %>% filter(country_region=='Brazil')
recover <-recover[c('new_recover')]
Brazil <- cbind(Brazil,recover)

#Fig
ay <- list(
  tickfont = list(color = "green"),
  overlaying = "y",
  side = "right",
  title = "Recover cases")

fig <- plot_ly(Brazil)
fig <- fig %>% add_trace(x = ~date,y = ~new_recover, name = "Recovered", yaxis = "y2", type='bar',
                         marker=list(opacity=0.5,color='green'))
fig <- fig %>% add_lines(x = ~date,y = ~new_cases_n, name = "Confirmed", yaxis='y1')
fig <- fig %>% layout(
  title = "Brazil Timeseries newcases",
  xaxis = list( rangeslider = list(type = "date")),
  yaxis2=ay)
fig