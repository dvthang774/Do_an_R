library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
theme_set(theme_minimal())
#ConfirmDataset
confirm_url<-'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirm <- read_csv(confirm_url)
confirm <- confirm %>%
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
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  ungroup()
#Brazil
bra<-confirm %>% filter(country_region == "Brazil")
bra <-bra[c('date','new_cases_n')]
colnames(bra)[2]<-'Brazil'
boxplot<-bra
#India
india <-confirm %>% filter(country_region == "India")
india <-india[c('date','new_cases_n')]
colnames(india)[2]<-'India'
boxplot <- merge(boxplot,india,by="date")
#US
us <- confirm %>% filter(country_region == "US")
us <- us[c('date','new_cases_n')]
colnames(us)[2]<-'US'
boxplot <- merge(boxplot,us,by="date")
#UK
uk<-confirm %>% filter(country_region == "United Kingdom")
uk <-uk[c('date','new_cases_n')]
colnames(uk)[2]<-'UK'
timeseries <- merge(boxplot,uk,by="date")


fig <- plot_ly(timeseries, x = ~date)
fig <- fig %>% add_lines(y = ~US, name = "US")
fig <- fig %>% add_lines(y = ~India, name = "India")
fig <- fig %>% add_lines(y = ~Brazil, name = "Brazil")
fig <- fig %>% layout(
  title = "Newcases Timeseries",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 3,
          label = "3 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 6,
          label = "6 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "1 yr",
          step = "year",
          stepmode = "backward"),
        list(
          count = 1,
          label = "YTD",
          step = "year",
          stepmode = "todate"),
        list(step = "all"))),
    
    rangeslider = list(type = "date")),
  
  yaxis = list(title = "cases"))

fig
