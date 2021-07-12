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
colnames(bra)[2]<-'NewCases'
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
boxplot <- merge(boxplot,uk,by="date")

#Boxplot:
fig <- plot_ly(boxplot, y = ~NewCases, type = "box",name = 'Brazil')
fig<- fig%>% add_trace(y = ~India, type='box', name='India',boxpoints = FALSE)
fig<- fig%>% add_trace(y = ~US, type='box', name='US',boxpoints = FALSE)
fig<- fig%>% add_trace(y = ~UK, type='box', name='UK',boxpoints = FALSE)
fig <- fig %>% layout(title = "Box Plot New cases by Country")
fig
