library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)

#Load Data
url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-29-2021.csv"
df <- read.csv(url, header = TRUE)
#Nhom du lieu
country <- df %>% group_by(Country_Region)
country <- country %>% summarise(Recovered=sum(Recovered),
                                 Confirmed=sum(Confirmed),
                                 Deaths=sum(Deaths),
                                 Active=sum(Active))
#Sort
country <- country[order(-country$Confirmed),]
country<-country[-c(1,2,3,4,7,11),]
country<-country<-country[1:10,]

fig <- plot_ly(country, x = ~Country_Region, y = ~Active, type = 'bar', name = 'Active')
fig <- fig %>% add_trace(y = ~Recovered, name = 'Recovered')
fig <- fig %>% add_trace(y = ~Confirmed, name = 'Confirmed',text =country$Confirmed, textposition = 'auto')
fig <- fig %>% layout(title='Stacked Bar Chart',yaxis = list(title = 'Count'), barmode = 'stack')

fig