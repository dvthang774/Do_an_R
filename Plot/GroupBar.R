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
groupbar <- country %>% filter(Country_Region == "Brazil")
india<-country %>% filter(Country_Region == "India")
groupbar<-rbind(groupbar,india)
rus<-country %>% filter(Country_Region == "Russia")
groupbar<-rbind(groupbar,rus)
#Fig
fig <- plot_ly(groupbar, x = ~Country_Region, y = ~Confirmed, type = 'bar', name = 'Confirmed',text =groupbar$Confirmed, textposition = 'auto',
               marker= list(color='lightred'))
fig <- fig %>% add_trace(y = ~Recovered, name = 'Recovered',text =groupbar$Recovered, textposition = 'auto',
                         marker=list(color='lightgreen'))
fig <- fig %>% add_trace(y = ~Deaths, name = 'Deaths',text =groupbar$Deaths, textposition = 'auto',
                         marker=list(color='red'))
fig <- fig %>% layout(title='Group Bar Chart',
                      yaxis = list(title = 'Count'),
                      xaxis = list(title = 'Country'), barmode = 'group')

fig
