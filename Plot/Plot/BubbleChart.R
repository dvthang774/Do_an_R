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
                                 Deaths=sum(Deaths))
#Sort
country <- country[order(-country$Confirmed),]
country<-country[-c(1,2,3,4,7,11),]
country<-country<-country[1:10,]
fig <- plot_ly(country, x = ~Confirmed, y = ~Recovered, text = ~Country_Region, type = 'scatter', mode = 'markers',
               color = ~Deaths, colors = 'Reds',
               marker = list(size = ~Deaths/5000, opacity = 0.8))
fig <- fig %>% layout(title = 'Bubble Chart')

fig