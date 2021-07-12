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
                                 Confirmed=sum(Confirmed))
#Sort
country <- country[order(-country$Confirmed),]
country<-country[-c(1,2,3,4,7,11),]
country<-country<-country[1:10,]
fig <- plot_ly(country, y = ~Confirmed, x = ~Country_Region, name = "Confirmed", type = 'scatter',
               mode = "markers", marker = list(color = "Blue"))

fig <- fig %>% add_trace(y = ~Recovered, x = ~Country_Region, name = "Recovered",type = 'scatter',
                         mode = "markers", marker = list(color = "pink"))
fig <- fig %>% layout(
  title = "Dots Plot",
  xaxis = list(title = "Country"),
  yaxis = list(title='Count')
)

fig
