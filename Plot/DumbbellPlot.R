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
country$Country_Region <- factor(s$Country_Region, levels = s$Country_Region[order(s$Confirmed)])

fig <- plot_ly(country, color = I("gray80"))
fig <- fig %>% add_segments(y = ~Recovered, yend = ~Confirmed, x = ~Country_Region, xend = ~Country_Region, showlegend = FALSE)
fig <- fig %>% add_markers(y = ~Recovered, x = ~Country_Region, name = "Recovered", color = I("aquamarine4"))
fig <- fig %>% add_markers(y = ~Confirmed, x = ~Country_Region, name = "Confirmed", color = I("blue"))
fig <- fig %>% layout(
  title = "Dumbbell Plot",
  xaxis = list(title = "Country"),
  yaxis = list(title='Humans')
)

fig
