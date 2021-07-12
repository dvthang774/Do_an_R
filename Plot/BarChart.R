#Load Data
url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-29-2021.csv"
df <- read.csv(url, header = TRUE)

#Summarize
str(df)
summary(df)
library(dplyr)
library(ggplot2)
library("ggstance")
library(ggcharts)
#Nhom du lieu
country <- df %>% group_by(Country_Region)
country <- country %>% summarise(Deaths = sum(Deaths))
#Sort
country <- country[order(-country$Deaths),]
#BarChart
country %>%
  bar_chart(x = Country_Region, y = Deaths, top_n = 20)