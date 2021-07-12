library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
theme_set(theme_minimal())
#ConfirmDataset
confirm_url<-'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirm <- read.csv(confirm_url,header = TRUE)
# grouped boxplot
ggplot(confirm, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()