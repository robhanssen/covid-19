#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(tidyverse)
# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

# process the time series into proper dataframe
covid <- melt(covid, id=c("Admin2", "Province_State","Country_Region"))


# clean up column names and differentiate between China/ex-China location
colnames(covid) = c("county","state","country","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

#max_states = covid %>% filter(time==max(covid$time)) %>% group_by(state) %>% summarise(count = sum(infections)) %>% arrange(-count)
#View(max_states)

covid_SC <- covid %>% filter(state=="South Carolina") %>% filter(county == "Greenville" | county == "Spartanburg")

# total spread of infections by countries
spread <- covid_SC %>% group_by(date, county, time) %>% summarise(count=sum(infections))

max_x = ceiling(max(spread$time)/10)*10
capt = paste("Source: JHU\nlast updated:", lastupdated)

spread %>% ggplot + aes(time, count, color=county) + geom_point() + geom_line() +
                        scale_x_continuous(limits=c(50,max_x)) + #scale_y_log10() + 
                        labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Number of cases per county") + ggtitle("Spread of COVID19 in SC by county") + 
                        annotate("text", x=75,y=400, label="Greenville") +
                        annotate("text", x=85,y=100, label="Spartanburg") 


ggsave("graphs/covid-sc-spread.pdf", device="pdf")
write_csv(spread, "data/covid-sc-spread.csv")