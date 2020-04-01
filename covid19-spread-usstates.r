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

# selection of states to highlight out of the full data set
covid$location = NA
covid$location[covid$state=="New York"] = "NY"
covid$location[covid$state=="California"] = "CA"
covid$location[covid$state=="New Jersey"] = "NJ"
covid$location[covid$state=="Michigan"] = "MI"
covid$location[covid$state=="Massachusetts"] = "MA"
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by countries
spread <- covid %>% group_by(time, location) %>% summarise(count=sum(infections))

max_x = ceiling(max(spread$time)/10)*10
capt = paste("Source: JHU\nlast updated:", lastupdated)



spread %>% ggplot + aes(time, count, color=location) + geom_point() + geom_line() +
                        scale_x_continuous(limit=c(40,max_x)) + scale_y_log10(limit=c(1e1,1e5)) + labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Number of cases per state") + ggtitle("Spread of COVID19 in the US by state") + 
                        annotate("text", x=60,y=20000, label="NY") +
                        annotate("text", x=62,y=10000, label="Other") +
                        annotate("text", x=67,y=18000, label="NJ") +
                        annotate("text", x=64,y=1000, label="MA") +
                        annotate("text", x=58,y=100, label="MI") +
                        annotate("text", x=58,y=1500, label="CA") 


ggsave("graphs/covid-us-spread.pdf", device="pdf")
write_csv(spread, "data/covid-us-spread.csv")