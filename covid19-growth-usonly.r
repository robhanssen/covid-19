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

# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
#covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid <- read_csv(covidfile)

# process the time series into proper dataframe
covid <- melt(covid, id=c("Province/State","Country/Region", "Lat","Long"))

# clean up column names and differentiate between different regions
colnames(covid) = c("province","region","lat","long","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

covid$location[covid$region == "China"] = "China"
covid$location[covid$region == "Italy"] = "Italy"
covid$location[covid$region == "Korea, South"] = "South Korea"
covid$location[covid$region == "US"] = "USA"
covid$location[covid$region == "Russia"] = "Wave 3"
covid$location[covid$region == "Brazil"] = "Wave 3"
covid$location[covid$region == "Peru"] = "Wave 3"
covid$location[covid$region == "Chile"] = "Wave 3"
covid$location[covid$region == "Mexico"] = "Wave 3"
covid$location[covid$region == "Saudi Arabia"] = "Wave 3"
covid$location[covid$region == "India"] = "Wave 3"
covid$location[covid$region == "Bangladesh"] = "Wave 3"
covid$location[covid$region == "Pakistan"] = "Wave 3"
covid$location[covid$region == "Iran"] = "Iran"
covid$location[is.na(covid$location)] = "Other"


# total spread of infections by countries
spread <- covid %>% group_by(time, location) %>% summarise(count=sum(infections))

widespread <- dcast(spread, time ~ location )

covid_growth_us = tibble(widespread$time[widespread$time>1], diff(widespread[,"USA"]), 
                                                             diff(widespread[,"Italy"]), 
                                                             diff(widespread[,"China"]), 
                                                             diff(widespread[,"South Korea"]),
                                                             diff(widespread[,"Wave 3"]),
                                                             diff(widespread[,"Iran"]),
                                                             diff(widespread[,"Other"]))



colnames(covid_growth_us) = c("time", "USA",  "Italy", "China", "South Korea", "Wave 3","Iran","Other")

covid_growth <- melt(covid_growth_us, id=c("time")) %>% arrange(time, variable)
colnames(covid_growth) = c("time", "location", "growth")


capt = paste("Source: JHU\nlast updated:", lastupdated)

covid_growth %>% filter(location =="USA" | location =="Italy") %>%
                  ggplot + aes(time, growth, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        scale_x_continuous() + labs(caption=capt) + #scale_y_log10() +
                        xlab("Days since Jan 22, 2020") + ylab("Growth of Infections") + ggtitle("Per diem growth of COVID-19 infections")

ggsave("graphs/covid-growth-usonly.pdf", device="pdf")
#write_csv(covid_growth, "data/covid-growth.csv")