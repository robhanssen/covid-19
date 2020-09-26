#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)

# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid <- read_csv(covidfile)

continents = read_csv("sources/countries-by-continent.csv")

# process the time series into proper dataframe
#covid <- melt(covid, id=c("Province/State","Country/Region", "Lat","Long"))

covid <- covid %>% pivot_longer(!c("Province/State","Country/Region", "Lat","Long"), 
   names_to = "date",
   values_to = "infections")
   


# clean up column names 
colnames(covid) = c("province","region","lat","long","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1


# assign locations to differentiate between counntries/groups of countries
covid <- covid %>% inner_join(continents) %>% rename(location = continent)
covid$location[is.na(covid$location)] = "Other"
#View(covid)

# total spread of infections by countries
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(infections)) %>% arrange(location, time)
spread$count[spread$count==0] = 1e-1

#
# clean up graphs and plot
#

capt = paste("Source: JHU\nlast updated:", lastupdated)

spread %>% ggplot + aes(date, count, color=location) + geom_point()  + 
                                        scale_y_log10(limit=c(1e3,1e7))  + labs(caption=capt) + 
                                        xlab("Date") + ylab("Infections") + ggtitle("Spread of COVID-19 infections") 
                                        

 filename_base = "covid-global-spread-continents"

 datafilename = paste("data/", filename_base, ".csv", sep="")
 graphfilename = paste("graphs/", filename_base, ".pdf", sep="")

 ggsave(graphfilename, device="pdf")
 write_csv(spread, datafilename)

