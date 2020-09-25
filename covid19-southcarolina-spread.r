#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(tidyverse)
library(lubridate)
# library(dplyr)
# library(ggplot2)
# library(readr)
# library(reshape2)

# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

# process the time series into proper dataframe
# covid <- melt(covid, id=c("Admin2", "Province_State","Country_Region"))
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
   names_to = "date",
   values_to = "infections") %>% filter(Province_State=="South Carolina")


# clean up column names and differentiate between counties in SC Upstate
colnames(covid) = c("county","state","country","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

locations = read_csv("sources/SCcountylist.csv")
covid <- covid %>% right_join(locations) 
# covid$location[is.na(covid$location)] = "Other"

# total spread of infections by counties
spread <- covid %>% group_by(date, county, time) %>% summarise(count=sum(infections))

spread_total <- covid %>% group_by(date, time) %>% summarise(count=sum(infections))
spread_total$county = "Upstate"

max_x = ceiling(max(spread$time)/10)*10
capt = paste("Source: JHU\nlast updated:", lastupdated)

spread %>% ggplot + aes(time, count, color=county) + geom_point() + geom_line() +
                        scale_x_continuous(limits=c(50,max_x)) + #scale_y_log10() + 
                        labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Number of cases per county") + ggtitle("Spread of COVID19 in SC by county") + 
                        #annotate("text", x=175,y=20000, label="SC Upstate total") +
                        #annotate("text", x=175,y=10000, label="Greenville") +
                        #annotate("text", x=175,y=4000, label="Spartanburg") +  
                        facet_wrap(.~county, ncol=5)
                        #geom_line(data=spread_total) +


ggsave("graphs/covid-sc-spread.pdf", device="pdf")
write_csv(spread, "data/covid-sc-spread.csv")