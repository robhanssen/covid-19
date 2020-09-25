#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(tidyverse)
library(lubridate)

source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_, -Population)

# process the time series into proper dataframe
# covid <- melt(covid, id=c("Admin2", "Province_State","Country_Region"))
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
   names_to = "date",
   values_to = "deaths")


# clean up column names and differentiate between different regions
colnames(covid) = c("county","state","country","date","deaths")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

# selection of states to highlight out of the full data set
locations = read_csv("sources/USstateslist.csv")
covid <- covid %>% left_join(locations) 
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by states
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(deaths))

max_x = ceiling(max(spread$time)/10)*10
capt = paste("Source: JHU\nlast updated:", lastupdated)



spread %>% ggplot + aes(time, count, color=location) + geom_point() + geom_line() +
                        scale_x_continuous(limit=c(40, max_x)) + scale_y_log10(limit=c(1e1,1e6)) + labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Number of casualties per state") + ggtitle("COVID19 casualties in the US by state") + 
                        annotate("text", x=62,y=200, label="NY") +
                        annotate("text", x=60,y=300, label="Other") +
                        annotate("text", x=65,y=150, label="NJ") +
                        annotate("text", x=66,y=20, label="LA") +
                        annotate("text", x=66,y=70, label="MI") +
                        annotate("text", x=60,y=30, label="CA") 


ggsave("graphs/covid-us-deaths.pdf", device="pdf")
write_csv(spread, "data/covid-us-deaths.csv")