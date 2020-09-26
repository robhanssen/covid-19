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
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
covid <- read_csv(covidfile)

# process the time series into proper dataframe
# covid <- melt(covid, id=c("Province/State","Country/Region", "Lat","Long"))
covid <- covid %>% pivot_longer(!c("Province/State","Country/Region", "Lat","Long"), 
   names_to = "date",
   values_to = "deaths")

# clean up column names and differentiate between different regions
colnames(covid) = c("province","region","lat","long","date","deaths")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

# location assigments
locations = read_csv("sources/countrylist.csv")
covid <- covid %>% left_join(locations) 
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by countries
spread <- covid %>% group_by(time, location) %>% summarise(count=sum(deaths)) %>% arrange(location)

widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)


covid_growth <- covid_growth %>% pivot_longer(!c("time"),
   names_to = "location",
   values_to = "growth") %>% filter(location!="Other")

capt = paste("Source: JHU\nlast updated:", lastupdated)

covid_growth %>% ggplot + aes(time, growth, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +

                        scale_x_continuous() + labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Growth of casualties") + ggtitle("Per diem growth of COVID-19 casualties") + 
                        facet_wrap(.~location)

ggsave("graphs/covid-global-casualty-growth.pdf", device="pdf")
write_csv(covid_growth, "data/covid-global-casualty-growth.csv")