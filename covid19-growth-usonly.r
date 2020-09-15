#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(tidyverse)
library(zoo)

deaths <- read_csv("data/covid-global-casualty-growth.csv") %>% filter(location == "USA")
cases <- read_csv("data/covid-global-growth.csv") %>% filter(location == "USA")

colnames(deaths) = c("time", "location", "deaths")
colnames(cases) = c("time", "location", "cases")

casesdeaths <- deaths %>% inner_join(cases)

lastupdated = as.Date("2020-01-21", format="%Y-%m-%d") + max(casesdeaths$time) 
casesdeaths$date = as.Date(casesdeaths$time + as.Date("2020-01-21", format="%Y-%m-%d"), format="%Y-%m-%d")
capt = paste("Source: JHU\nlast updated:", lastupdated)

correction = 14.5
avdays = 7

casesdeaths %>% ggplot + aes(date, cases) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction)) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="1 month", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date in 2020") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("US daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deaths), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), color="red") +
                        annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=20000,label="cases\n<-----", color="blue") + 
                        annotate("text",x=as.Date("2020-04-10", format="%Y-%m-%d"),y=10000,label="deaths\n------>", color="red") 

ggsave("graphs/covid-us-daily-cases-and-deaths.pdf", device="pdf")
write_csv(casesdeaths, "data/covid-us-daily-cases-and-deaths.csv")
