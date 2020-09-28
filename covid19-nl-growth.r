#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(tidyverse)
library(zoo)

deaths <- read_csv("data/covid-global-casualty-growth.csv") %>% filter(location == "NL")
cases <- read_csv("data/covid-global-growth.csv") %>% filter(location == "NL")

colnames(deaths) = c("time", "location", "deaths")
colnames(cases) = c("time", "location", "cases")

casesdeaths <- deaths %>% inner_join(cases)

lastupdated = as.Date("2020-01-21", format="%Y-%m-%d") + max(casesdeaths$time) 
casesdeaths$date = as.Date(casesdeaths$time + as.Date("2020-01-21", format="%Y-%m-%d"), format="%Y-%m-%d")
capt = paste("Source: JHU\nlast updated:", lastupdated)

correction = 7
avdays = 7

totalcases = sum(casesdeaths$cases)
totaldeaths  = sum(casesdeaths$deaths)

casesdeaths %>% ggplot + aes(date, cases) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,500,100))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="1 month", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("NL daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deaths), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), color="red") +
                        annotate("text",x=as.Date("2020-03-09", format="%Y-%m-%d"),y=800,label="cases\n<-----", color="blue") + 
                        annotate("text",x=as.Date("2020-04-05", format="%Y-%m-%d"),y=250,label="deaths\n------>", color="red") +
                        annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=2400,label=paste("Total cases:", format(totalcases, big.mark=" ")), color="blue") + 
                        annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=2200,label=paste("Total deaths:", format(totaldeaths, big.mark=" ")), color="red") 

ggsave("graphs/covid-nl-daily-cases-and-deaths.pdf", device="pdf")
write_csv(casesdeaths, "data/covid-nl-daily-cases-and-deaths.csv")
