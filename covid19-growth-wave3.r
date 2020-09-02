#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(tidyverse)
library(zoo)

deaths <- read_csv("data/covid-global-casualty-growth.csv") %>% filter(location == "Wave 3")
cases <- read_csv("data/covid-global-growth.csv") %>% filter(location == "Wave 3")

colnames(deaths) = c("time", "location", "deaths")
colnames(cases) = c("time", "location", "cases")

casesdeaths <- deaths %>% inner_join(cases)

lastupdated = as.Date("2020-01-21", format="%Y-%m-%d") + max(casesdeaths$time) 

capt = paste("Source: JHU\nlast updated:", lastupdated)

correction = 30
avdays = 7

casesdeaths %>% ggplot + aes(time, cases) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction)) + #scale_y_log10(limit=c(10,100000))+ 
                        labs(caption=capt) + xlab("Days since Jan 22, 2020") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("Wave 3 daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(time, correction*deaths), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), color="red") +
                        annotate("text",x=80,y=20000,label="cases\n<-----", color="blue") + 
                        annotate("text",x=100,y=10000,label="------>\ndeaths", color="red") #+ facet_wrap(.~location)



ggsave("graphs/covid-wave3-daily-cases-and-deaths.pdf", device="pdf")
