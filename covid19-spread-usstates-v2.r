#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

# still in development - do not use for analysis

#
# load the required libraries
#
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(data.table)
library(tidyverse)
library(data.table)
#install.packages(c("data.table"))
# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API

filebase = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
date_start = as.Date("3/23/2020", format="%m/%d/%Y")
date_stop = today()-1
date_range = format(as.Date(date_start:date_stop, origin="1970-01-01"), "%m-%d-%Y")
filelist = paste(filebase, date_range, ".csv", sep="")
tbl_fread <- filelist %>%  map_df(~fread(.))

colnames(tbl_fread) = c("FIPS", "Admin2", "province", "country", "lastupdated", "lat", "long", "confirmed", "deaths", "recovered","active", "junk")

data_us <- tbl_fread %>% filter(country=="US") %>% group_by(lastupdated, province) %>% summarise(infections=sum(confirmed), casualties=sum(deaths))

data_us$date = as.Date(data_us$lastupdated, format="%Y-%m-%d")

data_us$state = NA
data_us$state[data_us$province=="New York"] = "NY"
data_us$state[data_us$province=="California"] = "CA"
data_us$state[data_us$province=="New Jersey"] = "NJ"
data_us$state[data_us$province=="Washington"] = "WA"
data_us$state[data_us$province=="South Carolina"] = "SC"
data_us$state[is.na(data_us$state)] = "Other"

data_us %>%  group_by(date, state) %>% 
                summarise(infectionrate = sum(infections), casualtyrate = sum(casualties)) %>% 
                filter(state!="O_ther") %>% 
                ggplot + aes(date, infectionrate, color=state) + geom_point() + geom_line() +
                        scale_y_log10()
                                         

ggsave("graphs/us-states.pdf", device="pdf")
write_csv(data_us, "data/us-states.csv")