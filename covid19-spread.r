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
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
covid <- read_csv(covidfile)

# process the time series into proper dataframe
covid <- melt(covid, id=c("Province/State","Country/Region", "Lat","Long"))

# clean up column names and differentiate between China/ex-China location
colnames(covid) = c("province","region","lat","long","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

# specifically added to deal with an issue in the data file on 20200308, delete when appropriate
#covid$infections[is.na(covid$infections)] = 0

covid$location[covid$region == "China"] = "China"
covid$location[covid$region == "Italy"] = "Italy"
covid$location[covid$region == "Korea, South"] = "South Korea"
covid$location[covid$region == "US"] = "USA"
covid$location[covid$region == "Netherlands"] = "NL"
#covid$location[covid$region == "Germany"] = "Germany"
covid$location[is.na(covid$location)] = "Other"


# US reference line
time = c(54) # correct for 3/15 US count = day 54
count = c(3499) # correct for 3/15 US count
location = "US Reference Line"
usdop = tibble(time, count, location)



# 3/17 error in datafile correction - delete when appropriate
covid$infections[is.na(covid$infections)] = 0

# total spread of infections by countries
spread <- covid %>% group_by(time, location) %>% summarise(count=sum(infections))

spread$count[spread$count==0] = 1e-5 

# curve fitting first 8-30 days

location = "Other"
time_start = 8
time_stop = 30

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note = exponention_fit_rate(fit)
#summary(fit)$r.squared

# curve fitting pre 0-8 days

location = "Other"
time_start = 0
time_stop = 7

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred2 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note2 = exponention_fit_rate(fit)
#summary(fit)$r.squared

# curve fitting post 31-47 days

location = "Other"
time_start = 35
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred3 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note3 = exponention_fit_rate(fit)
#summary(fit)$r.squared

# curve fitting China first 9 days

location = "China"
time_start = 0
time_stop = 10

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred4 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note4 = exponention_fit_rate(fit)
#summary(fit)$r.squared

# curve fitting Italy past 39-56 days

location = "Italy"
time_start = 39
time_stop = 56

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred5 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note5 = exponention_fit_rate(fit)
itdata <- fitline(fit,time_start, time_stop)
#itdata <- fitline(fit, time_start,time_stop)
#summary(fit)$r.squared

# curve fitting Italy past 39-56 days

location = "Italy"
time_start = 56
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_IT2 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note_IT2 = exponention_fit_rate(fit)
it2data <- fitline(fit,time_start, time_stop)
#itdata <- fitline(fit, time_start,time_stop)
#summary(fit)$r.squared



# curve fitting SKorea past 33 days

location = "South Korea"
time_start = 32
time_stop = 39

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred6 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note6 = exponention_fit_rate(fit)
#summary(fit)$r.squared

# curve fitting USA past 38 days

location = "USA"
time_start = 41
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred7 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
#usdata <- fitline(fit, time_start, time_stop)

Note7 = exponention_fit_rate(fit)


# effect of US reference line 3/15
location = "USA"
time_start = 41
time_stop = 54

fit = exponential_fit(spread,location,time_start,time_stop)
usdop_pred <- fitline(fit, time_start, time_stop)


#summary(fit)$r.squared

# curve fitting NL 38-47 days

location = "NL"
time_start = 38
time_stop = 47

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred8 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note8 = exponention_fit_rate(fit)
#summary(fit)$r.squared

# curve fitting NL post47 days

location = "NL"
time_start = 44
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred9 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note9 = exponention_fit_rate(fit)
nldata = fitline(fit, time_start, time_stop)
#summary(fit)$r.squared

#
# clean up graphs and plot
#

capt = paste("Source: JHU\nlast updated:", lastupdated)

spread %>% filter(location != "South Korea" & location !="Spain" & location != "Iran") %>% 
                                    ggplot + aes(time, count, color=location) + geom_point()  + 
                                        scale_y_log10(limit=c(1,1e6)) + scale_x_continuous() + labs(caption=capt) + 
                                        xlab("Days since Jan 22, 2020") + ylab("Infections") + ggtitle("Spread of COVID-19 infections, with calculated days to double") +
                                        # China data
                                        annotate("text",x=20,y=20000,label="China", color="red") + 
                                            geom_line(data=spreadpred4, color="red", linetype="longdash") + annotate("text", , color="red", x = 5, y = 10000, label = Note4) +                                                
                                        # Other countries
                                        annotate("text",x=20,y=900,label="Other", color="aquamarine4")+
                                            geom_line(data=spreadpred, color="aquamarine4", linetype="longdash") + annotate("text", color="aquamarine4", x = 20, y = 200, label = Note) +
                                            geom_line(data=spreadpred2, color="aquamarine4", linetype="longdash") + annotate("text", color="aquamarine4",x = 5, y = 20, label = Note2) + 
                                            geom_line(data=spreadpred3, color="aquamarine4", linetype="longdash") + annotate("text", color="aquamarine4",x = 45, y = 22000, label = Note3) +
                                        # Italy data
                                        annotate("text",x=45,y=1500,label="Italy", color="dark green") + 
                                            geom_line(data=spreadpred5, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 45, y = 2200, label = Note5) +
                                            geom_line(data=spreadpred_IT2, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 57, y = 30000, label = Note_IT2) +
                                            geom_line(data=it2data, color="dark green") +
                                        # USA data
                                        annotate("text",x=42,y=400,label="USA", color="purple") + 
                                            geom_line(data=spreadpred7, color="purple", linetype="longdash") + annotate("text", color="purple", x = 42, y = 300, label = Note7) +
                                            geom_line(data=usdop_pred) +
                                            geom_point(data=usdop, color="purple", size=3) +
                                        # NL data
                                        annotate("text",x=40,y=5,label="NL", color="dark green") +
                                            geom_line(data=spreadpred8, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 42, y = 10, label = Note8) +
                                            geom_line(data=spreadpred9, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 50, y = 200, label = Note9) #+
                                        # South Korea data
                                        #annotate("text",x=25,y=50,label="South Korea", color="blue") +
                                        #    geom_line(data=spreadpred6, color="blue", linetype="longdash") + annotate("text", color="blue", x = 32, y = 800, label = Note6) #+
                                        #geom_line(data=usdata, linetype="longdash", color="purple") #+ geom_line(data=itdata) + geom_line(data=nldata)


filename_base = paste("covid-spread-", lastupdated, ".", sep="")

datafilename = paste("data/", filename_base, "csv", sep="")
graphfilename = paste("graphs/", filename_base, "pdf", sep="")

ggsave(graphfilename, device="pdf")
write_csv(spread, datafilename)


# comparison to Italy (day-shifted to 100 cases)

shiftedspread = spread

shiftedspread$time[shiftedspread$location=="USA"] = shiftedspread$time[shiftedspread$location=="USA"] - 10
shiftedspread$time[shiftedspread$location=="Germany"] = shiftedspread$time[shiftedspread$location=="Germany"] - 9
shiftedspread$time[shiftedspread$location=="NL"] = shiftedspread$time[shiftedspread$location=="NL"] - 12
shiftedspread$time[shiftedspread$location=="Iran"] = shiftedspread$time[shiftedspread$location=="Iran"] - 3
shiftedspread$time[shiftedspread$location=="South Korea"] = shiftedspread$time[shiftedspread$location=="South Korea"] + 3
shiftedspread$time[shiftedspread$location=="UK"] = shiftedspread$time[shiftedspread$location=="UK"] - 12
shiftedspread$time[shiftedspread$location=="Other"] = shiftedspread$time[shiftedspread$location=="Other"] 
shiftedspread$time[shiftedspread$location=="China"] = shiftedspread$time[shiftedspread$location=="China"] + 36

# all shift -32 days
shiftedspread$time = shiftedspread$time - 32

shiftedspread %>% filter(location !="Other") %>% 
                                    ggplot + aes(time, count, color=location) + geom_line()  + 
                                        scale_y_log10(limit=c(1e2,1e5)) + 
                                        scale_x_continuous(limit=c(0,50)) + labs(caption=capt) + 
                                        xlab("Days since 100 cases") + ylab("Infections") + ggtitle("comparison to Italy, time-shifted to match 100 cases") #+

filename_base = paste("covid-spread-vs-at-100cases-", lastupdated, ".", sep="")

datafilename = paste("data/", filename_base, "csv", sep="")
graphfilename = paste("graphs/", filename_base, "pdf", sep="")

ggsave(graphfilename, device="pdf")