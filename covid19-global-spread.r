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


# process the time series into proper dataframe
covid <- covid %>% pivot_longer(!c("Province/State","Country/Region", "Lat","Long"), 
   names_to = "date",
   values_to = "infections")


# clean up column names 
colnames(covid) = c("province","region","lat","long","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

# location assigments
locations = read_csv("sources/countrylist.csv")
covid <- covid %>% left_join(locations) 
covid$location[is.na(covid$location)] = "Other"


# US reference line
time = c(54) # correct for 3/15 US count = day 54
count = c(3499) # correct for 3/15 US count
location = "US Reference Line"
usdop = tibble(time, count, location)

# total spread of infections by countries
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(infections)) %>% arrange(location, time)

spread$count[spread$count==0] = 1e-1

maxtime = max(spread$time)

# curve fitting Other first 8-30 days

location = "Other"
time_start = 8
time_stop = 30

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note = exponential_fit_rate(fit)


# curve fitting Other pre 0-8 days

location = "Other"
time_start = 0
time_stop = 7

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred2 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note2 = exponential_fit_rate(fit)

# curve fitting Other post 40-56 days

location = "Other"
time_start = 40
time_stop = 56

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred3 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note3 = exponential_fit_rate(fit)
otherdata <- fitline(fit,time_start, time_stop)


# effect of Other reference line
location = "Other"
time_start = maxtime - 5
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
Note_Other = exponential_fit_rate(fit)
other_pred <- fitline(fit, time_start, time_stop)


# curve fitting China first 9 days
location = "China"
time_start = 0
time_stop = 10

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred4 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note4 = exponential_fit_rate(fit)

# curve fitting Italy past 39-56 days
location = "Italy"
time_start = 39
time_stop = 56

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred5 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note5 = exponential_fit_rate(fit)
itdata <- fitline(fit,time_start, time_stop)

# curve fitting Italy last 5 days of data
location = "Italy"
time_start = maxtime - 5 
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_IT2 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_IT2 = exponential_fit_rate(fit)
it2data <- fitline(fit,time_start, time_stop)


# curve fitting USA past 41-58 days

location = "USA"
time_start = 41
time_stop = 58

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred7 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note7 = exponential_fit_rate(fit)

# curve fitting USA past 58-66 days

location = "USA"
time_start = 57
time_stop = 66

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_US2 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_US2 = exponential_fit_rate(fit)


# fitting USA data last 5 days
location = "USA"
time_start = maxtime - 5
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
Note_US3 = exponential_fit_rate(fit)
usdop_pred <- fitline(fit, time_start, time_stop)


# curve fitting NL 38-47 days

location = "NL"
time_start = 38
time_stop = 47

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred8 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note8 = exponential_fit_rate(fit)

# curve fitting NL 44-56 days

location = "NL"
time_start = 44
time_stop = 56

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred9 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note9 = exponential_fit_rate(fit)
nldata = fitline(fit, time_start, time_stop)


# curve fit NL last 5 days
location = "NL"
time_start = maxtime -5
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_NL3 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_NL3 = exponential_fit_rate(fit)
nldata = fitline(fit, time_start, time_stop)

# Wave 3 extrapolation
location = "Wave 3"
time_start = maxtime - 5
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
Note_W3 = exponential_fit_rate(fit)
W3_pred <- fitline(fit, time_start, time_stop)


#
# clean up graphs and plot
#

capt = paste("Source: JHU\nlast updated:", lastupdated)

spread %>% ggplot + aes(time, count, color=location) + geom_point()  + 
                                        scale_y_log10(limit=c(1,1e7)) + scale_x_continuous() + labs(caption=capt) + 
                                        xlab("Days since Jan 22, 2020") + ylab("Infections") + ggtitle("Spread of COVID-19 infections, with calculated days to double") +
                                        # China data
                                        annotate("text",x=20,y=20000,label="China", color="red") + 
                                            geom_line(data=spreadpred4, color="red", linetype="longdash") + annotate("text", , color="red", x = 5, y = 10000, label = Note4) +                                                
                                        # Other countries
                                        annotate("text",x=20,y=900,label="Other", color="aquamarine4")+
                                            geom_line(data=spreadpred, color="aquamarine4", linetype="longdash") + annotate("text", color="aquamarine4", x = 20, y = 200, label = Note) +
                                            geom_line(data=spreadpred2, color="aquamarine4", linetype="longdash") + annotate("text", color="aquamarine4",x = 5, y = 20, label = Note2) + 
                                            geom_line(data=spreadpred3, color="aquamarine4", linetype="longdash") + annotate("text", color="aquamarine4",x = 45, y = 22000, label = Note3) +
                                            geom_line(data=other_pred, color="aquamarine") +
                                        # Italy data
                                        annotate("text",x=45,y=1500,label="Italy", color="dark green") + 
                                            geom_line(data=spreadpred5, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 45, y = 2200, label = Note5) +
                                            geom_line(data=spreadpred_IT2, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 115, y = 150000, label = Note_IT2) +
                                            geom_line(data=it2data, color="dark green") +
                                        # USA data
                                        annotate("text",x=42,y=400,label="USA", color="purple") + 
                                            geom_line(data=spreadpred7, color="purple", linetype="longdash") + annotate("text", color="purple", x = 42, y = 300, label = Note7) +
                                            geom_line(data=spreadpred_US2, color="purple", linetype="longdash") + annotate("text", color="purple", x = 60, y = 10000, label = Note_US2) +
                                            geom_line(data=usdop_pred) + annotate("text", color="purple", x = 75, y = 300000, label = Note_US3) +
                                            geom_point(data=usdop, color="purple", size=3) +
                                        # Wave 3 data
                                            geom_line(data=W3_pred, color="purple") + annotate("text", color="purple", x = 115, y = 500000, label = Note_W3) +

                                        # NL data
                                        annotate("text",x=40,y=5,label="NL", color="dark green") +
                                            geom_line(data=spreadpred8, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 42, y = 10, label = Note8) +
                                            geom_line(data=spreadpred9, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 50, y = 200, label = Note9) +
                                            geom_line(data=nldata, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 115, y = 30000, label = Note_NL3) 


filename_base = "covid-global-spread"

datafilename = paste("data/", filename_base, ".csv", sep="")
graphfilename = paste("graphs/", filename_base, ".pdf", sep="")

ggsave(graphfilename, device="pdf")
write_csv(spread, datafilename)