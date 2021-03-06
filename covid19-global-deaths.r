#
# R script to analyze deaths due to COVID-19, data from Johns Hopkids via github
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
# Data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
covid <- read_csv(covidfile)

# process the time series into proper dataframe
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

covid$deaths[is.na(covid$deaths)] = 0

# total spread of deaths
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(deaths)) %>% arrange(location, time)

spread$count[spread$count==0] = 1e-1

maxtime = max(spread$time)

# curve fitting starting at 22 days, Other

location = "Other"
time_start = 22
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note = exponential_fit_rate(fit)

# curve fitting China first 9 days

location = "China"
time_start = 0
time_stop = 10

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred4 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note4 = exponential_fit_rate(fit)

# curve fitting China first 15-33 days, linear fit - did not write a separate function for this

timeoffset = 15
timeoff2 = 33
fit = lm(spread$count[spread$location=="China" & spread$time > timeoffset & spread$time < timeoff2] ~ spread$time[spread$location=="China" & spread$time > timeoffset & spread$time < timeoff2])
A = fit$coefficients[1]
B = fit$coefficients[2]
Note5 = paste("linear rate\n",as.character(round(B,2)), "/day")
time = spread$time[spread$location=="China" & spread$time > timeoffset & spread$time < timeoff2]
prediction <- predict(fit, list=time)
spreadpred5 = tibble(time)
spreadpred5$count = prediction
spreadpred5$location = "Prediction"

# curve fitting China first 35-40 days, linear fit - did not write a separate function for this

timeoffset = 35
timeoff2 = 50
fit = lm(spread$count[spread$location=="China" & spread$time > timeoffset & spread$time < timeoff2] ~ spread$time[spread$location=="China" & spread$time > timeoffset & spread$time < timeoff2])
A = fit$coefficients[1]
B = fit$coefficients[2]
Note6 = paste("linear rate\n",as.character(round(B,2)), "/day")
time = spread$time[spread$location=="China" & spread$time > timeoffset & spread$time < timeoff2]
prediction <- predict(fit, list=time)
spreadpred6 = tibble(time)
spreadpred6$count = prediction
spreadpred6$location = "Prediction"

# curve fitting Italy from day 34-50

location = "Italy"
time_start = 33
time_stop = 50

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred7 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note7 = exponential_fit_rate(fit)

# curve fitting Italy from day 51-60

location = "Italy"
time_start = 51
time_stop = 60

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred10 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note10 = exponential_fit_rate(fit)

# curve fitting Italy last 5 days
location = "Italy"
time_start = maxtime - 5 
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_IT1 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_IT1 = exponential_fit_rate(fit)
IT1_data = fitline(fit, time_start, time_stop)

# curve fitting US from day 42-58
location = "USA"
time_start = 42
time_stop = 58

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred8 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note8 = exponential_fit_rate(fit)

# curve fitting US from day 57-70
location = "USA"
time_start = 57
time_stop = 70

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_US2 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_US2 = exponential_fit_rate(fit)


# curve fit USA last 5 days
location = "USA"
time_start = maxtime - 5
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
Note_US3 = exponential_fit_rate(fit)
US3_data = fitline(fit, time_start, time_stop)

# curve fitting NL from day 50-60
location = "NL"
time_start = 50
time_stop = 60

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred9 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note9 = exponential_fit_rate(fit)

# curve fitting NL last 5 days
location = "NL"
time_start = maxtime -5 
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_NL1 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_NL1 = exponential_fit_rate(fit)
NL1_data = fitline(fit, time_start, time_stop)

# curve fitting Wave 3 from extrapolate from t-5 days
location = "Wave 3"
time_start = maxtime -5 
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_NL1 <- exponential_fit_prediction(spread,fit, location, time_start,time_stop)
Note_BR1 = exponential_fit_rate(fit)
BR1_data = fitline(fit, time_start, time_stop)

#
# graph generation
#

capt = paste("Source: JHU\nlast updated:", lastupdated)

spread %>%   ggplot + aes(time, count, color=location) + geom_point()  + 
                scale_x_continuous() + scale_y_log10(limits=c(1,1e6)) + 
                labs(caption=capt) + xlab("Days since Jan 22, 2020") + ylab("Mortality") + ggtitle("Spread of COVID19 deaths, with calculated days to double") +
                # China data and fits                                
                    geom_line(data=spreadpred4, color="red", linetype="longdash") + annotate("text", x = 12, y = 100, color="red", label = paste("China\n",Note4)) +                                
                    geom_line(data=spreadpred5, color="red", linetype="longdash") + annotate("text", x = 20, y = 2000, label = Note5) +
                    geom_line(data=spreadpred6, color="red", linetype="longdash") + annotate("text", x = 38, y = 2000, label = Note6) +
                # USA data
                    geom_line(data=spreadpred8, color="purple", linetype="longdash") + annotate("text", color="purple", x = 45, y = 8, label = paste("USA\n",Note8)) +
                    geom_line(data=spreadpred_US2, color="purple", linetype="longdash") + annotate("text", color="purple", x = 58, y = 400, label = Note_US2) +
                    #geom_line(data=US2_data, color="purple") +
                    geom_line(data=US3_data, color="purple") + annotate("text", color="purple", x = 200, y = 100000, label = Note_US3) +
                # Other data
                    annotate("text",x=25,y=10,label="Other", color="blue")+
                    #geom_line(data=spreadpred, color="blue", linetype="longdash") + annotate("text", color="blue", x = 30, y = 50, label = Note) +
                # Italy data
                    geom_line(data=spreadpred7, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 45, y = 80, label = paste("Italy\n", Note7)) +
                    geom_line(data=spreadpred10, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 55, y = 1000, label = Note10)+
                    geom_line(data=spreadpred_IT1, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 180, y = 50000, label = Note_IT1)+
                    geom_line(data=IT1_data, color="dark green") +
                # Wave 3 data
                    geom_line(data=BR1_data, color="pink") + annotate("text", color="pink", x = 120, y = 60000, label = Note_BR1) + 
                # NL data
                    geom_line(data=spreadpred9, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 55, y = 5, label = paste("NL\n",Note9)) +
                    geom_line(data=spreadpred_NL1, color="dark green", linetype="longdash") + annotate("text", color="dark green", x = 170, y = 8000, label = paste("",Note_NL1)) +
                    geom_line(data=NL1_data, color="dark green") 

ggsave("graphs/covid-global-deaths.pdf", device="pdf")                                                 
write_csv(spread, "data/covid-global-deaths.csv")