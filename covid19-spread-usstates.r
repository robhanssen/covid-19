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

covid_us <- covid %>% filter(region == "US")

covid_us$location[covid_us$province == "Washington"] = "Washington"
covid_us$location[covid_us$province == "New York"] = "New York"
covid_us$location[covid_us$province == "California"] = "California"
covid_us$location[covid_us$province == "New Jersey"] = "New Jersey"
covid_us$location[is.na(covid_us$location)] = "Other"

spread <- covid_us %>% group_by(time, location) %>% summarise(count=sum(infections))

# curve fitting first 49 days

location = "Other"
time_start = 49
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note = exponention_fit_rate(fit)

# curve fitting first 49 days

location = "New York"
time_start = 55
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_NY1 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note_NY1 = exponention_fit_rate(fit)

# curve fitting first 49 days

location = "California"
time_start = 49
time_stop = infinite

fit = exponential_fit(spread,location,time_start,time_stop)
spreadpred_CA1 <- exponention_fit_prediction(spread,fit, location, time_start,time_stop)
Note_CA1 = exponention_fit_rate(fit)



#
# clean up graphs and plot
#

capt = paste("Source: JHU\nlast updated:", lastupdated)

xmin=40
xmax=ceiling(max(spread$time)/10)*10 + 10

spread %>% filter(location != "South Korea" & location !="Spain" & location != "Iran") %>% 
                                    ggplot + aes(time, count, color=location) + geom_point()  + 
                                        scale_y_log10(limit=c(1e2,1e5)) + scale_x_continuous(limit=c(xmin,xmax)) + labs(caption=capt) + 
                                        xlab("Days since Jan 22, 2020") + ylab("Infections") + ggtitle("Spread of COVID-19 infections, with calculated days to double") +
                                        # Other states data
                                        annotate("text",x=55,y=6000,label="Other", color="blue") + 
                                            geom_line(data=spreadpred, color="blue", linetype="longdash") + annotate("text", color="blue", x = 55, y = 4000, label = Note) +
                                        # NY data
                                        annotate("text",x=59,y=3000,label="NY", color="darkgreen") + 
                                            geom_line(data=spreadpred_NY1, color="darkgreen", linetype="longdash") + annotate("text", color="darkgreen", x = 59, y = 4000, label = Note_NY1) +
                                        # CA data
                                        annotate("text",x=52,y=200,label="CA", color="darkorange") + 
                                            geom_line(data=spreadpred_CA1, color="darkorange", linetype="longdash") + annotate("text", color="darkorange", x = 52, y = 150, label = Note_CA1)

filename_base = paste("covid-spread-us-", lastupdated, ".", sep="")

datafilename = paste("data/", filename_base, "csv", sep="")
graphfilename = paste("graphs/", filename_base, "pdf", sep="")

ggsave(graphfilename, device="pdf")
write_csv(spread, datafilename)


