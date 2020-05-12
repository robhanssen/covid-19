library(tidyverse)

spread <- read_csv("data/covid-spread.csv")
growth <- read_csv("data/covid-growth.csv")

spreadgrowth <- spread %>% inner_join(growth)

lastupdated = as.Date("2020-01-21", format="%Y-%m-%d") + max(spreadgrowth$time) 

capt = paste("Source: JHU\nlast updated:", lastupdated)

spreadgrowth %>% ggplot + aes(count, growth, color=location) + geom_point() + geom_smooth(method="loess") + 
                        scale_x_log10(limits=c(1e2,1e7)) + scale_y_log10(limits=c(1e2,1e5)) + 
                        labs(caption=capt) + xlab("Cumulative confirmed cases") + ylab("Daily incremental number of confirmed cases") +
                        ggtitle("Growth of confirmed cases by of existing confirmed cases")


ggsave("graphs/covid19-spread-vs-growth.pdf", device="pdf")
write_csv(spreadgrowth, "data/covid19-spreadvsgrowth.csv")