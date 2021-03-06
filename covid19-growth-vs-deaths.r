library(tidyverse)

growth <- read_csv("data/covid-global-casualty-growth.csv")
spread <- read_csv("data/covid-global-deaths.csv")

spreadgrowth <- spread %>% inner_join(growth)

lastupdated = as.Date("2020-01-21", format="%Y-%m-%d") + max(spreadgrowth$time) 

capt = paste("Source: JHU\nlast updated:", lastupdated)

spreadgrowth %>% ggplot + aes(count, growth, color=location) + geom_point() + geom_smooth(method="loess") + 
                        scale_x_log10(limits=c(1e0,1e6)) + scale_y_log10() + 
                        labs(caption=capt) + xlab("Cumulative casualties") + ylab("Daily incremental number of casualties") +
                        ggtitle("Growth of casualties by of existing casualties")

ggsave("graphs/covid-global-deaths-vs-growth.pdf", device="pdf")
write_csv(spreadgrowth, "data/covid-global-deathsvsgrowth.csv")