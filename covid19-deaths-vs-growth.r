library(tidyverse)

growth <- read_csv("data/covid-casualty-growth.csv")
spread <- read_csv("data/covid-deaths.csv")

spreadgrowth <- spread %>% inner_join(growth)

spreadgrowth %>% ggplot + aes(count, growth, color=location) + geom_point() + geom_smooth(method="loess") + scale_x_log10(limit=c(1,1e4)) + scale_y_log10()

ggsave("graphs/covid19-deaths-vs-growth.pdf", device="pdf")
write_csv(spreadgrowth, "data/covid19-deathsvsgrowth.csv")