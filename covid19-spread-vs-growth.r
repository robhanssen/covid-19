library(tidyverse)

spread <- read_csv("data/covid-spread.csv")
growth <- read_csv("data/covid-growth.csv")

spreadgrowth <- spread %>% inner_join(growth)

spreadgrowth %>% ggplot + aes(count, growth, color=location) + geom_point() + geom_smooth(method="loess") + scale_x_log10(limits=c(1e2,1e6)) + scale_y_log10(limits=c(1e2,1e5))

ggsave("graphs/covid19-spread-vs-growth.pdf", device="pdf")
write_csv(spreadgrowth, "data/covid19-spreadvsgrowth.csv")