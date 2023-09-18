library(tidyverse)


d <- read.csv("original/disaster.csv")
d2 <- d %>% filter(Year %in% seq(2000, 2019)) %>% filter(Disaster.Type %in% c("Earthquake", "Drought"))
d2 <- d2 %>% select(c(Year, ISO, Disaster.Type))
d2$drought <- ifelse(d2$Disaster.Type == "Drought", 1, 0)
d2$earthquake <- ifelse(d2$Disaster.Type == "Earthquake", 1, 0)

d3 <- d2 %>% group_by(Year, ISO) %>% summarize(drought = ifelse(sum(drought) >=1, 1, 0), earthquake = ifelse(sum(earthquake)>=1, 1, 0))
