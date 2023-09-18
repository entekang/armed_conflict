library(tidyverse)

d <- read.csv("data/maternalmortality.csv")
d <- d %>% select(c(Country.Name, X2000:X2019))
colnames(d)[-1] <- gsub("^X", "", colnames(d)[-1])

d2 <- d %>% pivot_longer(cols = starts_with("2"), names_to = "Year", values_to = "MatMor")
d2$Year <- as.numeric(d2$Year)
