
library(tidyverse)

df <- read.csv("data/original/finaldata.csv")

df2010_17 <- df %>% filter(year %in% seq(2010, 2017))
earth_drought <- df2010_17 %>% group_by(country_name) %>% summarize(total_earth = sum(earthquake), 
                                                   total_drought = sum(drought))

# countries that have had earthquakes or droughts between 2010-2017
earthcountry <- earth_drought %>% filter(total_earth >=1) %>% select(country_name)
droughtcountry <- earth_drought %>% filter(total_drought >=1) %>% select(country_name)

df$E <- rep(0, nrow(df))
df$E[df$country_name %in% earthcountry$country_name] <- 1

df$D <- rep(0, nrow(df))
df$D[df$country_name %in% droughtcountry$country_name] <- 1

# data for model
df2 <- df %>% filter(year == 2019) # since need armconf in 2018, which is lagged one year 

mod <- glm(armconf1~E+D, family = "binomial", data = df2)
summary(mod)

#### quiz ####

#2
earth_drought %>% filter(total_earth!=0 & total_drought!=0)
# or use b <- droughtcountry %>% inner_join(earthcountry)

#4
exp(-2.0463) / (1 + exp(-2.0463))
