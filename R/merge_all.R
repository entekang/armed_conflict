library(tidyverse)
library(countrycode)

source("R/cleaner.R")
source("R/conflict.R")

infant <- read.csv("data/original/infantmortality.csv")  
neonatal <- read.csv("data/original/neonatalmortality.csv") 
under5 <- read.csv("data/original/under5mortality.csv")
maternal <- read.csv("data/original/maternalmortality.csv")
cov <- read.csv("data/original/covariates.csv")
disaster <- read.csv("data/original/disaster_new.csv")[, -c(1)]
conflict <- read.csv("data/original/conflictdata.csv")

mort_data <- merge_mortdata(infant, neonatal, under5, maternal)
conf_clean <- create_armconf(conflict)
conf_clean <- subset(conf_clean, select = -c(n))


final_data <- mort_data %>% left_join(disaster, by = c("ISO", "Year")) %>% 
  left_join(conf_clean, by= join_by(ISO==ISO, Year==year)) %>% 
  inner_join(cov, by = join_by(ISO==ISO, Year==year)) 

final_data$drought[is.na(final_data$drought)] <- 0
final_data$earthquake[is.na(final_data$earthquake)] <- 0
final_data$armed_conflict[is.na(final_data$armed_conflict)] <- 0


write.csv(final_data, file = "data/original/finalmerged_data.csv")
