library(tidyverse)
library(countrycode)

cleaner <- function(df){
  d <- df %>% select(c(Country.Name, X2000:X2019))
  colnames(d)[-1] <- gsub("^X", "", colnames(d)[-1])
  name <- deparse(substitute(df))
  d <- d %>% pivot_longer(cols = starts_with("2"), names_to = "Year", values_to = "Mort") %>% 
    mutate(Year = as.numeric(Year))
  return(d)
}

# as.character(str_glue("{name}_mortality"))

infant <- read.csv("data/original/infantmortality.csv")  
neonatal <- read.csv("data/original/neonatalmortality.csv") 
under5 <- read.csv("data/original/under5mortality.csv")
maternal <- read.csv("data/original/maternalmortality.csv")

# dl <- list(maternal, infant, neonatal, under5)
# dl_clean <- lapply(dl, cleaner)
# 
# fulldata <- dl_clean %>% reduce(full_join, by = c("Country.Name", "Year"))
# colnames(fulldata) <- c("Country", "Year", "maternal_mort", "infant_mort", "neonatal_mort", "under5_mort")
# 
# full_clean <- fulldata %>% mutate(ISO = countrycode(Country, origin = 'country.name', destination = 'iso3c'))
# full_clean <- subset(full_clean, select = -c(Country))

merge_mortdata <- function(dinf, dneo, dunder, dmat){
  dl <- list(dinf, dneo, dunder, dmat)
  df_clean <- lapply(dl, cleaner)
  fulldata <- dl_clean %>% reduce(full_join, by = c("Country.Name", "Year"))
  colnames(fulldata) <- c("Country", "Year", "maternal_mort", "infant_mort", "neonatal_mort", "under5_mort")
  
  full_clean <- fulldata %>% mutate(ISO = countrycode(Country, origin = 'country.name', destination = 'iso3c'))
  full_clean <- subset(full_clean, select = -c(Country))
  return(full_clean)
}
  