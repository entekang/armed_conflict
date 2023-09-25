
conf <- read.csv("data/original/conflictdata.csv")
conf$armed_conflict <- ifelse(conf$best >= 25, 1, 0)
conf <- subset(conf, select = -c(best))

create_armconf <- function(df){
  d <- df %>% group_by(ISO, year) %>% summarize(n = sum(best))
  d$armed_conflict <- ifelse(d$n >= 25, 1, 0)
  return(d)
}



# trial <- conflict %>% group_by(ISO, year) %>% summarize(n = sum(best))
