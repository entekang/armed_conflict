library(tidyverse)
df <- read.csv("data/original/finaldata.csv")

ac <- df |>
  dplyr::filter(year == 2019) |>
  dplyr::select(ISO, armconf1)

subdata <- df |>
  dplyr::filter(2010 <= year & year < 2018) |>
  dplyr::group_by(ISO) |>
  summarise(ever_dr = max(drought), ever_eq = max(earthquake)) |>
  left_join(ac, by = "ISO")



negll <- function(par){
  y <- subdata$armconf1
  x1 <- subdata$ever_eq
  x2 <- subdata$ever_dr
  # 1. Calculate xbeta
  xbeta <- par[1] + par[2] * x1 + par[3] *x2
  # 2. Calculate p
  p <- exp(xbeta) / (1 + exp(xbeta))
  # 3. Calculate negative log-likelihood
  val <- -sum(y * log(p) + (1 - y) * log(1 - p))
  return(val)
}


library(optimx)
opt <- optimx(
  par = c(0,0, 0),
  fn = negll,
  control = list(trace = 0, all.methods = TRUE)
)

summary(opt, order = "convcode")

hessian_m <- attributes(opt)$details["BFGS", "nhatend"][[1]]
fisher_info <- solve(hessian_m)
prop_se <- sqrt(diag(fisher_info))
prop_se



mod <- glm(armconf1~ever_dr+ever_eq, data = subdata, family = "binomial")
summary(mod)
