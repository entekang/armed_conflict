library(SimDesign)

Design <- createDesign(n = c(50, 500),
                       pz = c(0.2, 0.8),
                       alpha0 = c(-1, 0, 1), 
                       alpha1 = c(0, 0.5, 1))
Generate <- function(condition, fixed_objects = NULL) {
  N <- condition$n
  probz <- condition$pz
  a0 <- condition$alpha0
  a1 <- condition$alpha1
  
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  
  z <- rbinom(N, size=1, prob = probz)
  px <- exp(a0 + a1*z) / (1 + exp(a0 + a1*z))
  x <- rbinom(N, size=1, prob=px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z)) 
  y <- rbinom(N, size = 1, prob = py)
  
  dat <- data.frame(y,x,z)
  dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
  unadj.mod <- glm(y ~ x, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  adj.mod <- glm(y ~ x+z, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  unadj.p <- unadj.coef[2,4]
  adj.p <- adj.coef[2,4]
  return(c(unadj.p, adj.p))
}


Summarise <- function(condition, results, fixed_objects = NULL) {
  ret <- c(type1 = EDR(results))
  ret
}

res <- runSimulation(Design, replications=1000, verbose=FALSE, parallel=TRUE,
                     generate=Generate, 
                     analyse=Analyse, 
                     summarise=Summarise)

library(tidyverse)
ggplot(res, aes(x=factor(alpha1), y=type1.V1, colour = factor(pz)))+
  geom_point() +
  geom_line(aes(group = pz))+
  facet_grid(alpha0~n)


ggplot(res, aes(x=alpha1))+
  geom_line(aes(y=type1.V2, colour = "adjusted"))+
  geom_line(aes(y=type1.V1, colour = "unadjusted"))+
  facet_grid(alpha0~n)
