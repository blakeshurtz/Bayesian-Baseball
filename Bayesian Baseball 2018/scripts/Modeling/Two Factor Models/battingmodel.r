###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
library(tidyverse)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m2 <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    mu <- A + B,
    A <- h[home] + a[away],
    B <- hits_h[home] * Hh + double_h[home] * B2Bh + triple_h[home] * B3Bh + HR_h[home] * BHRh + balls_h[home] * BBBh +
         hits_a[away] * Ha + double_a[away] * B2Ba + triple_a[away] * B3Ba + HR_a[away] * BHRa + balls_a[away] * BBBa,
    ###adaptive priors
    c(h, hits_h, double_h, triple_h, HR_h, balls_h)[home] ~ dmvnormNC(sigma_home,Rho_home),
    c(a, hits_a, double_a, triple_a, HR_a, balls_a)[away] ~ dmvnormNC(sigma_away,Rho_away),
    ###
    sigma_home ~ dcauchy(0,2),
    sigma_away ~ dcauchy(0,2),
    Rho_home ~ dlkjcorr(4),
    Rho_away ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)),
   data=newdata2,
   iter=5000 , warmup=2000 , chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
