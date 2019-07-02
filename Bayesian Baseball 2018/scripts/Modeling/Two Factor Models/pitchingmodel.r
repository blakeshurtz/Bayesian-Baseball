###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
library(tidyverse)
###
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m5 <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    #likelihood function
    mu <- A + C,
    #varying intercepts
    A <- h[home] + a[away],
    #pitching predictors
    C <- hits_allowed_h[home] * HHAh + pballs_h[home] * PBBh + pstrikeouts_h[home] * PSOh + strikes_h[home] * Strh + 
         hits_allowed_a[away] * HHAa + pballs_a[away] * PBBa + pstrikeouts_a[away] * PSOa + strikes_a[away] * Stra,
    ###adaptive priors
    c(h, hits_allowed_h, pballs_h, pstrikeouts_h, strikes_h)[home] ~ dmvnormNC(sigma_home,Rho_home),
    c(a, hits_allowed_a, pballs_a, pstrikeouts_a, strikes_a)[away] ~ dmvnormNC(sigma_away,Rho_away),
    ###priors
    sigma_home ~ dcauchy(0,2),
    sigma_away ~ dcauchy(0,2),
    Rho_home ~ dlkjcorr(4),
    Rho_away ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)),
  data=newdata2,
  iter=5000 , warmup=2000 , chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
