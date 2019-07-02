###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
library(tidyverse)
###
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m3 <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    #likelihood function
    mu <- A + B + C,
    #varying intercepts
    A <- h[home] + a[away],
    #batting predictors
    B <- hits_h[home] * Hh + double_h[home] * B2Bh + triple_h[home] * B3Bh + HR_h[home] * BHRh + balls_h[home] * BBBh +
         hits_a[away] * Ha + double_a[away] * B2Ba + triple_a[away] * B3Ba + HR_a[away] * BHRa + balls_a[away] * BBBa,
    #pitching predictors
    C <- hits_allowed_h[home] * HHAh + pballs_h[home] * PBBh + pstrikeouts_h[home] * PSOh + strikes_h[home] * Strh + 
         hits_allowed_a[away] * HHAa + pballs_a[away] * PBBa + pstrikeouts_a[away] * PSOa + strikes_a[away] * Stra,
    ###adaptive priors
    c(h, hits_h, double_h, triple_h, HR_h, balls_h, hits_allowed_h, pballs_h, pstrikeouts_h, strikes_h)[home] ~ dmvnormNC(sigma_home,Rho_home),
    c(a, hits_a, double_a, triple_a, HR_a, balls_a, hits_allowed_a, pballs_a, pstrikeouts_a, strikes_a)[away] ~ dmvnormNC(sigma_away,Rho_away),
    ###priors
    sigma_home ~ dcauchy(0,2),
    sigma_away ~ dcauchy(0,2),
    Rho_home ~ dlkjcorr(4),
    Rho_away ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)),
  data=newdata2,
  iter=5000 , warmup=2000 , chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
