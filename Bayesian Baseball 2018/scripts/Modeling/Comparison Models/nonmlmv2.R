###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
library(tidyverse)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
###
m2 <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    mu <- int +
      a + h + 
      hits_h * Hh + double_h * B2Bh + triple_h * B3Bh + HR_h * BHRh + balls_h * BBBh +
      hits_a * Ha + double_a * B2Ba + triple_a * B3Ba + HR_a * BHRa + balls_a * BBBa +
      hits_allowed_h * HHAh + pballs_h * PBBh + pstrikeouts_h * PSOh +  strikes_h * Strh +
      hits_allowed_a * HHAa + pballs_a * PBBa + pstrikeouts_a * PSOa + strikes_a * Stra +
      fh1 * PO_h + fh2 * A_h + fh3 * E_h + fh4 * DP_h + fa1 * PO_a + fa2 * A_a + fa3 * E_a + fa4 * DP_a,
    
    ###adaptive priors for coefficients
    int ~ dnorm(0,3),
    h ~ dnorm(0, 3),
    a ~ dnorm(0, 3),
    ###
    hits_h ~ dnorm(0, 3),
    hits_a ~ dnorm(0, 3),
    ###
    double_h ~ dnorm(0, 3),
    double_a ~ dnorm(0, 3),
    ###
    triple_h ~ dnorm(0, 3),
    triple_a ~ dnorm(0, 3),
    ###
    HR_h ~ dnorm(0, 3),
    HR_a ~ dnorm(0, 3),
    ###
    balls_h ~ dnorm(0, 3),
    balls_a ~ dnorm(0, 3),
    ###
    hits_allowed_h ~ dnorm(0, 3),
    hits_allowed_a ~ dnorm(0, 3),
    ###
    pballs_h ~ dnorm(0, 3),
    pballs_a ~ dnorm(0, 3),
    ###
    pstrikeouts_h ~ dnorm(0, 3),
    pstrikeouts_a ~ dnorm(0, 3),
    ###
    strikes_h ~ dnorm(0, 3),
    strikes_a ~ dnorm(0, 3),
    ###standard priors
    c(fh1, fh2, fh3, fh4, fa1, fa2, fa3, fa4) ~ dnorm(0,1),
    sigma ~ dcauchy(0,2)),
  data=mydata2,
  iter=1000, warmup=100, chains=1, cores=4)

#option
#,control = list(adapt_delta = 0.99, max_treedepth = 15)
