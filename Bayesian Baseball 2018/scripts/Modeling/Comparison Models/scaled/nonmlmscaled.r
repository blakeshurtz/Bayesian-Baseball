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
    mu <- A + B + C + D,
    A <- h + a,
    B <- b + hits_h * Hh + double_h * B2Bh + triple_h * B3Bh + HR_h * BHRh + balls_h * BBBh +
      hits_a * Ha + double_a * B2Ba + triple_a * B3Ba + HR_a * BHRa + balls_a * BBBa,
    C <- c + hits_allowed_h * HHAh + pballs_h * PBBh + pstrikeouts_h * PSOh +  strikes_h * Strh +
      hits_allowed_a * HHAa + pballs_a * PBBa + pstrikeouts_a * PSOa + strikes_a * Stra,
    D <- d + fh1 * PO_h + fh2 * A_h + fh3 * E_h + fh4 * DP_h + fa1 * PO_a + fa2 * A_a + fa3 * E_a + fa4 * DP_a,
    ###all priors normalized
    h ~ dnorm(0,1),
    a ~ dnorm(0,1),
    ###
    hits_h ~ dnorm(0,1),
    hits_a ~ dnorm(0,1),
    ###
    double_h ~ dnorm(0,1),
    double_a ~ dnorm(0,1),
    ###
    triple_h ~ dnorm(0,1),
    triple_a ~ dnorm(0,1),
    ###
    HR_h ~ dnorm(0,1),
    HR_a ~ dnorm(0,1),
    ###
    balls_h ~ dnorm(0,1),
    balls_a ~ dnorm(0,1),
    ###
    hits_allowed_h ~ dnorm(0,1),
    hits_allowed_a ~ dnorm(0,1),
    ###
    pballs_h ~ dnorm(0,1),
    pballs_a ~ dnorm(0,1),
    ###
    pstrikeouts_h ~ dnorm(0,1),
    pstrikeouts_a ~ dnorm(0,1),
    ###
    strikes_h ~ dnorm(0,1),
    strikes_a ~ dnorm(0,1),

    ###standard priors
    c(b, c, d) ~ dnorm(0,1),
    ###  
    c(fh1, fh2, fh3, fh4, fa1, fa2, fa3, fa4) ~ dnorm(0,1),
    sigma ~ dcauchy(0,2)),
  data=mydata2,
  iter=2000, warmup=200, chains=1, cores=4)

#option
#,control = list(adapt_delta = 0.99, max_treedepth = 15)
