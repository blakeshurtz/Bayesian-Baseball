###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
library(tidyverse)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
###
m <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    mu <- A + B + C + D,
    A <- a + h[home] + a[away],
    B <- b + hits_h[home] * Hh + double_h[home] * B2Bh + triple_h[home] * B3Bh + HR_h[home] * BHRh + balls_h[home] * BBBh +
         hits_a[away] * Ha + double_a[away] * B2Ba + triple_a[away] * B3Ba + HR_a[away] * BHRa + balls_a[away] * BBBa,
    C <- c + hits_allowed_h[home] * HHAh + pballs_h[home] * PBBh + pstrikeouts_h[home] * PSOh +  strikes_h[home] * Strh +
         hits_allowed_a[away] * HHAa + pballs_a[away] * PBBa + pstrikeouts_a[away] * PSOa + strikes_a[away] * Stra,
    D <- d + fh1 * PO_h + fh2 * A_h + fh3 * E_h + fh4 * DP_h + fa1 * PO_a + fa2 * A_a + fa3 * E_a + fa4 * DP_a,
    ###adaptive priors
    c(h, hits_h, double_h, triple_h, HR_h, balls_h, hits_allowed_h, pballs_h, pstrikeouts_h, strikes_h)[home] ~ dmvnormNC(sigma_home,Rho_home),
    c(a, hits_a, double_a, triple_a, HR_a, balls_a, hits_allowed_a, pballs_a, pstrikeouts_a, strikes_a)[away] ~ dmvnormNC(sigma_away,Rho_away),
    ###standard priors
    c(a, b, c, d) ~ dnorm(0,1),
    c(fh1, fh2, fh3, fh4, fa1, fa2, fa3, fa4) ~ dnorm(0,1),
    sigma_home ~ dcauchy(0,2),
    sigma_away ~ dcauchy(0,2),
    Rho_home ~ dlkjcorr(4),
    Rho_away ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)),
  data=mydata,
  iter=20000, warmup=2000, chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
