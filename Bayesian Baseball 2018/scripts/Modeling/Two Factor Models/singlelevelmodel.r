###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
library(tidyverse)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
###
m1 <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    mu <- A + B + C + D,
    A <- a + h[home] + a[away],
    B <- b + hits_h * Hh + double_h * B2Bh + triple_h * B3Bh + HR_h * BHRh + balls_h * BBBh +
             hits_a * Ha + double_a * B2Ba + triple_a * B3Ba + HR_a * BHRa + balls_a * BBBa,
    C <- c + hits_allowed_h * HHAh + pballs_h * PBBh + pstrikeouts_h * PSOh +  strikes_h * Strh +
             hits_allowed_a * HHAa + pballs_a * PBBa + pstrikeouts_a * PSOa + strikes_a * Stra,
    D <- d + fh1 * PO_h + fh2 * A_h + fh3 * E_h + fh4 * DP_h + fa1 * PO_a + fa2 * A_a + fa3 * E_a + fa4 * DP_a,
    ###adaptive priors
    c(h, hits_h, double_h, triple_h, HR_h, balls_h, hits_allowed_h, pballs_h, pstrikeouts_h, strikes_h)[home] ~ dmvnormNC(sigma_home,Rho_home),
    c(a, hits_a, double_a, triple_a, HR_a, balls_a, hits_allowed_a, pballs_a, pstrikeouts_a, strikes_a)[away] ~ dmvnormNC(sigma_away,Rho_away),
    ###standard priors
    c(a, b, c, d) ~ dnorm(0,1),
    ###adaptive priors for coefficients
    hits_h ~ dnorm(hits_h_mu, hits_h_sigma),
    hits_h_mu ~ dnorm(0,1),
    hits_h_sigma ~ dcauchy(0,2), 
    hits_a ~ dnorm(hits_a_mu, hits_a_sigma),
    hits_a_mu ~ dnorm(0,1),
    hits_a_sigma ~ dcauchy(0,2),
    ###
    double_h ~ dnorm(double_h_mu, double_h_sigma),
    double_h_mu ~ dnorm(0,1),
    double_h_sigma ~ dcauchy(0,2),
    double_a ~ dnorm(double_a_mu, double_a_sigma),
    double_a_mu ~ dnorm(0,1),
    double_a_sigma ~ dcauchy(0,2),    
    ###
    triple_h ~ dnorm(triple_h_mu, triple_h_sigma),
    triple_h_mu ~ dnorm(0,1),
    triple_h_sigma ~ dcauchy(0,2),
    triple_a ~ dnorm(triple_a_mu, triple_a_sigma),
    triple_a_mu ~ dnorm(0,1),
    triple_a_sigma ~ dcauchy(0,2),
    ###
    HR_h ~ dnorm(HR_h_mu, HR_h_sigma),
    HR_h_mu ~ dnorm(0,1),
    HR_h_sigma ~ dcauchy(0,2),
    HR_a ~ dnorm(HR_a_mu, HR_a_sigma),
    HR_a_mu ~ dnorm(0,1),
    HR_a_sigma ~ dcauchy(0,2),    
    ###
    balls_h ~ dnorm(balls_h_mu, balls_h_sigma),
    balls_h_mu ~ dnorm(0,1),
    balls_h_sigma ~ dcauchy(0,2),
    balls_a ~ dnorm(balls_a_mu, balls_a_sigma),
    balls_a_mu ~ dnorm(0,1),
    balls_a_sigma ~ dcauchy(0,2),      
    ###
    hits_allowed_h ~ dnorm(hits_allowed_h_mu, hits_allowed_h_sigma),
    hits_allowed_h_mu ~ dnorm(0,1),
    hits_allowed_h_sigma ~ dcauchy(0,2),
    hits_allowed_a ~ dnorm(hits_allowed_a_mu, hits_allowed_a_sigma),
    hits_allowed_a_mu ~ dnorm(0,1),
    hits_allowed_a_sigma ~ dcauchy(0,2),      
    ###
    pballs_h ~ dnorm(pballs_h_mu, pballs_h_sigma),
    pballs_h_mu ~ dnorm(0,1),
    pballs_h_sigma ~ dcauchy(0,2),
    pballs_a ~ dnorm(pballs_a_mu, pballs_a_sigma),
    pballs_a_mu ~ dnorm(0,1),
    pballs_a_sigma ~ dcauchy(0,2),      
    ###
    pstrikeouts_h ~ dnorm(pstrikeouts_h_mu, pstrikeouts_h_sigma),
    pstrikeouts_h_mu ~ dnorm(0,1),
    pstrikeouts_h_sigma ~ dcauchy(0,2),
    pstrikeouts_a ~ dnorm(pstrikeouts_a_mu, pstrikeouts_a_sigma),
    pstrikeouts_a_mu ~ dnorm(0,1),
    pstrikeouts_a_sigma ~ dcauchy(0,2),     
    ###
    strikes_h ~ dnorm(strikes_h_mu, strikes_h_sigma),
    strikes_h_mu ~ dnorm(0,1),
    strikes_h_sigma ~ dcauchy(0,2),
    strikes_a ~ dnorm(strikes_a_mu, strikes_a_sigma),
    strikes_a_mu ~ dnorm(0,1),
    strikes_a_sigma ~ dcauchy(0,2),
    ###  
    c(fh1, fh2, fh3, fh4, fa1, fa2, fa3, fa4) ~ dnorm(0,1),
    sigma_home ~ dcauchy(0,2),
    sigma_away ~ dcauchy(0,2),
    Rho_home ~ dlkjcorr(4),
    Rho_away ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)),
  data=mydata,
  iter=20000, warmup=2000, chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
