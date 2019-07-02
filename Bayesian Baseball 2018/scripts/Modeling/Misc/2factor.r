###two-factor model, pg. 443 (non-adaptive priors)
###starting with mydata from ari
library(rethinking)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m2 <- map2stan(
  alist(
    rdiffscale ~ dnorm( mu , sigma ),
    ###likelihood/linear models
    mu <- A + B*BAscale,
    A <- a + a_opp[Opp] + a_block[loc_block],
    B <- b + b_opp[Opp] + b_block[loc_block],
    ###priors
    c(a_opp,b_opp)[Opp] ~ dmvnormNC(sigma_opp,Rho_opp),
    c(a_block,b_block)[loc_block] ~ dmvnormNC(sigma_block,Rho_block),
    c(a, b) ~ dnorm(0,1),
    sigma_opp ~ dcauchy(0,2),
    sigma_block ~ dcauchy(0,2),
    Rho_opp ~ dlkjcorr(4),
    Rho_block ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###
stancode(m2)
