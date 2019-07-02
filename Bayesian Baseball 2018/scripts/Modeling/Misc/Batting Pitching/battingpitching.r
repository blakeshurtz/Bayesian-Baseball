###two-variable model: batting and pitching
###starting with mydata from ari
library(rethinking)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m2 <- map2stan(
  alist(
    rdiffscale ~ dnorm( mu, sigma),
    ###likelihood/linear models
    mu <- a_in[Opp] + bat[Opp] * BAscale + pit[Opp] * HHAscale,
    ###adaptive priors
    c(a_in,bat,pit)[Opp] ~ dmvnormNC(sigma_opp,Rho),
    ###fixed priors
    c(a, b, c) ~ dnorm(0,1),
    sigma_opp ~ dcauchy(0,2),
    Rho ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###
stancode(m2)
###
