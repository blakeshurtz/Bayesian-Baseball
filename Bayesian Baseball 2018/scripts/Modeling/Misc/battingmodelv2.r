###two-variable model: batting and pitching
###starting with mydata from ari
library(rethinking)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m3 <- map2stan(
  alist(
    rdiffscale ~ dnorm( mu , sigma ),
    ###likelihood/linear models
    mu <- A + B,
    A <- a + a_bat[Opp],
    B <- b + b_bat[Opp] * BAscale,
    ###adaptive priors
    c(a_bat,b_bat)[Opp] ~ dmvnorm2(c(a,b),sigma_opp,Rho),
    #c(a_bat,b_bat)[Opp] ~ dmvnormNC(sigma_opp,Rho_opp),
    ###fixed priors
    c(a, b) ~ dnorm(0,1),
    sigma_opp ~ dcauchy(0,2),
    #Rho_opp ~ dlkjcorr(2),
    Rho ~ dlkjcorr(2),
    sigma ~ dcauchy(0,2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###
stancode(m3)
###
precis(m3, depth=1)
precis(m1, depth=1)
