###batting log wrangled in "substring.r"
###data was slightly edited from original. redo in R.
library(rethinking)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m1 <- map2stan(
  alist(
    rdiffscale ~ dnorm( mu , sigma ),
    mu <- a_bat[Opp] + b_bat[Opp]*BAscale,
    c(a_bat,b_bat)[Opp] ~ dmvnorm2(c(a,b),sigma_opp,Rho),
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    sigma_opp ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###
stancode(m1)
