###fixed intercept model
library(rethinking)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m <- map2stan(
  alist(
    rdiffscale ~ dnorm( mu , sigma ),
    mu <- a_bat[Opp],
    a_bat[Opp] ~ dnorm(a,s),
    a ~ dnorm(0,1),
    s ~ dcauchy(0,1),
    sigma ~ dcauchy(0,2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###
stancode(m)
