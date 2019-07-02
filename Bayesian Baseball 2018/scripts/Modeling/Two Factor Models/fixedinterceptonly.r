###fixed intercept model
library(rethinking)
###
set.seed(1234)
options(mc.cores = parallel::detectCores())
###
m1 <- map2stan(
  alist(
    rdiff ~ dnorm( mu , sigma ),
    mu <- a + h[home] + a[away],
    #adaptive priors
    h[home] ~ dnorm(0,sh),
    a[away] ~ dnorm(0, sa),
    #mean priors
    a ~ dnorm(0,1),
    #variance priors
    sh ~ dcauchy(0,1),
    sa ~ dcauchy(0,1),
    sigma ~ dcauchy(0,2)
  ) ,
  data=newdata2,
  iter=5000 , warmup=2000 , chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
###
