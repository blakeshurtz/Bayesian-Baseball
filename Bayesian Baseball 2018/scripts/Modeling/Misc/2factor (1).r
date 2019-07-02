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
    mu <- A + B + C,
    A <- a_inh[home] + a_ina[away],
    B <- bath[home] * ba_home_scale + bata[away] * ba_away_scale,
    C <- pith[home] * RSA_home_scale + pita[away] * RSA_away_scale,
    ###priors
    c(a_inh, bath, pith)[home] ~ dmvnormNC(sigma_home,Rho_home),
    c(a_ina, bata, pita)[away] ~ dmvnormNC(sigma_away,Rho_away),
    #c(a, b, c) ~ dnorm(0,1),
    sigma_home ~ dcauchy(0,2),
    sigma_away ~ dcauchy(0,2),
    Rho_home ~ dlkjcorr(4),
    Rho_away ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###
stancode(m2)
precis(m2, depth=2, corr=FALSE)
?precis
