mydata$HHAscale <- scale(ari_pit$HHA)
mydata$RSAscale <- scale(ari_pit$RSA)
###
m4 <- map2stan(
  alist(
    rdiffscale ~ dnorm( mu, sigma),
    ###likelihood/linear models
    mu <- A + B + C,
    A <- a_in[Opp], 
    B <- bat[Opp] * BAscale, 
    C <- pit[Opp] * HHAscale + pit2[Opp] * RSAscale,
    ###adaptive priors
    c(a_in,bat,pit, pit2)[Opp] ~ dmvnormNC(sigma_opp,Rho),
    ###fixed priors
    c(a, b, c, d) ~ dnorm(0,1),
    sigma_opp ~ dcauchy(0,2),
    Rho ~ dlkjcorr(4),
    sigma ~ dcauchy(0,2)
  ) ,
  data=mydata ,
  iter=5000 , warmup=2000 , chains=1, cores=1 )
###

compare(m3, m4)
