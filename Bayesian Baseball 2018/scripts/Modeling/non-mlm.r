
```{r nonmlm.m,eval=FALSE, echo=FALSE}
TEMP <- mydata
TEMP$home <- coerce_index(TEMP$home)
TEMP$away <- coerce_index(TEMP$away)

set.seed(1234)
options(mc.cores = parallel::detectCores())

nonmlm.m <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    mu <- inter +
      h * home + a * away + 
      hits_h * Hh + double_h * B2Bh + triple_h * B3Bh + HR_h * BHRh + balls_h * BBBh +
      hits_a * Ha + double_a * B2Ba + triple_a * B3Ba + HR_a * BHRa + balls_a * BBBa +
      hits_allowed_h * HHAh + pballs_h * PBBh + pstrikeouts_h * PSOh +  strikes_h * Strh +
      hits_allowed_a * HHAa + pballs_a * PBBa + pstrikeouts_a * PSOa + strikes_a * Stra +
      fh1 * PO_h + fh2 * A_h + fh3 * E_h + fh4 * DP_h + fa1 * PO_a + fa2 * A_a + fa3 * E_a + fa4 * DP_a,
    ###adaptive priors for coefficients
    inter ~ dunif(-100, 100),
    h ~ dunif(-100, 100),
    a ~ dunif(-100, 100),
    ###
    hits_h ~ dunif(-100, 100),
    hits_a ~ dunif(-100, 100),
    ###
    double_h ~ dunif(-100, 100),
    double_a ~ dunif(-100, 100),
    ###
    triple_h ~ dunif(-100, 100),
    triple_a ~ dunif(-100, 100),
    ###
    HR_h ~ dunif(-100, 100),
    HR_a ~ dunif(-100, 100),
    ###
    balls_h ~ dunif(-100, 100),
    balls_a ~ dunif(-100, 100),
    ###
    hits_allowed_h ~ dunif(-100, 100),
    hits_allowed_a ~ dunif(-100, 100),
    ###
    pballs_h ~ dunif(-100, 100),
    pballs_a ~ dunif(-100, 100),
    ###
    pstrikeouts_h ~ dunif(-100, 100),
    pstrikeouts_a ~ dunif(-100, 100),
    ###
    strikes_h ~ dunif(-100, 100),
    strikes_a ~ dunif(-100, 100),
    ###standard priors
    c(fh1, fh2, fh3, fh4, fa1, fa2, fa3, fa4) ~ dunif(-100, 100),
    sigma ~ dcauchy(0,2)),
  data=TEMP,
  iter=100, warmup=10, chains=1, cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15))
```
