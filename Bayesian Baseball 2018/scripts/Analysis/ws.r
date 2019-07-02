
```{r}
TEMP <- wspre #rdiff is how they performed as the home team
sim.model <- sim(ws2, data=as.data.frame(TEMP)) #predict individual games
hist(sim.model)
sum(sim.model>0)/1000
```

####Game 3
Game 3 is the first game in the WS where the home/away roles are switched. We run against the constraints of the model. What to do?
  
  1. Simulate a new model that incorporates the previous games between BOS and LAD.
2. Put in fixed intercept terms

```{r}
TEMP <- ws[1:2,] #rdiff is how they performed as the home team
sim.model <- sim(m, data=as.data.frame(TEMP)) #predict individual games
sim.model <- as.numeric(sim.model) #single vector
hist(sim.model)
```

####Game 4

```{r}
#boston- home
TEMP <- ws[] #rdiff is how they performed as the home team
sim.model1 <- sim(m, data=as.data.frame(TEMP)) #predict individual games
sim.model1 <- as.numeric(sim.model1) #single vector
#hist(sim.model1)

#lad- away
TEMP <- mydata %>% filter(away == "LAD") #rdiff is how they performed as the away team
sim.model2 <- sim(m, data=as.data.frame(TEMP)) #predict individual games
sim.model2 <- as.numeric(sim.model2) #single vector
sim.model2 <- sim.model2 * -1 #convert to positive
#hist(sim.model2)
```




```{r, message=FALSE, eval=FALSE}
set.seed(1234)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

wspreint <- map2stan(
  alist(
    rdiff ~ dnorm( mu, sigma),
    mu <- a + h[home] + a[away],
    ###adaptive priors
    h[home] ~ dnorm( hm , hs ), #adaptive prior from the data
    hm ~ dnorm(0, 1),
    hs ~ dcauchy(0,2),
    a[away] ~ dnorm( am , as ), #adaptive prior from the data
    am ~ dnorm(0, 1),
    as ~ dcauchy(0,2),
    a ~ dnorm(0,3),  
    sigma ~ dcauchy(0,2)),
  data=mydataint,
  iter=2000, warmup=200, chains=1, cores=4)
#control = list(adapt_delta = 0.99, max_treedepth = 15)
```


```{r}
d.pred <- list(
  home = 'BOS', 
  away = 'LAD') 

sim.model <- sim(wspreint, data=as.data.frame(d.pred)) #predict individual games
hist(sim.model)
sum(sim.model>0)/1000
```

