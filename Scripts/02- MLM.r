'What is Stan? Stan is a program that does monte carlo simulation, specifically, 
Hamiltonian Monte Carlo with a no u-turn sample, to calculate posterior distributions 
through stochastic (as opposed to analytical means). Hamiltonian monte carlo is less 
likely to get stuck as local minimums, which is a problem when dealing with 
multi-modal posteriors.https://chi-feng.github.io/mcmc-demo/'


set.seed(1234)
model <- map2stan(
  alist(
    score ~ dnorm( mu , sigma ) , #fair assumption given data
    mu <- a[opp_team], #this is the regression equation/likelihood function
    a[opp_team] ~ dnorm( 0 , 4 ), #what choice prior
    sigma ~ dcauchy(0, 2.5) #prior to sigma
    
  ),
  data=data, iter=6000, warmup=3000, chains=4)

#coefficients
head(precis(model, depth=2, prob=.95)) #OK that 0 in CI, similar sigma

#sample from posterior
postmodel <- extract.samples( model, n=5000)
postmodel.df <- as.data.frame(postmodel)
str(postmodel)

#Prior and Posterior- Note: does not account for group-level variance
par(mfrow=c(1,1))
dens( postmodel$a[,7], show.HPDI=0.95, main= "Cubs vs. Angels- Posterior of Mean Score") #POSTERIOR dist of means 
x<-seq(from=-4, to=10, by=.1)
lines(x, dnorm(x, 0, 4), col="blue") #PRIOR Distribution

#Simulating posterior distributions all opposing teams
d.pred <- list(opp_team = 1:20)

#sim function to plot distribution of scores- This accounts for variance 
sim.model <- sim( model , data=d.pred)
str(sim.model)

#CI for scores by team
pred.p <- apply( sim.model, 2 , mean); pred.p
pred.p.PI <- apply( sim.model, 2 , HPDI, prob=.95 ); pred.p.PI
pred.p2 <-as.data.frame(pred.p); head(pred.p2)
pred.p.PI2 <-as.data.frame(pred.p.PI); head(pred.p.PI2)
pred.p.PI2 <- t(pred.p.PI2); head(pred.p.PI2)
d<-cbind(pred.p2, pred.p.PI2, 1:20)
names(d) <- c("mean", "lower", "upper", "games"); head(d)

#Posterior Distributions
plot(d$games, d$mean, type = "n", xlab = "Opposing Team", ylab = "Score", ylim=c(-15,15), main= "MLM Regression")
segments(d$games, d$lower, d$games, d$upper)
abline(h = 0, col = "red")
points(data$opp_team, data$score, col = "red", pch=4) 
points(d$games, d$mean, col = "blue", pch=19)

#compare intervals to standard regression
plot(lmdata$opp_team, lmdata$mean, type = "n", ylim=c(-15, 15), main ="OLS Regression") #note enlarging y-axis
segments(lmdata$opp_team, lmdata$mean + lmdata$lwr, lmdata$opp_team, lmdata$mean + lmdata$upr)
points(data$opp_team, data$score, col = "red", pch=4)
points(lmdata$opp_team, lmdata$mean, col = "blue", pch=19)
abline(h = 0, col = "red")

#Effect of MLM on mean, pooling effect across tanks
plot(d$games, d$mean, type = "n", xlab = "match", ylab = "score", ylim=c(-5,5), main="Pooling Effect of MLM")
points(lmdata$opp_team, lmdata$mean, col = "red", pch=1) #mean for standard regression
points(d$games, d$mean, col = "blue", pch=19) #mean for MLM
abline(h=mean(data$score), col = "blue")

#MLM. Pitcher ERA, WL record, how many levels in baseball?
#Note hyperparameters
standata <- data[,c("score", "opp_team", "pitera_norm", "wl_norm")]
set.seed(1234)
model2 <- map2stan(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + a_team[opp_team] + b * pitera_norm + c * wl_norm,
    sigma ~ dcauchy(0, 2.5),
    a ~ dnorm(0,3),
    a_team[opp_team] ~ dnorm( ai , as ), #adaptive prior from the data
    ai ~ dnorm(0, 1),
    as ~ dcauchy(0,2),
    b ~ dnorm( 0, 1 ),
    c ~ dnorm(0,1)
  ),
  data=standata, iter=12000, warmup=3000, chains=4, cores=4)

#comparing models
compare(model, model2)

#coefficients
precis(model2, depth=2, prob=.95) #n_eff: number effective samples. R-hat: convergence to target dist.  
coef <- precis(model2, depth=2, prob=.95)
coef2 <- slice(coef, c(1:2, 23:26))
variable <- c("sigma", "a", "ai", "as", "b", "c")
coef2 <- cbind(coef2, variable)
coef2 <-as.data.frame(coef2)
coef2 <- select(coef2, "variable", "mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")

#trace plot. Using monte carlo methods to "hill climb" and find the most likely parameter.
plot(model2)



