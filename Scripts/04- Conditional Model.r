library(rethinking)
library(tidyverse)
#Starting Fresh
urlfile<-'https://raw.githubusercontent.com/blakeobeans/Bayesian-Baseball/master/Cubs/Season/cubs.csv'
data<-read.csv(urlfile)
#Pitcher Stats
urlfile<-'https://raw.githubusercontent.com/blakeobeans/Bayesian-Baseball/master/Cubs/Pitching/pitching.csv'
pitcher<-read.csv(urlfile)
data<- left_join(data, pitcher,by="Pit")
#WL Record
urlfile<-'https://raw.githubusercontent.com/blakeobeans/Bayesian-Baseball/master/Cubs/Team%20Rankings/rankings.csv'
WL<-read.csv(urlfile)
data<- left_join(data, WL,by="Opp")
#Transform Data
data$score <- data$R-data$RA #Positive score indicates Giants win. Beats logistic. No ties.
data$opp_team <- coerce_index(data$Opp) #ID for team (function from rethinking)
data$pit_id <- coerce_index(data$Pit) #Home pitcher
names(data) <- c("tm", "opp", "R", "RA", "pit", "pitera", "wl", "score",  "opp_team", "pit_id")
data$pitera_norm <-  (data$pitera - mean(data$pitera))/sd(data$pitera) #normalize ERA
data$wl_norm <-  (data$wl - mean(data$wl))/sd(data$wl) #normalize WL
data <- as.data.frame(data)
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
#Predicting game1
d.pred <- list(
  pitera_norm = -.63, #Lester
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 0) #placeholder
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model2 , data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
prob_success1 <- sum(sim$V1 > 0)/nrow(sim); prob_success1 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#Win 4 out of 7
dbinom(4, size=7, prob=prob_success1) + dbinom(5, size=7, prob=prob_success1) + dbinom(6, size=7, prob=prob_success1) + dbinom(7, size=7, prob=prob_success1)
prob_win1 <- pbinom(3, size=7, prob=prob_success1, lower.tail = FALSE) ; prob_win1
exp1 <- 7*prob_win1; exp1
var1 <- 7*prob_win1*(1-prob_win1); var1
binomplot <- as.data.frame(cbind(exp1, var1))

#Predicting 2016 Game 2
#After game 1
game1 <- c(-6, 21, -.63, 1.29)
standata2 <- rbind(standata, game1)
#run model with game 1 data
set.seed(1234)
model3 <- map2stan(
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
  data=standata2, iter=12000, warmup=3000, chains=4, cores=4)
#Predicting Game 2
d.pred <- list(
  pitera_norm = -0.004, #Arrieta
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 21) #New team- Indians (1 game played)
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model3 , data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
prob_success2 <- sum(sim$V1 > 0)/nrow(sim); prob_success2 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#Win 4 out of 6
dbinom(4, size=6, prob=prob_success2) + dbinom(5, size=6, prob=prob_success2) + dbinom(6, size=6, prob=prob_success2)
prob_win2 <- pbinom(3, size=6, prob=prob_success2, lower.tail = FALSE) ; prob_win2
exp2 <- 7*prob_win2; exp2
var2 <- 7*prob_win2*(1-prob_win2); var2
game2exp <- c(exp2, var2)
binomplot <- rbind(binomplot, game2exp)

#Predicting 2016 Game 3
game2 <- c(4, 21, -0.004, 1.29)
standata3 <- rbind(standata2, game2)
#run model with game 2 data
set.seed(1234)
model4 <- map2stan(
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
  data=standata3, iter=12000, warmup=3000, chains=4, cores=4)
#Predicting Game 3
d.pred <- list(
  pitera_norm = .616, 
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 21) #New team- Indians (1 game played)
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model4 , data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
prob_success3 <- sum(sim$V1 > 0)/nrow(sim); prob_success3 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#Win 3 out of 5
dbinom(3, size=5, prob=prob_success3) + dbinom(4, size=5, prob=prob_success3) + dbinom(5, size=5, prob=prob_success3)
prob_win3 <- pbinom(2, size=5, prob=prob_success3, lower.tail = FALSE) ; prob_win3
exp3 <- 7*prob_win3; exp3
var3 <- 7*prob_win3*(1-prob_win3); var3
game3exp <- c(exp3, var3)
binomplot <- rbind(binomplot, game3exp)

#Predicting 2016 Game 4
game3 <- c(-1, 21, .616, 1.29)
standata4 <- rbind(standata3, game3)
#run model with game 3 data
set.seed(1234)
model5 <- map2stan(
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
  data=standata4, iter=12000, warmup=3000, chains=4, cores=4)
#Predicting Game 4
d.pred <- list(
  pitera_norm = .24, 
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 21) #New team- Indians (1 game played)
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model5 , data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
prob_success4 <- sum(sim$V1 > 0)/nrow(sim); prob_success4 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#After the fact: Win 3 out of 4
dbinom(3, size=4, prob=prob_success4) + dbinom(4, size=4, prob=prob_success4) 
prob_win4 <- pbinom(2, size=4, prob=prob_success4, lower.tail = FALSE); prob_win4
exp4 <- 7*prob_win4; exp4
var4 <- 7*prob_win4*(1-prob_win4); var4
game4exp <- c(exp4, var4)
binomplot <- rbind(binomplot, game4exp)

#Predicting 2016 Game 5
game4 <- c(-5, 21, .24, 1.29)
standata5 <- rbind(standata4, game4)
#run model with game 4 data
set.seed(1234)
model6 <- map2stan(
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
  data=standata5, iter=12000, warmup=3000, chains=4, cores=4)
#Predicting Game 5
d.pred <- list(
  pitera_norm = -.64, 
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 21) #New team- Indians (1 game played)
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model6, data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
prob_success5 <- sum(sim$V1 > 0)/nrow(sim); prob_success5 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#Win 3 out of 3
dbinom(3, size=3, prob=prob_success5) 
prob_win5 <- pbinom(2, size=3, prob=prob_success5, lower.tail = FALSE); prob_win5
exp5 <- 7*prob_win5; exp5
var5 <- 7*prob_win5*(1-prob_win5); var5
game5exp <- c(exp5, var5)
binomplot <- rbind(binomplot, game5exp)

#Predicting 2016 Game 6
game5 <- c(1, 21, -.64, 1.29)
standata6 <- rbind(standata5, game5)
#run model with game 5 data
set.seed(1234)
model7 <- map2stan(
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
  data=standata6, iter=12000, warmup=3000, chains=4, cores=4)
#Predicting Game 6
d.pred <- list(
  pitera_norm = -0.004, 
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 21) #New team- Indians (1 game played)
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model7, data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
prob_success6 <- sum(sim$V1 > 0)/nrow(sim); prob_success6 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#Win 2 out of 2
dbinom(2, size=2, prob=prob_success6) 
prob_win6 <- pbinom(1, size=2, prob=prob_success6, lower.tail = FALSE); prob_win6
exp6 <- 7*prob_win6; exp6
var6 <- 7*prob_win6*(1-prob_win6); var6
game6exp <- c(exp6, var6)
binomplot <- rbind(binomplot, game6exp)

#Predicting 2016 Game 7
game6 <- c(6, 21, -0.004, 1.29)
standata7 <- rbind(standata6, game6)
#run model with game 6 data
set.seed(1234)
model8 <- map2stan(
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
  data=standata7, iter=12000, warmup=3000, chains=4, cores=4)
#Predicting Game 7
d.pred <- list(
  pitera_norm = -2, 
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 21) #New team- Indians (1 game played)
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model8, data=d.pred, n=6000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
prob_success7 <- sum(sim$V1 > 0)/nrow(sim); prob_success7 #percent of scores that are wins
prob_fail <- sum(sim$V1 < 0)/nrow(sim); prob_fail #percent of scores that are losses
#Win 1 out of 1
dbinom(1, size=1, prob=prob_success7) 
prob_win7 <- pbinom(0, size=1, prob=prob_success7, lower.tail = FALSE); prob_win7
exp7 <- 7*prob_win7; exp7
var7 <- 7*prob_win7*(1-prob_win7); var7
game7exp <- c(exp7, var7)
binomplot <- rbind(binomplot, game7exp)

#Plot Single-Game Probabilities
probabilities <- c(prob_success1, prob_success2, prob_success3, prob_success4, prob_success5, prob_success6, prob_success7)
probabilities <- c(.597, .527, .518, .528, .5515, .5215, .641)
plot(probabilities, type="b", xlab="Game", main="Conditional Probability of Single-Game Success",
     sub="Games 1 through 7", ylim=c(.4,.7))
abline(h=.5, col="red")

#Plot Binomial Probabilities
probabilities <- c(prob_win1, prob_win2, prob_win3, prob_win4, prob_win5, prob_win6, prob_win7)
probabilities <- c(.7, .4, .54, .36, .17, .27, .94)
plot(probabilities, type="b", xlab="Game", main="Conditional Probability of Cubs Winning World Series",
     sub="Games 1 through 7", ylim=c(0,1))
abline(h=.5, col="red")
