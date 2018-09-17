#Predicting Game 1
d.pred <- list(
  pitera_norm = -.929, #Lester
  wl_norm = 1.29, #WL normalized for CLE (from WL dataset)
  opp_team = 0) #placeholder
#Posterior Simulation
set.seed(1234)
sim.model <- sim( model2 , data=d.pred, n=12000)
sim <- as.data.frame(sim.model)
#Posterior Statistics
pred.p.mean <- apply( sim , 2 , mean ); pred.p.mean
pred.p.PI <- apply( sim, 2 , PI , prob=0.95 ); pred.p.PI
#Plot
par(mfrow=c(1,1))
histgm <- hist(sim$V1, breaks=100, prob=TRUE, main="Simulated Games", xlab="Score") #Distribution of predicted scores
histgm$counts=histgm$counts/sum(histgm$counts)
plot(histgm, col=ifelse(histgm$breaks >= 0, "green", "red"), main="Posterior Distribution of Outcomes Against CLE (Game 1)", xlab="Score", xlim=c(-12,12))
abline(v=0, col="blue")
prob_success <- sum(sim$V1 > 0)/nrow(sim); prob_success #percent of scores that are wins
sum(sim$V1 < 0)/nrow(sim) #percent of scores that are losses
#Win 4 out of 7
dbinom(4, size=7, prob=prob_success) + dbinom(5, size=7, prob=prob_success) + dbinom(6, size=7, prob=prob_success)+ dbinom(7, size=7, prob=prob_success) 
pbinom(3, size=7, prob=prob_success, lower.tail = FALSE) 

