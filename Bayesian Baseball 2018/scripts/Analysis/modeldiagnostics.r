library(rethinking)
library(tidyverse)



compare(m2, mscaled, mvarintscaled)
compare(m2, mscaled, mvarintscaled)@dSE
plot(compare(m2, mscaled, mvarintscaled))



#compare different models
compare(m1, m2, m3, m4, m5)
compare(m1, m2, m3, m4, m5, func = LOO)

###coefficient estimates
precis(mscaled, depth=3) #pars = sigma_home, sigma_away, sigma, h, a, hits_a, etc.
precis(mscaled, depth=3, pars="sigma")
precis(m2, depth=3, pars="sigma")
precis(mvarintscaled, depth=3, pars="sigma")

#extract samples
post <- extract.samples(mscaled)

rm(k)
###using link to construct a single posterior distribution based on selected team(s)
#1. Consider combining home and away games into the same distribution... check for statistical difference
library(tidyverse)
TEMP <- newdata2 %>% filter(home == "ARI" & away == "COL")
mu <- link(m4, data=data.frame(TEMP))
dens(mu[["mu"]], col=col.alpha("black",0.4), show.HPDI=.5) ##note negative mu means a loss for home team
#2. use link to measure strength of pitching, batting etc.
dens(mu[["A"]], col=col.alpha("black",0.4), show.HPDI=.5) 

#Summarize the distribution of mu (mean for each game) 
#this may be useful, but not on a team-level
mu <- as.data.frame(mu)
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

#predict individual games, incorporating model variance (pg. 108)
sim.m <- sim(m2, data=as.data.frame(TEMP))
mu.mean.sim <- apply(sim.m, 2, mean)
mu.int.sim <- apply(sim.m, 2 , PI , prob=0.89)

#Plotting 95% intervals for each GAME
par(mfrow=c(1,1))
plot(seq(1:length(mu.mean.sim)), mu.mean.sim, type = "n", ylim=c(-10, 10), main ="Model Predictions") 
segments(seq(1:length(mu.mean.sim)), mu.mean.sim + mu.int.sim[1,], seq(1:length(mu.mean.sim)), mu.mean.sim + mu.int.sim[2,])
points(seq(1:length(mu.mean.sim)), TEMP$rdiff, col = "red", pch=4) #actual scores
points(seq(1:length(mu.mean.sim)), mu.mean.sim, col = "blue", pch=19) #predicted scores
abline(h = 0, col = "red")

#plotting posterior density (for a single game)
sim.m.df <- as.data.frame(sim.m)
dens(sim.m.df$V1)

#generate predictions (pg. 415)
#1. write function
p.link <- function( BA , mu.mean.sim ) {
  mu <- with( post , a_bat[,mu.mean.sim] + b_bat[,mu.mean.sim] * BA
  )
  return( mu )
}
#2. sort observations
BA <- TEMP$BA
mu.mean.sim <- as.character(TEMP$mu.mean.sim)
#3. generate predictions
pred.raw <- sapply( 1:length(BA) , function(i) p.link(BA[i],2) )
pred.p <- apply( pred.raw , 2 , mean )
pred.p.PI <- apply( pred.raw , 2 , PI )
