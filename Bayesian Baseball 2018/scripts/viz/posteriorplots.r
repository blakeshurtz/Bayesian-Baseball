###ggplot visualization of posteriors for each game
###using m4
library(tidyverse)
library(rethinking)

hometeam = "ARI"
awayteam = "COL"

###create posteriors for individual games
TEMP <- newdata2 %>% filter(home == hometeam & away == awayteam)
mu <- link(m4, data=data.frame(TEMP))
mu.dist <- as.data.frame(mu$mu)

###vizualize posteriors for all games
c <-  ggplot(mu.dist) + 
      geom_freqpoly(aes(x=V1, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V2, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V3, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V4, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V5, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V6, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V7, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V8, y=..density.., alpha=0.5)) +
      geom_freqpoly(aes(x=V9, y=..density.., alpha=0.5)) 
c

###visualize "grand posterior"
###Note: matches other methods of visualization
mu.dist.t <- cbind(c(mu.dist$V1, mu.dist$V2, mu.dist$V3, mu.dist$V4, mu.dist$V5, mu.dist$V6, mu.dist$V7, mu.dist$V8, mu.dist$V9))
mu.dist.t <- as.data.frame(mu.dist.t)
colnames(mu.dist.t) <- "total"
d <- ggplot(mu.dist.t) +
    geom_freqpoly(aes(x=total, y=..density..))
d

###visualize posteriors for all games and "grand posterior"
mu.dist.all <- cbind(mu.dist, mu.dist.t)
e <-ggplot(mu.dist.all) + 
    geom_freqpoly(aes(x=V1, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V2, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V3, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V4, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V5, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V6, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V7, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V8, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=V9, y=..density.., alpha=0.5)) +
    geom_freqpoly(aes(x=total, y=..density.., alpha=1)) 
e

###RUNNING CODE FOR OTHER SIDE
###create posteriors for individual games
TEMP2 <- newdata2 %>% filter(away == hometeam & home == awayteam)
mu2 <- link(m4, data=data.frame(TEMP2))
mu.dist2 <- as.data.frame(mu2$mu)

###vizualize posteriors for all games
c2 <-  ggplot(mu.dist2) + 
  geom_freqpoly(aes(x=V1, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V2, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V3, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V4, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V5, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V6, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V7, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V8, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V9, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V10, y=..density.., alpha=0.5)) 
c2

###visualize "grand posterior"
mu.dist.t2 <- cbind(c(mu.dist2$V1, mu.dist2$V2, mu.dist2$V3, mu.dist2$V4, mu.dist2$V5, 
                      mu.dist2$V6, mu.dist2$V7, mu.dist2$V8, mu.dist2$V9, mu.dist2$V10))
mu.dist.t2 <- as.data.frame(mu.dist.t2)
colnames(mu.dist.t2) <- "total"

d2 <- ggplot(mu.dist.t2) +
  geom_freqpoly(aes(x=total, y=..density..))
d2

###visualize posteriors for all games and "grand posterior"
mu.dist.all2 <- cbind(mu.dist2, mu.dist.t2)
e <-ggplot(mu.dist.all2) + 
  geom_freqpoly(aes(x=V1, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V2, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V3, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V4, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V5, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V6, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V7, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V8, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=V9, y=..density.., alpha=0.5)) +
  geom_freqpoly(aes(x=total, y=..density.., alpha=1)) 
e


#2. use link to measure strength of pitching, batting etc.
dens(mu[["A"]], col=col.alpha("black",0.4), show.HPDI=.5) 