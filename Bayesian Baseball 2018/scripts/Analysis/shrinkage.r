###Shrinkage

```{r, non-mlm, include=FALSE}
post2 <- extract.samples(m2)

#sample from posterior
postmodel2.df <- as.data.frame(post2)

TEMP2 <- mydata2 %>% filter(home == "ARI" & away == "COL")
mu2 <- link(m2, data=data.frame(TEMP2))

#predict individual games, incorporating model variance (pg. 108)
sim.model2 <- sim(m2, data=as.data.frame(TEMP2))

#CI for scores by game
pred.p2 <- apply( sim.model2, 2 , mean); pred.p2
pred.p.PI2 <- apply( sim.model2, 2 , HPDI, prob=.95 ); pred.p.PI2
pred.p22 <-as.data.frame(pred.p2); head(pred.p22)
pred.p.PI22 <-as.data.frame(pred.p.PI2); head(pred.p.PI22)
pred.p.PI22 <- t(pred.p.PI22); head(pred.p.PI22)
d2<-cbind(pred.p22, pred.p.PI22, 1:9)
names(d2) <- c("mean", "lower", "upper", "games"); head(d2)
```

```{r, include=FALSE}
#sample from posterior
postmodel.df <- as.data.frame(post)

TEMP <- mydata %>% filter(home == "ARI" & away == "COL")
mu <- link(m, data=data.frame(TEMP))

#predict individual games, incorporating model variance (pg. 108)
sim.model <- sim(m, data=as.data.frame(TEMP))

#CI for scores by game
pred.p <- apply( sim.model, 2 , mean); pred.p
pred.p.PI <- apply( sim.model, 2 , HPDI, prob=.95 ); pred.p.PI
pred.p2 <-as.data.frame(pred.p); head(pred.p2)
pred.p.PI2 <-as.data.frame(pred.p.PI); head(pred.p.PI2)
pred.p.PI2 <- t(pred.p.PI2); head(pred.p.PI2)
d<-cbind(pred.p2, pred.p.PI2, 1:9)
names(d) <- c("mean", "lower", "upper", "games"); head(d)

```

```{r, echo=FALSE}
#Posterior Distributions
plot(d$games, d$mean, type = "n", xlab = "Opposing Team", ylab = "Score", ylim=c(-10,10), main= "MLM Regression")
segments(d$games, d$lower, d$games, d$upper)
abline(h = mean(TEMP$rdiff), col = "red")
points(d$games, d$mean, col = "red", pch=4) 
points(d2$games, d2$mean, col = "green", pch=4) 
points(d$games, TEMP$rdiff, col = "blue", pch=19)
```

The green x's are unpooled and the red x's are MLM.