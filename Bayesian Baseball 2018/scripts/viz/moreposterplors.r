All matches can be combined into a single posterior distribution. They can be separated by home or away. Sigma is included in the model.

```{r, echo=FALSE, message=FALSE, warning=FALSE, include=FALSE}
hometeam = "ARI"
awayteam = "COL"

###create posteriors for individual games
TEMP <- mydata %>% filter(home == hometeam & away == awayteam)
mu <- link(m, data=data.frame(TEMP))
mu.dist <- as.data.frame(mu$mu)
###visualize "grand posterior"
###Note: matches other methods of visualization
mu.dist.t <- cbind(c(mu.dist$V1, mu.dist$V2, mu.dist$V3, mu.dist$V4, mu.dist$V5, mu.dist$V6, mu.dist$V7, mu.dist$V8, mu.dist$V9))
mu.dist.t <- as.data.frame(mu.dist.t)
colnames(mu.dist.t) <- "total"
```


```{r, echo=FALSE, message=FALSE, warning=FALSE}

###visualize posteriors for all games and "grand posterior"
mu.dist.all <- cbind(mu.dist, mu.dist.t)
ggplot(mu.dist.all) + 
  geom_freqpoly(aes(x=V1, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V2, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V3, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V4, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V5, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V6, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V7, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V8, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=V9, y=..density..), alpha=0.1) +
  geom_freqpoly(aes(x=total, y=..density..), alpha=1) 
```
