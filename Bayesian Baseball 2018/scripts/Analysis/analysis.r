post <- extract.samples(mdiff)


hometeam = "BOS"
awayteam = "LAD"

#BOSTONs Performance (home)
TEMP <- mydata %>% filter(home == hometeam)
link.model <- link(mdiff, data=data.frame(TEMP))

mean(link.model$mu)#BOS is better by 1.65 points.
#this is a function of:
mean(link.model$A) #intercepts- 0.33 team effect
mean(link.model$B) #home team- 2.65 pts for BOS- batting and pitching
mean(link.model$C) #away team- 1.12 points for away team batting and pitching
mean(link.model$D) #fielding- BOS- .09 pts for fielding

#LAD's performance (away)
TEMP <- mydata %>% filter(away == awayteam)
link.model2 <- link(mdiff, data=data.frame(TEMP))

mean(link.model2$mu)#LAD is better by -1.48 points.
#this is a function of:
mean(link.model2$A) #intercepts- 0.02 pts team effect
mean(link.model2$B) #home team- -0.16 for home team 
mean(link.model2$C) #away team- 1.79 points for LAD
mean(link.model2$D) #fielding- .34 pts for fielding- HOME TEAM

#wrangling for viz
BOS <- melt(link.model$B); colnames(BOS) <- c("Category", "x2bat", "Value")
BOS$Category = rep("BOS_away_BP", nrow(BOS)); 
BOS <- BOS[,c("Category", "Value")]
#
LAD <- melt(link.model2$C); colnames(LAD) <- c("Category", "x2pit", "Value")
LAD$Category = rep("LAD_Away_BP", nrow(LAD))
LAD <- LAD[,c("Category", "Value")]
LAD$Value = LAD$Value * -1 #for visualization
#
TEMP <- as.data.frame(rbind(BOS, LAD))
#graph
ggplot(TEMP) + 
  geom_density(aes(x = Value, group=Category, col=Category), kernel="gaussian")
