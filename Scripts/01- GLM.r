#Linear Model 1- Team as Factor, no intercept. Not a bad fit!
lm1 <- lm(score ~ factor(opp) -1 , data= data); summary(lm1)
#Residual Plot
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
par(mfrow=c(2,2)) #better ordering of graphs
plot(lm1, which = 1:4)
shapiro.test(lm1$residuals)

#Predicting scores- factors, so not line of best fit
teams <- unique(data$opp)
newdata = data.frame(factor(teams)); names(newdata) <- 'opp'
lmdata <- predict(lm1, newdata, interval="predict")
games <- 1:20
lmdata <- as.data.frame(cbind(lmdata, 1:20))
names(lmdata) <- c("mean", "lwr", "upr", "opp_team"); head(lmdata)

#Plotting 95% intervals for each opposing team
par(mfrow=c(1,1))
plot(lmdata$opp_team, lmdata$mean, type = "n", ylim=c(-15, 15), main ="OLS Regression") #note enlarging y-axis
segments(lmdata$opp_team, lmdata$mean + lmdata$lwr, lmdata$opp_team, lmdata$mean + lmdata$upr)
points(data$opp_team, data$score, col = "red", pch=4)
points(lmdata$opp_team, lmdata$mean, col = "blue", pch=19)
abline(h = 0, col = "red")

