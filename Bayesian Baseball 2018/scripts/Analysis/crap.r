#1. Use to estimate strength of batting, pitching, etc. 
#2. Redo using actual data to plot points earned. Go ahead and combine home and away games.
d2 <- m2@coef; d2 <- as.data.frame(m2@coef[]); d2$names <- rownames(d2)
rownames(d2) <- NULL
d3<- d2[206:235,1]; d3 <- as.data.frame(d3)
d3$ba <- d2[236:265, 1]
d3$ca <- d2[266:295, 1]
names(d3) <- c('aa', 'ba', 'ca')
d3$t <- d3$aa + d3$ba + d3$ca
plot(c(1:30), d3$t)

#plot estimates
plot(precis(m1)) 
