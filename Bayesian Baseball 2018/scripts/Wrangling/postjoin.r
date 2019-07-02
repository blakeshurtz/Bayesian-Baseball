mydata <- na.omit(mydata)
mydata <- as.data.frame(mydata)
mydata$rdiff = mydata$Rhome - mydata$Raway

mydata$PO_h <- scale(mydata$PO_h)
mydata$A_h <- scale(mydata$A_h)
mydata$E_h <- scale(mydata$E_h)
mydata$DP_h <- scale(mydata$DP_h)
mydata$PO_a <- scale(mydata$PO_a)
mydata$A_a <- scale(mydata$A_a)
mydata$E_a <- scale(mydata$E_a)
mydata$DP_a <- scale(mydata$DP_a)

write.csv(mydata, "mydata.csv")
