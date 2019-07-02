###import data manually
###example done with ari 
library(tidyverse)
library(rethinking)
mydata <- ari_bl %>% select(Opp, Rslt, BA, X, Gtm)
#splitting Rslt vector
class(ari_bl$Rslt)
mydata$Rslt <- as.character(mydata$Rslt) #character vector
TEMP<- strsplit(x=mydata$Rslt, "-")
TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE)
TEMP <- as.data.frame(TEMP)   
mydata$RA <- TEMP$V2
TEMP$V1 <- as.character(TEMP$V1)
mydata$R <- substring(TEMP$V1, 2); mydata$R <- as.numeric(mydata$R)
mydata$RA <- as.numeric(as.character(mydata$RA))
mydata$rdiff <- mydata$R-mydata$RA
###transforming home/away variable
mydata$X <- as.character(mydata$X)
mydata$X[mydata$X=="@"] <- "away"
mydata$X[mydata$X==""] <- "home"    
names(mydata)[names(mydata) == 'X'] <- 'loc_block'
###change class of Gtm
mydata$Gtm <- as.character(mydata$Gtm)
class(mydata$Gtm)

