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
mydata$rdiff <- mydata$R-mydata$RA #negative value indicates away team wins
###create home variable
mydata$X <- as.character(mydata$X)
mydata$Opp <- as.character(mydata$Opp)
mydata$X <- ifelse(mydata$X =='@', mydata$Opp, mydata$X)
mydata$X <- ifelse(mydata$X =='', "ARI", mydata$X)
names(mydata)[names(mydata) == 'X'] <- 'home'
###create away variable
mydata$Opp <- ifelse(mydata$Opp == mydata$home, "ARI", mydata$Opp)
names(mydata)[names(mydata) == 'Opp'] <- 'away'
###change class of Gtm
mydata$Gtm <- as.character(mydata$Gtm)
class(mydata$Gtm)
###lose unnecessary observations 
mydata <- mydata %>% select(home, away, Gtm, rdiff, BA)
names(mydata)[names(mydata) == 'BA'] <- 'ba_home'
mydata$ba_away <- runif(162, .2, .3)
###import pitching data
###do this after wrangling batting data
###import data manually
###example done with ari 
library(tidyverse)
library(rethinking)
ari_pit <- ari_pit %>% select(Gtm, H, R)
###change class of Gtm
ari_pit$Gtm <- as.character(ari_pit$Gtm)
###renaming some variables
names(ari_pit)[names(ari_pit) == 'R'] <- 'RSA_home'
names(ari_pit)[names(ari_pit) == 'H'] <- 'HHA_home'
###join pitching with batting
mydata <- right_join(mydata, ari_pit, by='Gtm')
mydata$HHA_away <- runif(162, 0, 10)
mydata$RSA_away <- runif(162, 0, 10)
###scale data
mydata$ba_home_scale <- scale(mydata$ba_home)
mydata$ba_away_scale <- scale(mydata$ba_away)
mydata$rdiffscale <- scale(mydata$rdiff)
mydata$HHA_home_scale <- scale(mydata$HHA_home)
mydata$HHA_away_scale <- scale(mydata$HHA_away)
mydata$RSA_home_scale <- scale(mydata$RSA_home)
mydata$RSA_away_scale <- scale(mydata$RSA_away)
