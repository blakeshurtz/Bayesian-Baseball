###do this after wrangling batting data
###import data manually
###example done with ari 
library(tidyverse)
library(rethinking)
ari_pit <- ari_pit %>% select(Gtm, H, R)
###transforming home/away variable
'
ari_pit$X <- as.character(ari_pit$X)
ari_pit$X[ari_pit$X=="@"] <- "away"
ari_pit$X[ari_pit$X==""] <- "home"
'
###renaming some variables
names(ari_pit)[names(ari_pit) == 'X'] <- 'loc_block'
names(ari_pit)[names(ari_pit) == 'R'] <- 'RSA'
names(ari_pit)[names(ari_pit) == 'H'] <- 'HHA'
###change class of Gtm
ari_pit$Gtm <- as.character(ari_pit$Gtm)
class(ari_pit$Gtm)
###join pitching with batting
mydata <- right_join(mydata, ari_pit, by='Gtm')
###scale data
mydata$BAscale <- scale(mydata$BA)
mydata$rdiffscale <- scale(mydata$rdiff)
mydata$HHAscale <- scale(ari_pit$HHA)
