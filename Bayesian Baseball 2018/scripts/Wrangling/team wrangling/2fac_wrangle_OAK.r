library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
oak_bat <- read.csv("oak_bat.txt")

team <- 'OAK' #team name variable

###oak_homeaway for home/away###
oak_homeaway <- oak_bat %>% select(Date, Opp, X)
###create home variable
oak_homeaway$X <- as.character(oak_homeaway$X)
oak_homeaway$Opp <- as.character(oak_homeaway$Opp)
oak_homeaway$X <- ifelse(oak_homeaway$X =='@', oak_homeaway$Opp, oak_homeaway$X)
oak_homeaway$X <- ifelse(oak_homeaway$X =='', team, oak_homeaway$X) #use of team variable name here
names(oak_homeaway)[names(oak_homeaway) == 'X'] <- 'home'
###create away variable
oak_homeaway$Opp <- ifelse(oak_homeaway$Opp == oak_homeaway$home, team, oak_homeaway$Opp) #use of team variable name here
names(oak_homeaway)[names(oak_homeaway) == 'Opp'] <- 'away'
###change class of Date
oak_homeaway$Date <- as.character(oak_homeaway$Date)
###select key vars
oak_homeaway <- oak_homeaway %>% select(home, away, Date) 

###oak_bat_a for batting vars (away)###
oak_bat_a <- oak_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
oak_bat_a <- oak_bat_a %>% filter(X == '@') #away games
###runs for away games###
oak_bat_a$Rslt <- as.character(oak_bat_a$Rslt) #character vector
TEMP<- strsplit(x=oak_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
oak_bat_a$Raway <- substring(TEMP$V1, 2); oak_bat_a$Raway <- as.numeric(oak_bat_a$Raway)
###Creating batting predictors for away games###
oak_bat_a$BAa <- oak_bat_a$BA
oak_bat_a$PAa <- oak_bat_a$PA
oak_bat_a$ABa <- oak_bat_a$AB
oak_bat_a$Ra <- oak_bat_a$R
oak_bat_a$Ha <- oak_bat_a$H
oak_bat_a$B2Ba <- oak_bat_a$X2B
oak_bat_a$B3Ba <- oak_bat_a$X3B
oak_bat_a$BHRa <- oak_bat_a$HR
oak_bat_a$RBIa <- oak_bat_a$RBI
oak_bat_a$BBBa <- oak_bat_a$BB
oak_bat_a$BSOa <- oak_bat_a$SO
oak_bat_a$SHa <- oak_bat_a$SH
oak_bat_a$SFa <- oak_bat_a$SF
oak_bat_a$SBa <- oak_bat_a$SB
###create away variable###
oak_bat_a$X <- ifelse(oak_bat_a$X =='@', team, oak_bat_a$X) #use of team variable name here
names(oak_bat_a)[names(oak_bat_a) == 'X'] <- 'away'
###change class of Date
oak_bat_a$Date <- as.character(oak_bat_a$Date)
###select key vars
oak_bat_a <- oak_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###oak_bat_h for batting vars (home)###
oak_bat_h <- oak_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
oak_bat_h <- oak_bat_h %>% filter(X == '') #home games
###runs for home games###
oak_bat_h$Rslt <- as.character(oak_bat_h$Rslt) #character vector
TEMP<- strsplit(x=oak_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
oak_bat_h$Rhome <- substring(TEMP$V1, 2); oak_bat_h$Rhome <- as.numeric(oak_bat_h$Rhome)
###Creating batting predictors for home games###
oak_bat_h$BAh <- oak_bat_h$BA
oak_bat_h$PAh <- oak_bat_h$PA
oak_bat_h$ABh <- oak_bat_h$AB
oak_bat_h$Rh <- oak_bat_h$R
oak_bat_h$Hh <- oak_bat_h$H
oak_bat_h$B2Bh <- oak_bat_h$X2B
oak_bat_h$B3Bh <- oak_bat_h$X3B
oak_bat_h$BHRh <- oak_bat_h$HR
oak_bat_h$RBIh <- oak_bat_h$RBI
oak_bat_h$BBBh <- oak_bat_h$BB
oak_bat_h$BSOh <- oak_bat_h$SO
oak_bat_h$SHh <- oak_bat_h$SH
oak_bat_h$SFh <- oak_bat_h$SF
oak_bat_h$SBh <- oak_bat_h$SB
###create home variable###
oak_bat_h$X <- ifelse(oak_bat_h$X =='', team, oak_bat_h$X) #use of team variable name here
names(oak_bat_h)[names(oak_bat_h) == 'X'] <- 'home'
###change class of Date
oak_bat_h$Date <- as.character(oak_bat_h$Date)
###select key vars
oak_bat_h <- oak_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: oak_pit###
oak_pit <- read.csv("oak_pit.txt")

###oak_pit_a for pitching variables (away)###
oak_pit_a <- oak_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
oak_pit_a <- oak_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(oak_pit_a)[names(oak_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(oak_pit_a)[names(oak_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
oak_pit_a$ERa <- oak_pit_a$ER
oak_pit_a$UERa <- oak_pit_a$UER
oak_pit_a$PBBa <- oak_pit_a$BB
oak_pit_a$PSOa <- oak_pit_a$SO
oak_pit_a$PHRa <- oak_pit_a$HR
oak_pit_a$BFa <- oak_pit_a$BF
oak_pit_a$Pita <- oak_pit_a$Pit
oak_pit_a$Stra <- oak_pit_a$Str
oak_pit_a$P2Ba <- oak_pit_a$X2B
oak_pit_a$P3Ba <- oak_pit_a$X3B
###create away variable###
oak_pit_a$X <- ifelse(oak_pit_a$X =='@', team, oak_pit_a$X) #use of team variable name here
names(oak_pit_a)[names(oak_pit_a) == 'X'] <- 'away'
###change class of Date
oak_pit_a$Date <- as.character(oak_pit_a$Date)
###select key vars
oak_pit_a <- oak_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###oak_pit_h for pitching variables (home)###
oak_pit_h <- oak_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
oak_pit_h <- oak_pit_h %>% filter(X == '') #home games
###splitting oak_pit_h vector###
names(oak_pit_h)[names(oak_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(oak_pit_h)[names(oak_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
oak_pit_h$ERh <- oak_pit_h$ER
oak_pit_h$UERh <- oak_pit_h$UER
oak_pit_h$PBBh <- oak_pit_h$BB
oak_pit_h$PSOh <- oak_pit_h$SO
oak_pit_h$PHRh <- oak_pit_h$HR
oak_pit_h$BFh <- oak_pit_h$BF
oak_pit_h$Pith <- oak_pit_h$Pit
oak_pit_h$Strh <- oak_pit_h$Str
oak_pit_h$P2Bh <- oak_pit_h$X2B
oak_pit_h$P3Bh <- oak_pit_h$X3B
###create home variable###
oak_pit_h$X <- ifelse(oak_pit_h$X =='', team, oak_pit_h$X) #use of team variable name here
names(oak_pit_h)[names(oak_pit_h) == 'X'] <- 'home'
###change class of Date
oak_pit_h$Date <- as.character(oak_pit_h$Date)
###filter down data
oak_pit_h <- oak_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
