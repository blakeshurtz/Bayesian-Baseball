library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
wsn_bat <- read.csv("wsn_bat.txt")

team <- 'WSN' #team name variable

###wsn_homeaway for home/away###
wsn_homeaway <- wsn_bat %>% select(Date, Opp, X)
###create home variable
wsn_homeaway$X <- as.character(wsn_homeaway$X)
wsn_homeaway$Opp <- as.character(wsn_homeaway$Opp)
wsn_homeaway$X <- ifelse(wsn_homeaway$X =='@', wsn_homeaway$Opp, wsn_homeaway$X)
wsn_homeaway$X <- ifelse(wsn_homeaway$X =='', team, wsn_homeaway$X) #use of team variable name here
names(wsn_homeaway)[names(wsn_homeaway) == 'X'] <- 'home'
###create away variable
wsn_homeaway$Opp <- ifelse(wsn_homeaway$Opp == wsn_homeaway$home, team, wsn_homeaway$Opp) #use of team variable name here
names(wsn_homeaway)[names(wsn_homeaway) == 'Opp'] <- 'away'
###change class of Date
wsn_homeaway$Date <- as.character(wsn_homeaway$Date)
###select key vars
wsn_homeaway <- wsn_homeaway %>% select(home, away, Date) 

###wsn_bat_a for batting vars (away)###
wsn_bat_a <- wsn_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
wsn_bat_a <- wsn_bat_a %>% filter(X == '@') #away games
###runs for away games###
wsn_bat_a$Rslt <- as.character(wsn_bat_a$Rslt) #character vector
TEMP<- strsplit(x=wsn_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
wsn_bat_a$Raway <- substring(TEMP$V1, 2); wsn_bat_a$Raway <- as.numeric(wsn_bat_a$Raway)
###Creating batting predictors for away games###
wsn_bat_a$BAa <- wsn_bat_a$BA
wsn_bat_a$PAa <- wsn_bat_a$PA
wsn_bat_a$ABa <- wsn_bat_a$AB
wsn_bat_a$Ra <- wsn_bat_a$R
wsn_bat_a$Ha <- wsn_bat_a$H
wsn_bat_a$B2Ba <- wsn_bat_a$X2B
wsn_bat_a$B3Ba <- wsn_bat_a$X3B
wsn_bat_a$BHRa <- wsn_bat_a$HR
wsn_bat_a$RBIa <- wsn_bat_a$RBI
wsn_bat_a$BBBa <- wsn_bat_a$BB
wsn_bat_a$BSOa <- wsn_bat_a$SO
wsn_bat_a$SHa <- wsn_bat_a$SH
wsn_bat_a$SFa <- wsn_bat_a$SF
wsn_bat_a$SBa <- wsn_bat_a$SB
###create away variable###
wsn_bat_a$X <- ifelse(wsn_bat_a$X =='@', team, wsn_bat_a$X) #use of team variable name here
names(wsn_bat_a)[names(wsn_bat_a) == 'X'] <- 'away'
###change class of Date
wsn_bat_a$Date <- as.character(wsn_bat_a$Date)
###select key vars
wsn_bat_a <- wsn_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###wsn_bat_h for batting vars (home)###
wsn_bat_h <- wsn_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
wsn_bat_h <- wsn_bat_h %>% filter(X == '') #home games
###runs for home games###
wsn_bat_h$Rslt <- as.character(wsn_bat_h$Rslt) #character vector
TEMP<- strsplit(x=wsn_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
wsn_bat_h$Rhome <- substring(TEMP$V1, 2); wsn_bat_h$Rhome <- as.numeric(wsn_bat_h$Rhome)
###Creating batting predictors for home games###
wsn_bat_h$BAh <- wsn_bat_h$BA
wsn_bat_h$PAh <- wsn_bat_h$PA
wsn_bat_h$ABh <- wsn_bat_h$AB
wsn_bat_h$Rh <- wsn_bat_h$R
wsn_bat_h$Hh <- wsn_bat_h$H
wsn_bat_h$B2Bh <- wsn_bat_h$X2B
wsn_bat_h$B3Bh <- wsn_bat_h$X3B
wsn_bat_h$BHRh <- wsn_bat_h$HR
wsn_bat_h$RBIh <- wsn_bat_h$RBI
wsn_bat_h$BBBh <- wsn_bat_h$BB
wsn_bat_h$BSOh <- wsn_bat_h$SO
wsn_bat_h$SHh <- wsn_bat_h$SH
wsn_bat_h$SFh <- wsn_bat_h$SF
wsn_bat_h$SBh <- wsn_bat_h$SB
###create home variable###
wsn_bat_h$X <- ifelse(wsn_bat_h$X =='', team, wsn_bat_h$X) #use of team variable name here
names(wsn_bat_h)[names(wsn_bat_h) == 'X'] <- 'home'
###change class of Date
wsn_bat_h$Date <- as.character(wsn_bat_h$Date)
###select key vars
wsn_bat_h <- wsn_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: wsn_pit###
wsn_pit <- read.csv("wsn_pit.txt")

###wsn_pit_a for pitching variables (away)###
wsn_pit_a <- wsn_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
wsn_pit_a <- wsn_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(wsn_pit_a)[names(wsn_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(wsn_pit_a)[names(wsn_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
wsn_pit_a$ERa <- wsn_pit_a$ER
wsn_pit_a$UERa <- wsn_pit_a$UER
wsn_pit_a$PBBa <- wsn_pit_a$BB
wsn_pit_a$PSOa <- wsn_pit_a$SO
wsn_pit_a$PHRa <- wsn_pit_a$HR
wsn_pit_a$BFa <- wsn_pit_a$BF
wsn_pit_a$Pita <- wsn_pit_a$Pit
wsn_pit_a$Stra <- wsn_pit_a$Str
wsn_pit_a$P2Ba <- wsn_pit_a$X2B
wsn_pit_a$P3Ba <- wsn_pit_a$X3B
###create away variable###
wsn_pit_a$X <- ifelse(wsn_pit_a$X =='@', team, wsn_pit_a$X) #use of team variable name here
names(wsn_pit_a)[names(wsn_pit_a) == 'X'] <- 'away'
###change class of Date
wsn_pit_a$Date <- as.character(wsn_pit_a$Date)
###select key vars
wsn_pit_a <- wsn_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###wsn_pit_h for pitching variables (home)###
wsn_pit_h <- wsn_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
wsn_pit_h <- wsn_pit_h %>% filter(X == '') #home games
###splitting wsn_pit_h vector###
names(wsn_pit_h)[names(wsn_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(wsn_pit_h)[names(wsn_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
wsn_pit_h$ERh <- wsn_pit_h$ER
wsn_pit_h$UERh <- wsn_pit_h$UER
wsn_pit_h$PBBh <- wsn_pit_h$BB
wsn_pit_h$PSOh <- wsn_pit_h$SO
wsn_pit_h$PHRh <- wsn_pit_h$HR
wsn_pit_h$BFh <- wsn_pit_h$BF
wsn_pit_h$Pith <- wsn_pit_h$Pit
wsn_pit_h$Strh <- wsn_pit_h$Str
wsn_pit_h$P2Bh <- wsn_pit_h$X2B
wsn_pit_h$P3Bh <- wsn_pit_h$X3B
###create home variable###
wsn_pit_h$X <- ifelse(wsn_pit_h$X =='', team, wsn_pit_h$X) #use of team variable name here
names(wsn_pit_h)[names(wsn_pit_h) == 'X'] <- 'home'
###change class of Date
wsn_pit_h$Date <- as.character(wsn_pit_h$Date)
###filter down data
wsn_pit_h <- wsn_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
