library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
bal_bat <- read.csv("bal_bat.txt")

team <- 'BAL' #team name variable

###bal_homeaway for home/away###
bal_homeaway <- bal_bat %>% select(Date, Opp, X)
###create home variable
bal_homeaway$X <- as.character(bal_homeaway$X)
bal_homeaway$Opp <- as.character(bal_homeaway$Opp)
bal_homeaway$X <- ifelse(bal_homeaway$X =='@', bal_homeaway$Opp, bal_homeaway$X)
bal_homeaway$X <- ifelse(bal_homeaway$X =='', team, bal_homeaway$X) #use of team variable name here
names(bal_homeaway)[names(bal_homeaway) == 'X'] <- 'home'
###create away variable
bal_homeaway$Opp <- ifelse(bal_homeaway$Opp == bal_homeaway$home, team, bal_homeaway$Opp) #use of team variable name here
names(bal_homeaway)[names(bal_homeaway) == 'Opp'] <- 'away'
###change class of Date
bal_homeaway$Date <- as.character(bal_homeaway$Date)
###select key vars
bal_homeaway <- bal_homeaway %>% select(home, away, Date) 

###bal_bat_a for batting vars (away)###
bal_bat_a <- bal_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
bal_bat_a <- bal_bat_a %>% filter(X == '@') #away games
###runs for away games###
bal_bat_a$Rslt <- as.character(bal_bat_a$Rslt) #character vector
TEMP<- strsplit(x=bal_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bal_bat_a$Raway <- substring(TEMP$V1, 2); bal_bat_a$Raway <- as.numeric(bal_bat_a$Raway)
###Creating batting predictors for away games###
bal_bat_a$BAa <- bal_bat_a$BA
bal_bat_a$PAa <- bal_bat_a$PA
bal_bat_a$ABa <- bal_bat_a$AB
bal_bat_a$Ra <- bal_bat_a$R
bal_bat_a$Ha <- bal_bat_a$H
bal_bat_a$B2Ba <- bal_bat_a$X2B
bal_bat_a$B3Ba <- bal_bat_a$X3B
bal_bat_a$BHRa <- bal_bat_a$HR
bal_bat_a$RBIa <- bal_bat_a$RBI
bal_bat_a$BBBa <- bal_bat_a$BB
bal_bat_a$BSOa <- bal_bat_a$SO
bal_bat_a$SHa <- bal_bat_a$SH
bal_bat_a$SFa <- bal_bat_a$SF
bal_bat_a$SBa <- bal_bat_a$SB
###create away variable###
bal_bat_a$X <- ifelse(bal_bat_a$X =='@', team, bal_bat_a$X) #use of team variable name here
names(bal_bat_a)[names(bal_bat_a) == 'X'] <- 'away'
###change class of Date
bal_bat_a$Date <- as.character(bal_bat_a$Date)
###select key vars
bal_bat_a <- bal_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###bal_bat_h for batting vars (home)###
bal_bat_h <- bal_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
bal_bat_h <- bal_bat_h %>% filter(X == '') #home games
###runs for home games###
bal_bat_h$Rslt <- as.character(bal_bat_h$Rslt) #character vector
TEMP<- strsplit(x=bal_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bal_bat_h$Rhome <- substring(TEMP$V1, 2); bal_bat_h$Rhome <- as.numeric(bal_bat_h$Rhome)
###Creating batting predictors for home games###
bal_bat_h$BAh <- bal_bat_h$BA
bal_bat_h$PAh <- bal_bat_h$PA
bal_bat_h$ABh <- bal_bat_h$AB
bal_bat_h$Rh <- bal_bat_h$R
bal_bat_h$Hh <- bal_bat_h$H
bal_bat_h$B2Bh <- bal_bat_h$X2B
bal_bat_h$B3Bh <- bal_bat_h$X3B
bal_bat_h$BHRh <- bal_bat_h$HR
bal_bat_h$RBIh <- bal_bat_h$RBI
bal_bat_h$BBBh <- bal_bat_h$BB
bal_bat_h$BSOh <- bal_bat_h$SO
bal_bat_h$SHh <- bal_bat_h$SH
bal_bat_h$SFh <- bal_bat_h$SF
bal_bat_h$SBh <- bal_bat_h$SB
###create home variable###
bal_bat_h$X <- ifelse(bal_bat_h$X =='', team, bal_bat_h$X) #use of team variable name here
names(bal_bat_h)[names(bal_bat_h) == 'X'] <- 'home'
###change class of Date
bal_bat_h$Date <- as.character(bal_bat_h$Date)
###select key vars
bal_bat_h <- bal_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: bal_pit###
bal_pit <- read.csv("bal_pit.txt")

###bal_pit_a for pitching variables (away)###
bal_pit_a <- bal_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
bal_pit_a <- bal_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(bal_pit_a)[names(bal_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(bal_pit_a)[names(bal_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
bal_pit_a$ERa <- bal_pit_a$ER
bal_pit_a$UERa <- bal_pit_a$UER
bal_pit_a$PBBa <- bal_pit_a$BB
bal_pit_a$PSOa <- bal_pit_a$SO
bal_pit_a$PHRa <- bal_pit_a$HR
bal_pit_a$BFa <- bal_pit_a$BF
bal_pit_a$Pita <- bal_pit_a$Pit
bal_pit_a$Stra <- bal_pit_a$Str
bal_pit_a$P2Ba <- bal_pit_a$X2B
bal_pit_a$P3Ba <- bal_pit_a$X3B
###create away variable###
bal_pit_a$X <- ifelse(bal_pit_a$X =='@', team, bal_pit_a$X) #use of team variable name here
names(bal_pit_a)[names(bal_pit_a) == 'X'] <- 'away'
###change class of Date
bal_pit_a$Date <- as.character(bal_pit_a$Date)
###select key vars
bal_pit_a <- bal_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###bal_pit_h for pitching variables (home)###
bal_pit_h <- bal_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
bal_pit_h <- bal_pit_h %>% filter(X == '') #home games
###splitting bal_pit_h vector###
names(bal_pit_h)[names(bal_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(bal_pit_h)[names(bal_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
bal_pit_h$ERh <- bal_pit_h$ER
bal_pit_h$UERh <- bal_pit_h$UER
bal_pit_h$PBBh <- bal_pit_h$BB
bal_pit_h$PSOh <- bal_pit_h$SO
bal_pit_h$PHRh <- bal_pit_h$HR
bal_pit_h$BFh <- bal_pit_h$BF
bal_pit_h$Pith <- bal_pit_h$Pit
bal_pit_h$Strh <- bal_pit_h$Str
bal_pit_h$P2Bh <- bal_pit_h$X2B
bal_pit_h$P3Bh <- bal_pit_h$X3B
###create home variable###
bal_pit_h$X <- ifelse(bal_pit_h$X =='', team, bal_pit_h$X) #use of team variable name here
names(bal_pit_h)[names(bal_pit_h) == 'X'] <- 'home'
###change class of Date
bal_pit_h$Date <- as.character(bal_pit_h$Date)
###filter down data
bal_pit_h <- bal_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
