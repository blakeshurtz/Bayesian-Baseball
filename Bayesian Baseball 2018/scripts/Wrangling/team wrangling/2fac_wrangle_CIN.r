library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
cin_bat <- read.csv("cin_bat.txt")

team <- 'CIN' #team name variable

###cin_homeaway for home/away###
cin_homeaway <- cin_bat %>% select(Date, Opp, X)
###create home variable
cin_homeaway$X <- as.character(cin_homeaway$X)
cin_homeaway$Opp <- as.character(cin_homeaway$Opp)
cin_homeaway$X <- ifelse(cin_homeaway$X =='@', cin_homeaway$Opp, cin_homeaway$X)
cin_homeaway$X <- ifelse(cin_homeaway$X =='', team, cin_homeaway$X) #use of team variable name here
names(cin_homeaway)[names(cin_homeaway) == 'X'] <- 'home'
###create away variable
cin_homeaway$Opp <- ifelse(cin_homeaway$Opp == cin_homeaway$home, team, cin_homeaway$Opp) #use of team variable name here
names(cin_homeaway)[names(cin_homeaway) == 'Opp'] <- 'away'
###change class of Date
cin_homeaway$Date <- as.character(cin_homeaway$Date)
###select key vars
cin_homeaway <- cin_homeaway %>% select(home, away, Date) 

###cin_bat_a for batting vars (away)###
cin_bat_a <- cin_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
cin_bat_a <- cin_bat_a %>% filter(X == '@') #away games
###runs for away games###
cin_bat_a$Rslt <- as.character(cin_bat_a$Rslt) #character vector
TEMP<- strsplit(x=cin_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cin_bat_a$Raway <- substring(TEMP$V1, 2); cin_bat_a$Raway <- as.numeric(cin_bat_a$Raway)
###Creating batting predictors for away games###
cin_bat_a$BAa <- cin_bat_a$BA
cin_bat_a$PAa <- cin_bat_a$PA
cin_bat_a$ABa <- cin_bat_a$AB
cin_bat_a$Ra <- cin_bat_a$R
cin_bat_a$Ha <- cin_bat_a$H
cin_bat_a$B2Ba <- cin_bat_a$X2B
cin_bat_a$B3Ba <- cin_bat_a$X3B
cin_bat_a$BHRa <- cin_bat_a$HR
cin_bat_a$RBIa <- cin_bat_a$RBI
cin_bat_a$BBBa <- cin_bat_a$BB
cin_bat_a$BSOa <- cin_bat_a$SO
cin_bat_a$SHa <- cin_bat_a$SH
cin_bat_a$SFa <- cin_bat_a$SF
cin_bat_a$SBa <- cin_bat_a$SB
###create away variable###
cin_bat_a$X <- ifelse(cin_bat_a$X =='@', team, cin_bat_a$X) #use of team variable name here
names(cin_bat_a)[names(cin_bat_a) == 'X'] <- 'away'
###change class of Date
cin_bat_a$Date <- as.character(cin_bat_a$Date)
###select key vars
cin_bat_a <- cin_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###cin_bat_h for batting vars (home)###
cin_bat_h <- cin_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
cin_bat_h <- cin_bat_h %>% filter(X == '') #home games
###runs for home games###
cin_bat_h$Rslt <- as.character(cin_bat_h$Rslt) #character vector
TEMP<- strsplit(x=cin_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cin_bat_h$Rhome <- substring(TEMP$V1, 2); cin_bat_h$Rhome <- as.numeric(cin_bat_h$Rhome)
###Creating batting predictors for home games###
cin_bat_h$BAh <- cin_bat_h$BA
cin_bat_h$PAh <- cin_bat_h$PA
cin_bat_h$ABh <- cin_bat_h$AB
cin_bat_h$Rh <- cin_bat_h$R
cin_bat_h$Hh <- cin_bat_h$H
cin_bat_h$B2Bh <- cin_bat_h$X2B
cin_bat_h$B3Bh <- cin_bat_h$X3B
cin_bat_h$BHRh <- cin_bat_h$HR
cin_bat_h$RBIh <- cin_bat_h$RBI
cin_bat_h$BBBh <- cin_bat_h$BB
cin_bat_h$BSOh <- cin_bat_h$SO
cin_bat_h$SHh <- cin_bat_h$SH
cin_bat_h$SFh <- cin_bat_h$SF
cin_bat_h$SBh <- cin_bat_h$SB
###create home variable###
cin_bat_h$X <- ifelse(cin_bat_h$X =='', team, cin_bat_h$X) #use of team variable name here
names(cin_bat_h)[names(cin_bat_h) == 'X'] <- 'home'
###change class of Date
cin_bat_h$Date <- as.character(cin_bat_h$Date)
###select key vars
cin_bat_h <- cin_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: cin_pit###
cin_pit <- read.csv("cin_pit.txt")

###cin_pit_a for pitching variables (away)###
cin_pit_a <- cin_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
cin_pit_a <- cin_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(cin_pit_a)[names(cin_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(cin_pit_a)[names(cin_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
cin_pit_a$ERa <- cin_pit_a$ER
cin_pit_a$UERa <- cin_pit_a$UER
cin_pit_a$PBBa <- cin_pit_a$BB
cin_pit_a$PSOa <- cin_pit_a$SO
cin_pit_a$PHRa <- cin_pit_a$HR
cin_pit_a$BFa <- cin_pit_a$BF
cin_pit_a$Pita <- cin_pit_a$Pit
cin_pit_a$Stra <- cin_pit_a$Str
cin_pit_a$P2Ba <- cin_pit_a$X2B
cin_pit_a$P3Ba <- cin_pit_a$X3B
###create away variable###
cin_pit_a$X <- ifelse(cin_pit_a$X =='@', team, cin_pit_a$X) #use of team variable name here
names(cin_pit_a)[names(cin_pit_a) == 'X'] <- 'away'
###change class of Date
cin_pit_a$Date <- as.character(cin_pit_a$Date)
###select key vars
cin_pit_a <- cin_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###cin_pit_h for pitching variables (home)###
cin_pit_h <- cin_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
cin_pit_h <- cin_pit_h %>% filter(X == '') #home games
###splitting cin_pit_h vector###
names(cin_pit_h)[names(cin_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(cin_pit_h)[names(cin_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
cin_pit_h$ERh <- cin_pit_h$ER
cin_pit_h$UERh <- cin_pit_h$UER
cin_pit_h$PBBh <- cin_pit_h$BB
cin_pit_h$PSOh <- cin_pit_h$SO
cin_pit_h$PHRh <- cin_pit_h$HR
cin_pit_h$BFh <- cin_pit_h$BF
cin_pit_h$Pith <- cin_pit_h$Pit
cin_pit_h$Strh <- cin_pit_h$Str
cin_pit_h$P2Bh <- cin_pit_h$X2B
cin_pit_h$P3Bh <- cin_pit_h$X3B
###create home variable###
cin_pit_h$X <- ifelse(cin_pit_h$X =='', team, cin_pit_h$X) #use of team variable name here
names(cin_pit_h)[names(cin_pit_h) == 'X'] <- 'home'
###change class of Date
cin_pit_h$Date <- as.character(cin_pit_h$Date)
###filter down data
cin_pit_h <- cin_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
