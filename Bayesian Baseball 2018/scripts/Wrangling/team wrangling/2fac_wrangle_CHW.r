library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
chw_bat <- read.csv("chw_bat.txt")

team <- 'CHW' #team name variable

###chw_homeaway for home/away###
chw_homeaway <- chw_bat %>% select(Date, Opp, X)
###create home variable
chw_homeaway$X <- as.character(chw_homeaway$X)
chw_homeaway$Opp <- as.character(chw_homeaway$Opp)
chw_homeaway$X <- ifelse(chw_homeaway$X =='@', chw_homeaway$Opp, chw_homeaway$X)
chw_homeaway$X <- ifelse(chw_homeaway$X =='', team, chw_homeaway$X) #use of team variable name here
names(chw_homeaway)[names(chw_homeaway) == 'X'] <- 'home'
###create away variable
chw_homeaway$Opp <- ifelse(chw_homeaway$Opp == chw_homeaway$home, team, chw_homeaway$Opp) #use of team variable name here
names(chw_homeaway)[names(chw_homeaway) == 'Opp'] <- 'away'
###change class of Date
chw_homeaway$Date <- as.character(chw_homeaway$Date)
###select key vars
chw_homeaway <- chw_homeaway %>% select(home, away, Date) 

###chw_bat_a for batting vars (away)###
chw_bat_a <- chw_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
chw_bat_a <- chw_bat_a %>% filter(X == '@') #away games
###runs for away games###
chw_bat_a$Rslt <- as.character(chw_bat_a$Rslt) #character vector
TEMP<- strsplit(x=chw_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chw_bat_a$Raway <- substring(TEMP$V1, 2); chw_bat_a$Raway <- as.numeric(chw_bat_a$Raway)
###Creating batting predictors for away games###
chw_bat_a$BAa <- chw_bat_a$BA
chw_bat_a$PAa <- chw_bat_a$PA
chw_bat_a$ABa <- chw_bat_a$AB
chw_bat_a$Ra <- chw_bat_a$R
chw_bat_a$Ha <- chw_bat_a$H
chw_bat_a$B2Ba <- chw_bat_a$X2B
chw_bat_a$B3Ba <- chw_bat_a$X3B
chw_bat_a$BHRa <- chw_bat_a$HR
chw_bat_a$RBIa <- chw_bat_a$RBI
chw_bat_a$BBBa <- chw_bat_a$BB
chw_bat_a$BSOa <- chw_bat_a$SO
chw_bat_a$SHa <- chw_bat_a$SH
chw_bat_a$SFa <- chw_bat_a$SF
chw_bat_a$SBa <- chw_bat_a$SB
###create away variable###
chw_bat_a$X <- ifelse(chw_bat_a$X =='@', team, chw_bat_a$X) #use of team variable name here
names(chw_bat_a)[names(chw_bat_a) == 'X'] <- 'away'
###change class of Date
chw_bat_a$Date <- as.character(chw_bat_a$Date)
###select key vars
chw_bat_a <- chw_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###chw_bat_h for batting vars (home)###
chw_bat_h <- chw_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
chw_bat_h <- chw_bat_h %>% filter(X == '') #home games
###runs for home games###
chw_bat_h$Rslt <- as.character(chw_bat_h$Rslt) #character vector
TEMP<- strsplit(x=chw_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chw_bat_h$Rhome <- substring(TEMP$V1, 2); chw_bat_h$Rhome <- as.numeric(chw_bat_h$Rhome)
###Creating batting predictors for home games###
chw_bat_h$BAh <- chw_bat_h$BA
chw_bat_h$PAh <- chw_bat_h$PA
chw_bat_h$ABh <- chw_bat_h$AB
chw_bat_h$Rh <- chw_bat_h$R
chw_bat_h$Hh <- chw_bat_h$H
chw_bat_h$B2Bh <- chw_bat_h$X2B
chw_bat_h$B3Bh <- chw_bat_h$X3B
chw_bat_h$BHRh <- chw_bat_h$HR
chw_bat_h$RBIh <- chw_bat_h$RBI
chw_bat_h$BBBh <- chw_bat_h$BB
chw_bat_h$BSOh <- chw_bat_h$SO
chw_bat_h$SHh <- chw_bat_h$SH
chw_bat_h$SFh <- chw_bat_h$SF
chw_bat_h$SBh <- chw_bat_h$SB
###create home variable###
chw_bat_h$X <- ifelse(chw_bat_h$X =='', team, chw_bat_h$X) #use of team variable name here
names(chw_bat_h)[names(chw_bat_h) == 'X'] <- 'home'
###change class of Date
chw_bat_h$Date <- as.character(chw_bat_h$Date)
###select key vars
chw_bat_h <- chw_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: chw_pit###
chw_pit <- read.csv("chw_pit.txt")

###chw_pit_a for pitching variables (away)###
chw_pit_a <- chw_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
chw_pit_a <- chw_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(chw_pit_a)[names(chw_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(chw_pit_a)[names(chw_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
chw_pit_a$ERa <- chw_pit_a$ER
chw_pit_a$UERa <- chw_pit_a$UER
chw_pit_a$PBBa <- chw_pit_a$BB
chw_pit_a$PSOa <- chw_pit_a$SO
chw_pit_a$PHRa <- chw_pit_a$HR
chw_pit_a$BFa <- chw_pit_a$BF
chw_pit_a$Pita <- chw_pit_a$Pit
chw_pit_a$Stra <- chw_pit_a$Str
chw_pit_a$P2Ba <- chw_pit_a$X2B
chw_pit_a$P3Ba <- chw_pit_a$X3B
###create away variable###
chw_pit_a$X <- ifelse(chw_pit_a$X =='@', team, chw_pit_a$X) #use of team variable name here
names(chw_pit_a)[names(chw_pit_a) == 'X'] <- 'away'
###change class of Date
chw_pit_a$Date <- as.character(chw_pit_a$Date)
###select key vars
chw_pit_a <- chw_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###chw_pit_h for pitching variables (home)###
chw_pit_h <- chw_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
chw_pit_h <- chw_pit_h %>% filter(X == '') #home games
###splitting chw_pit_h vector###
names(chw_pit_h)[names(chw_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(chw_pit_h)[names(chw_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
chw_pit_h$ERh <- chw_pit_h$ER
chw_pit_h$UERh <- chw_pit_h$UER
chw_pit_h$PBBh <- chw_pit_h$BB
chw_pit_h$PSOh <- chw_pit_h$SO
chw_pit_h$PHRh <- chw_pit_h$HR
chw_pit_h$BFh <- chw_pit_h$BF
chw_pit_h$Pith <- chw_pit_h$Pit
chw_pit_h$Strh <- chw_pit_h$Str
chw_pit_h$P2Bh <- chw_pit_h$X2B
chw_pit_h$P3Bh <- chw_pit_h$X3B
###create home variable###
chw_pit_h$X <- ifelse(chw_pit_h$X =='', team, chw_pit_h$X) #use of team variable name here
names(chw_pit_h)[names(chw_pit_h) == 'X'] <- 'home'
###change class of Date
chw_pit_h$Date <- as.character(chw_pit_h$Date)
###filter down data
chw_pit_h <- chw_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
