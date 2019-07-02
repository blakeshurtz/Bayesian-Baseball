library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
chc_bat <- read.csv("chc_bat.txt")

team <- 'CHC' #team name variable

###chc_homeaway for home/away###
chc_homeaway <- chc_bat %>% select(Date, Opp, X)
###create home variable
chc_homeaway$X <- as.character(chc_homeaway$X)
chc_homeaway$Opp <- as.character(chc_homeaway$Opp)
chc_homeaway$X <- ifelse(chc_homeaway$X =='@', chc_homeaway$Opp, chc_homeaway$X)
chc_homeaway$X <- ifelse(chc_homeaway$X =='', team, chc_homeaway$X) #use of team variable name here
names(chc_homeaway)[names(chc_homeaway) == 'X'] <- 'home'
###create away variable
chc_homeaway$Opp <- ifelse(chc_homeaway$Opp == chc_homeaway$home, team, chc_homeaway$Opp) #use of team variable name here
names(chc_homeaway)[names(chc_homeaway) == 'Opp'] <- 'away'
###change class of Date
chc_homeaway$Date <- as.character(chc_homeaway$Date)
###select key vars
chc_homeaway <- chc_homeaway %>% select(home, away, Date) 

###chc_bat_a for batting vars (away)###
chc_bat_a <- chc_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
chc_bat_a <- chc_bat_a %>% filter(X == '@') #away games
###runs for away games###
chc_bat_a$Rslt <- as.character(chc_bat_a$Rslt) #character vector
TEMP<- strsplit(x=chc_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chc_bat_a$Raway <- substring(TEMP$V1, 2); chc_bat_a$Raway <- as.numeric(chc_bat_a$Raway)
###Creating batting predictors for away games###
chc_bat_a$BAa <- chc_bat_a$BA
chc_bat_a$PAa <- chc_bat_a$PA
chc_bat_a$ABa <- chc_bat_a$AB
chc_bat_a$Ra <- chc_bat_a$R
chc_bat_a$Ha <- chc_bat_a$H
chc_bat_a$B2Ba <- chc_bat_a$X2B
chc_bat_a$B3Ba <- chc_bat_a$X3B
chc_bat_a$BHRa <- chc_bat_a$HR
chc_bat_a$RBIa <- chc_bat_a$RBI
chc_bat_a$BBBa <- chc_bat_a$BB
chc_bat_a$BSOa <- chc_bat_a$SO
chc_bat_a$SHa <- chc_bat_a$SH
chc_bat_a$SFa <- chc_bat_a$SF
chc_bat_a$SBa <- chc_bat_a$SB
###create away variable###
chc_bat_a$X <- ifelse(chc_bat_a$X =='@', team, chc_bat_a$X) #use of team variable name here
names(chc_bat_a)[names(chc_bat_a) == 'X'] <- 'away'
###change class of Date
chc_bat_a$Date <- as.character(chc_bat_a$Date)
###select key vars
chc_bat_a <- chc_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###chc_bat_h for batting vars (home)###
chc_bat_h <- chc_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
chc_bat_h <- chc_bat_h %>% filter(X == '') #home games
###runs for home games###
chc_bat_h$Rslt <- as.character(chc_bat_h$Rslt) #character vector
TEMP<- strsplit(x=chc_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chc_bat_h$Rhome <- substring(TEMP$V1, 2); chc_bat_h$Rhome <- as.numeric(chc_bat_h$Rhome)
###Creating batting predictors for home games###
chc_bat_h$BAh <- chc_bat_h$BA
chc_bat_h$PAh <- chc_bat_h$PA
chc_bat_h$ABh <- chc_bat_h$AB
chc_bat_h$Rh <- chc_bat_h$R
chc_bat_h$Hh <- chc_bat_h$H
chc_bat_h$B2Bh <- chc_bat_h$X2B
chc_bat_h$B3Bh <- chc_bat_h$X3B
chc_bat_h$BHRh <- chc_bat_h$HR
chc_bat_h$RBIh <- chc_bat_h$RBI
chc_bat_h$BBBh <- chc_bat_h$BB
chc_bat_h$BSOh <- chc_bat_h$SO
chc_bat_h$SHh <- chc_bat_h$SH
chc_bat_h$SFh <- chc_bat_h$SF
chc_bat_h$SBh <- chc_bat_h$SB
###create home variable###
chc_bat_h$X <- ifelse(chc_bat_h$X =='', team, chc_bat_h$X) #use of team variable name here
names(chc_bat_h)[names(chc_bat_h) == 'X'] <- 'home'
###change class of Date
chc_bat_h$Date <- as.character(chc_bat_h$Date)
###select key vars
chc_bat_h <- chc_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: chc_pit###
chc_pit <- read.csv("chc_pit.txt")

###chc_pit_a for pitching variables (away)###
chc_pit_a <- chc_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
chc_pit_a <- chc_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(chc_pit_a)[names(chc_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(chc_pit_a)[names(chc_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
chc_pit_a$ERa <- chc_pit_a$ER
chc_pit_a$UERa <- chc_pit_a$UER
chc_pit_a$PBBa <- chc_pit_a$BB
chc_pit_a$PSOa <- chc_pit_a$SO
chc_pit_a$PHRa <- chc_pit_a$HR
chc_pit_a$BFa <- chc_pit_a$BF
chc_pit_a$Pita <- chc_pit_a$Pit
chc_pit_a$Stra <- chc_pit_a$Str
chc_pit_a$P2Ba <- chc_pit_a$X2B
chc_pit_a$P3Ba <- chc_pit_a$X3B
###create away variable###
chc_pit_a$X <- ifelse(chc_pit_a$X =='@', team, chc_pit_a$X) #use of team variable name here
names(chc_pit_a)[names(chc_pit_a) == 'X'] <- 'away'
###change class of Date
chc_pit_a$Date <- as.character(chc_pit_a$Date)
###select key vars
chc_pit_a <- chc_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###chc_pit_h for pitching variables (home)###
chc_pit_h <- chc_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
chc_pit_h <- chc_pit_h %>% filter(X == '') #home games
###splitting chc_pit_h vector###
names(chc_pit_h)[names(chc_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(chc_pit_h)[names(chc_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
chc_pit_h$ERh <- chc_pit_h$ER
chc_pit_h$UERh <- chc_pit_h$UER
chc_pit_h$PBBh <- chc_pit_h$BB
chc_pit_h$PSOh <- chc_pit_h$SO
chc_pit_h$PHRh <- chc_pit_h$HR
chc_pit_h$BFh <- chc_pit_h$BF
chc_pit_h$Pith <- chc_pit_h$Pit
chc_pit_h$Strh <- chc_pit_h$Str
chc_pit_h$P2Bh <- chc_pit_h$X2B
chc_pit_h$P3Bh <- chc_pit_h$X3B
###create home variable###
chc_pit_h$X <- ifelse(chc_pit_h$X =='', team, chc_pit_h$X) #use of team variable name here
names(chc_pit_h)[names(chc_pit_h) == 'X'] <- 'home'
###change class of Date
chc_pit_h$Date <- as.character(chc_pit_h$Date)
###filter down data
chc_pit_h <- chc_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
