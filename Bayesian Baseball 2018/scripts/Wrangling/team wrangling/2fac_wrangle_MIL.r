library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
mil_bat <- read.csv("mil_bat.txt")

team <- 'MIL' #team name variable

###mil_homeaway for home/away###
mil_homeaway <- mil_bat %>% select(Date, Opp, X)
###create home variable
mil_homeaway$X <- as.character(mil_homeaway$X)
mil_homeaway$Opp <- as.character(mil_homeaway$Opp)
mil_homeaway$X <- ifelse(mil_homeaway$X =='@', mil_homeaway$Opp, mil_homeaway$X)
mil_homeaway$X <- ifelse(mil_homeaway$X =='', team, mil_homeaway$X) #use of team variable name here
names(mil_homeaway)[names(mil_homeaway) == 'X'] <- 'home'
###create away variable
mil_homeaway$Opp <- ifelse(mil_homeaway$Opp == mil_homeaway$home, team, mil_homeaway$Opp) #use of team variable name here
names(mil_homeaway)[names(mil_homeaway) == 'Opp'] <- 'away'
###change class of Date
mil_homeaway$Date <- as.character(mil_homeaway$Date)
###select key vars
mil_homeaway <- mil_homeaway %>% select(home, away, Date) 

###mil_bat_a for batting vars (away)###
mil_bat_a <- mil_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
mil_bat_a <- mil_bat_a %>% filter(X == '@') #away games
###runs for away games###
mil_bat_a$Rslt <- as.character(mil_bat_a$Rslt) #character vector
TEMP<- strsplit(x=mil_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mil_bat_a$Raway <- substring(TEMP$V1, 2); mil_bat_a$Raway <- as.numeric(mil_bat_a$Raway)
###Creating batting predictors for away games###
mil_bat_a$BAa <- mil_bat_a$BA
mil_bat_a$PAa <- mil_bat_a$PA
mil_bat_a$ABa <- mil_bat_a$AB
mil_bat_a$Ra <- mil_bat_a$R
mil_bat_a$Ha <- mil_bat_a$H
mil_bat_a$B2Ba <- mil_bat_a$X2B
mil_bat_a$B3Ba <- mil_bat_a$X3B
mil_bat_a$BHRa <- mil_bat_a$HR
mil_bat_a$RBIa <- mil_bat_a$RBI
mil_bat_a$BBBa <- mil_bat_a$BB
mil_bat_a$BSOa <- mil_bat_a$SO
mil_bat_a$SHa <- mil_bat_a$SH
mil_bat_a$SFa <- mil_bat_a$SF
mil_bat_a$SBa <- mil_bat_a$SB
###create away variable###
mil_bat_a$X <- ifelse(mil_bat_a$X =='@', team, mil_bat_a$X) #use of team variable name here
names(mil_bat_a)[names(mil_bat_a) == 'X'] <- 'away'
###change class of Date
mil_bat_a$Date <- as.character(mil_bat_a$Date)
###select key vars
mil_bat_a <- mil_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###mil_bat_h for batting vars (home)###
mil_bat_h <- mil_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
mil_bat_h <- mil_bat_h %>% filter(X == '') #home games
###runs for home games###
mil_bat_h$Rslt <- as.character(mil_bat_h$Rslt) #character vector
TEMP<- strsplit(x=mil_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mil_bat_h$Rhome <- substring(TEMP$V1, 2); mil_bat_h$Rhome <- as.numeric(mil_bat_h$Rhome)
###Creating batting predictors for home games###
mil_bat_h$BAh <- mil_bat_h$BA
mil_bat_h$PAh <- mil_bat_h$PA
mil_bat_h$ABh <- mil_bat_h$AB
mil_bat_h$Rh <- mil_bat_h$R
mil_bat_h$Hh <- mil_bat_h$H
mil_bat_h$B2Bh <- mil_bat_h$X2B
mil_bat_h$B3Bh <- mil_bat_h$X3B
mil_bat_h$BHRh <- mil_bat_h$HR
mil_bat_h$RBIh <- mil_bat_h$RBI
mil_bat_h$BBBh <- mil_bat_h$BB
mil_bat_h$BSOh <- mil_bat_h$SO
mil_bat_h$SHh <- mil_bat_h$SH
mil_bat_h$SFh <- mil_bat_h$SF
mil_bat_h$SBh <- mil_bat_h$SB
###create home variable###
mil_bat_h$X <- ifelse(mil_bat_h$X =='', team, mil_bat_h$X) #use of team variable name here
names(mil_bat_h)[names(mil_bat_h) == 'X'] <- 'home'
###change class of Date
mil_bat_h$Date <- as.character(mil_bat_h$Date)
###select key vars
mil_bat_h <- mil_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: mil_pit###
mil_pit <- read.csv("mil_pit.txt")

###mil_pit_a for pitching variables (away)###
mil_pit_a <- mil_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
mil_pit_a <- mil_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(mil_pit_a)[names(mil_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(mil_pit_a)[names(mil_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
mil_pit_a$ERa <- mil_pit_a$ER
mil_pit_a$UERa <- mil_pit_a$UER
mil_pit_a$PBBa <- mil_pit_a$BB
mil_pit_a$PSOa <- mil_pit_a$SO
mil_pit_a$PHRa <- mil_pit_a$HR
mil_pit_a$BFa <- mil_pit_a$BF
mil_pit_a$Pita <- mil_pit_a$Pit
mil_pit_a$Stra <- mil_pit_a$Str
mil_pit_a$P2Ba <- mil_pit_a$X2B
mil_pit_a$P3Ba <- mil_pit_a$X3B
###create away variable###
mil_pit_a$X <- ifelse(mil_pit_a$X =='@', team, mil_pit_a$X) #use of team variable name here
names(mil_pit_a)[names(mil_pit_a) == 'X'] <- 'away'
###change class of Date
mil_pit_a$Date <- as.character(mil_pit_a$Date)
###select key vars
mil_pit_a <- mil_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###mil_pit_h for pitching variables (home)###
mil_pit_h <- mil_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
mil_pit_h <- mil_pit_h %>% filter(X == '') #home games
###splitting mil_pit_h vector###
names(mil_pit_h)[names(mil_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(mil_pit_h)[names(mil_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
mil_pit_h$ERh <- mil_pit_h$ER
mil_pit_h$UERh <- mil_pit_h$UER
mil_pit_h$PBBh <- mil_pit_h$BB
mil_pit_h$PSOh <- mil_pit_h$SO
mil_pit_h$PHRh <- mil_pit_h$HR
mil_pit_h$BFh <- mil_pit_h$BF
mil_pit_h$Pith <- mil_pit_h$Pit
mil_pit_h$Strh <- mil_pit_h$Str
mil_pit_h$P2Bh <- mil_pit_h$X2B
mil_pit_h$P3Bh <- mil_pit_h$X3B
###create home variable###
mil_pit_h$X <- ifelse(mil_pit_h$X =='', team, mil_pit_h$X) #use of team variable name here
names(mil_pit_h)[names(mil_pit_h) == 'X'] <- 'home'
###change class of Date
mil_pit_h$Date <- as.character(mil_pit_h$Date)
###filter down data
mil_pit_h <- mil_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
