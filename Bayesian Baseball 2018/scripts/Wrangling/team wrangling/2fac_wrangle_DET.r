library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
det_bat <- read.csv("det_bat.txt")

team <- 'DET' #team name variable

###det_homeaway for home/away###
det_homeaway <- det_bat %>% select(Date, Opp, X)
###create home variable
det_homeaway$X <- as.character(det_homeaway$X)
det_homeaway$Opp <- as.character(det_homeaway$Opp)
det_homeaway$X <- ifelse(det_homeaway$X =='@', det_homeaway$Opp, det_homeaway$X)
det_homeaway$X <- ifelse(det_homeaway$X =='', team, det_homeaway$X) #use of team variable name here
names(det_homeaway)[names(det_homeaway) == 'X'] <- 'home'
###create away variable
det_homeaway$Opp <- ifelse(det_homeaway$Opp == det_homeaway$home, team, det_homeaway$Opp) #use of team variable name here
names(det_homeaway)[names(det_homeaway) == 'Opp'] <- 'away'
###change class of Date
det_homeaway$Date <- as.character(det_homeaway$Date)
###select key vars
det_homeaway <- det_homeaway %>% select(home, away, Date) 

###det_bat_a for batting vars (away)###
det_bat_a <- det_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
det_bat_a <- det_bat_a %>% filter(X == '@') #away games
###runs for away games###
det_bat_a$Rslt <- as.character(det_bat_a$Rslt) #character vector
TEMP<- strsplit(x=det_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
det_bat_a$Raway <- substring(TEMP$V1, 2); det_bat_a$Raway <- as.numeric(det_bat_a$Raway)
###Creating batting predictors for away games###
det_bat_a$BAa <- det_bat_a$BA
det_bat_a$PAa <- det_bat_a$PA
det_bat_a$ABa <- det_bat_a$AB
det_bat_a$Ra <- det_bat_a$R
det_bat_a$Ha <- det_bat_a$H
det_bat_a$B2Ba <- det_bat_a$X2B
det_bat_a$B3Ba <- det_bat_a$X3B
det_bat_a$BHRa <- det_bat_a$HR
det_bat_a$RBIa <- det_bat_a$RBI
det_bat_a$BBBa <- det_bat_a$BB
det_bat_a$BSOa <- det_bat_a$SO
det_bat_a$SHa <- det_bat_a$SH
det_bat_a$SFa <- det_bat_a$SF
det_bat_a$SBa <- det_bat_a$SB
###create away variable###
det_bat_a$X <- ifelse(det_bat_a$X =='@', team, det_bat_a$X) #use of team variable name here
names(det_bat_a)[names(det_bat_a) == 'X'] <- 'away'
###change class of Date
det_bat_a$Date <- as.character(det_bat_a$Date)
###select key vars
det_bat_a <- det_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###det_bat_h for batting vars (home)###
det_bat_h <- det_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
det_bat_h <- det_bat_h %>% filter(X == '') #home games
###runs for home games###
det_bat_h$Rslt <- as.character(det_bat_h$Rslt) #character vector
TEMP<- strsplit(x=det_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
det_bat_h$Rhome <- substring(TEMP$V1, 2); det_bat_h$Rhome <- as.numeric(det_bat_h$Rhome)
###Creating batting predictors for home games###
det_bat_h$BAh <- det_bat_h$BA
det_bat_h$PAh <- det_bat_h$PA
det_bat_h$ABh <- det_bat_h$AB
det_bat_h$Rh <- det_bat_h$R
det_bat_h$Hh <- det_bat_h$H
det_bat_h$B2Bh <- det_bat_h$X2B
det_bat_h$B3Bh <- det_bat_h$X3B
det_bat_h$BHRh <- det_bat_h$HR
det_bat_h$RBIh <- det_bat_h$RBI
det_bat_h$BBBh <- det_bat_h$BB
det_bat_h$BSOh <- det_bat_h$SO
det_bat_h$SHh <- det_bat_h$SH
det_bat_h$SFh <- det_bat_h$SF
det_bat_h$SBh <- det_bat_h$SB
###create home variable###
det_bat_h$X <- ifelse(det_bat_h$X =='', team, det_bat_h$X) #use of team variable name here
names(det_bat_h)[names(det_bat_h) == 'X'] <- 'home'
###change class of Date
det_bat_h$Date <- as.character(det_bat_h$Date)
###select key vars
det_bat_h <- det_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: det_pit###
det_pit <- read.csv("det_pit.txt")

###det_pit_a for pitching variables (away)###
det_pit_a <- det_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
det_pit_a <- det_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(det_pit_a)[names(det_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(det_pit_a)[names(det_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
det_pit_a$ERa <- det_pit_a$ER
det_pit_a$UERa <- det_pit_a$UER
det_pit_a$PBBa <- det_pit_a$BB
det_pit_a$PSOa <- det_pit_a$SO
det_pit_a$PHRa <- det_pit_a$HR
det_pit_a$BFa <- det_pit_a$BF
det_pit_a$Pita <- det_pit_a$Pit
det_pit_a$Stra <- det_pit_a$Str
det_pit_a$P2Ba <- det_pit_a$X2B
det_pit_a$P3Ba <- det_pit_a$X3B
###create away variable###
det_pit_a$X <- ifelse(det_pit_a$X =='@', team, det_pit_a$X) #use of team variable name here
names(det_pit_a)[names(det_pit_a) == 'X'] <- 'away'
###change class of Date
det_pit_a$Date <- as.character(det_pit_a$Date)
###select key vars
det_pit_a <- det_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###det_pit_h for pitching variables (home)###
det_pit_h <- det_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
det_pit_h <- det_pit_h %>% filter(X == '') #home games
###splitting det_pit_h vector###
names(det_pit_h)[names(det_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(det_pit_h)[names(det_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
det_pit_h$ERh <- det_pit_h$ER
det_pit_h$UERh <- det_pit_h$UER
det_pit_h$PBBh <- det_pit_h$BB
det_pit_h$PSOh <- det_pit_h$SO
det_pit_h$PHRh <- det_pit_h$HR
det_pit_h$BFh <- det_pit_h$BF
det_pit_h$Pith <- det_pit_h$Pit
det_pit_h$Strh <- det_pit_h$Str
det_pit_h$P2Bh <- det_pit_h$X2B
det_pit_h$P3Bh <- det_pit_h$X3B
###create home variable###
det_pit_h$X <- ifelse(det_pit_h$X =='', team, det_pit_h$X) #use of team variable name here
names(det_pit_h)[names(det_pit_h) == 'X'] <- 'home'
###change class of Date
det_pit_h$Date <- as.character(det_pit_h$Date)
###filter down data
det_pit_h <- det_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
