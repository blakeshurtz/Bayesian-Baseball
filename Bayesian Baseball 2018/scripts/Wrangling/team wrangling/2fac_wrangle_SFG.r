library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
sfg_bat <- read.csv("sfg_bat.txt")

team <- 'SFG' #team name variable

###sfg_homeaway for home/away###
sfg_homeaway <- sfg_bat %>% select(Date, Opp, X)
###create home variable
sfg_homeaway$X <- as.character(sfg_homeaway$X)
sfg_homeaway$Opp <- as.character(sfg_homeaway$Opp)
sfg_homeaway$X <- ifelse(sfg_homeaway$X =='@', sfg_homeaway$Opp, sfg_homeaway$X)
sfg_homeaway$X <- ifelse(sfg_homeaway$X =='', team, sfg_homeaway$X) #use of team variable name here
names(sfg_homeaway)[names(sfg_homeaway) == 'X'] <- 'home'
###create away variable
sfg_homeaway$Opp <- ifelse(sfg_homeaway$Opp == sfg_homeaway$home, team, sfg_homeaway$Opp) #use of team variable name here
names(sfg_homeaway)[names(sfg_homeaway) == 'Opp'] <- 'away'
###change class of Date
sfg_homeaway$Date <- as.character(sfg_homeaway$Date)
###select key vars
sfg_homeaway <- sfg_homeaway %>% select(home, away, Date) 

###sfg_bat_a for batting vars (away)###
sfg_bat_a <- sfg_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
sfg_bat_a <- sfg_bat_a %>% filter(X == '@') #away games
###runs for away games###
sfg_bat_a$Rslt <- as.character(sfg_bat_a$Rslt) #character vector
TEMP<- strsplit(x=sfg_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sfg_bat_a$Raway <- substring(TEMP$V1, 2); sfg_bat_a$Raway <- as.numeric(sfg_bat_a$Raway)
###Creating batting predictors for away games###
sfg_bat_a$BAa <- sfg_bat_a$BA
sfg_bat_a$PAa <- sfg_bat_a$PA
sfg_bat_a$ABa <- sfg_bat_a$AB
sfg_bat_a$Ra <- sfg_bat_a$R
sfg_bat_a$Ha <- sfg_bat_a$H
sfg_bat_a$B2Ba <- sfg_bat_a$X2B
sfg_bat_a$B3Ba <- sfg_bat_a$X3B
sfg_bat_a$BHRa <- sfg_bat_a$HR
sfg_bat_a$RBIa <- sfg_bat_a$RBI
sfg_bat_a$BBBa <- sfg_bat_a$BB
sfg_bat_a$BSOa <- sfg_bat_a$SO
sfg_bat_a$SHa <- sfg_bat_a$SH
sfg_bat_a$SFa <- sfg_bat_a$SF
sfg_bat_a$SBa <- sfg_bat_a$SB
###create away variable###
sfg_bat_a$X <- ifelse(sfg_bat_a$X =='@', team, sfg_bat_a$X) #use of team variable name here
names(sfg_bat_a)[names(sfg_bat_a) == 'X'] <- 'away'
###change class of Date
sfg_bat_a$Date <- as.character(sfg_bat_a$Date)
###select key vars
sfg_bat_a <- sfg_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###sfg_bat_h for batting vars (home)###
sfg_bat_h <- sfg_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
sfg_bat_h <- sfg_bat_h %>% filter(X == '') #home games
###runs for home games###
sfg_bat_h$Rslt <- as.character(sfg_bat_h$Rslt) #character vector
TEMP<- strsplit(x=sfg_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sfg_bat_h$Rhome <- substring(TEMP$V1, 2); sfg_bat_h$Rhome <- as.numeric(sfg_bat_h$Rhome)
###Creating batting predictors for home games###
sfg_bat_h$BAh <- sfg_bat_h$BA
sfg_bat_h$PAh <- sfg_bat_h$PA
sfg_bat_h$ABh <- sfg_bat_h$AB
sfg_bat_h$Rh <- sfg_bat_h$R
sfg_bat_h$Hh <- sfg_bat_h$H
sfg_bat_h$B2Bh <- sfg_bat_h$X2B
sfg_bat_h$B3Bh <- sfg_bat_h$X3B
sfg_bat_h$BHRh <- sfg_bat_h$HR
sfg_bat_h$RBIh <- sfg_bat_h$RBI
sfg_bat_h$BBBh <- sfg_bat_h$BB
sfg_bat_h$BSOh <- sfg_bat_h$SO
sfg_bat_h$SHh <- sfg_bat_h$SH
sfg_bat_h$SFh <- sfg_bat_h$SF
sfg_bat_h$SBh <- sfg_bat_h$SB
###create home variable###
sfg_bat_h$X <- ifelse(sfg_bat_h$X =='', team, sfg_bat_h$X) #use of team variable name here
names(sfg_bat_h)[names(sfg_bat_h) == 'X'] <- 'home'
###change class of Date
sfg_bat_h$Date <- as.character(sfg_bat_h$Date)
###select key vars
sfg_bat_h <- sfg_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: sfg_pit###
sfg_pit <- read.csv("sfg_pit.txt")

###sfg_pit_a for pitching variables (away)###
sfg_pit_a <- sfg_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
sfg_pit_a <- sfg_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(sfg_pit_a)[names(sfg_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(sfg_pit_a)[names(sfg_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
sfg_pit_a$ERa <- sfg_pit_a$ER
sfg_pit_a$UERa <- sfg_pit_a$UER
sfg_pit_a$PBBa <- sfg_pit_a$BB
sfg_pit_a$PSOa <- sfg_pit_a$SO
sfg_pit_a$PHRa <- sfg_pit_a$HR
sfg_pit_a$BFa <- sfg_pit_a$BF
sfg_pit_a$Pita <- sfg_pit_a$Pit
sfg_pit_a$Stra <- sfg_pit_a$Str
sfg_pit_a$P2Ba <- sfg_pit_a$X2B
sfg_pit_a$P3Ba <- sfg_pit_a$X3B
###create away variable###
sfg_pit_a$X <- ifelse(sfg_pit_a$X =='@', team, sfg_pit_a$X) #use of team variable name here
names(sfg_pit_a)[names(sfg_pit_a) == 'X'] <- 'away'
###change class of Date
sfg_pit_a$Date <- as.character(sfg_pit_a$Date)
###select key vars
sfg_pit_a <- sfg_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###sfg_pit_h for pitching variables (home)###
sfg_pit_h <- sfg_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
sfg_pit_h <- sfg_pit_h %>% filter(X == '') #home games
###splitting sfg_pit_h vector###
names(sfg_pit_h)[names(sfg_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(sfg_pit_h)[names(sfg_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
sfg_pit_h$ERh <- sfg_pit_h$ER
sfg_pit_h$UERh <- sfg_pit_h$UER
sfg_pit_h$PBBh <- sfg_pit_h$BB
sfg_pit_h$PSOh <- sfg_pit_h$SO
sfg_pit_h$PHRh <- sfg_pit_h$HR
sfg_pit_h$BFh <- sfg_pit_h$BF
sfg_pit_h$Pith <- sfg_pit_h$Pit
sfg_pit_h$Strh <- sfg_pit_h$Str
sfg_pit_h$P2Bh <- sfg_pit_h$X2B
sfg_pit_h$P3Bh <- sfg_pit_h$X3B
###create home variable###
sfg_pit_h$X <- ifelse(sfg_pit_h$X =='', team, sfg_pit_h$X) #use of team variable name here
names(sfg_pit_h)[names(sfg_pit_h) == 'X'] <- 'home'
###change class of Date
sfg_pit_h$Date <- as.character(sfg_pit_h$Date)
###filter down data
sfg_pit_h <- sfg_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
