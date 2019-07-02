library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
cle_bat <- read.csv("cle_bat.txt")

team <- 'CLE' #team name variable

###cle_homeaway for home/away###
cle_homeaway <- cle_bat %>% select(Date, Opp, X)
###create home variable
cle_homeaway$X <- as.character(cle_homeaway$X)
cle_homeaway$Opp <- as.character(cle_homeaway$Opp)
cle_homeaway$X <- ifelse(cle_homeaway$X =='@', cle_homeaway$Opp, cle_homeaway$X)
cle_homeaway$X <- ifelse(cle_homeaway$X =='', team, cle_homeaway$X) #use of team variable name here
names(cle_homeaway)[names(cle_homeaway) == 'X'] <- 'home'
###create away variable
cle_homeaway$Opp <- ifelse(cle_homeaway$Opp == cle_homeaway$home, team, cle_homeaway$Opp) #use of team variable name here
names(cle_homeaway)[names(cle_homeaway) == 'Opp'] <- 'away'
###change class of Date
cle_homeaway$Date <- as.character(cle_homeaway$Date)
###select key vars
cle_homeaway <- cle_homeaway %>% select(home, away, Date) 

###cle_bat_a for batting vars (away)###
cle_bat_a <- cle_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
cle_bat_a <- cle_bat_a %>% filter(X == '@') #away games
###runs for away games###
cle_bat_a$Rslt <- as.character(cle_bat_a$Rslt) #character vector
TEMP<- strsplit(x=cle_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cle_bat_a$Raway <- substring(TEMP$V1, 2); cle_bat_a$Raway <- as.numeric(cle_bat_a$Raway)
###Creating batting predictors for away games###
cle_bat_a$BAa <- cle_bat_a$BA
cle_bat_a$PAa <- cle_bat_a$PA
cle_bat_a$ABa <- cle_bat_a$AB
cle_bat_a$Ra <- cle_bat_a$R
cle_bat_a$Ha <- cle_bat_a$H
cle_bat_a$B2Ba <- cle_bat_a$X2B
cle_bat_a$B3Ba <- cle_bat_a$X3B
cle_bat_a$BHRa <- cle_bat_a$HR
cle_bat_a$RBIa <- cle_bat_a$RBI
cle_bat_a$BBBa <- cle_bat_a$BB
cle_bat_a$BSOa <- cle_bat_a$SO
cle_bat_a$SHa <- cle_bat_a$SH
cle_bat_a$SFa <- cle_bat_a$SF
cle_bat_a$SBa <- cle_bat_a$SB
###create away variable###
cle_bat_a$X <- ifelse(cle_bat_a$X =='@', team, cle_bat_a$X) #use of team variable name here
names(cle_bat_a)[names(cle_bat_a) == 'X'] <- 'away'
###change class of Date
cle_bat_a$Date <- as.character(cle_bat_a$Date)
###select key vars
cle_bat_a <- cle_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###cle_bat_h for batting vars (home)###
cle_bat_h <- cle_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
cle_bat_h <- cle_bat_h %>% filter(X == '') #home games
###runs for home games###
cle_bat_h$Rslt <- as.character(cle_bat_h$Rslt) #character vector
TEMP<- strsplit(x=cle_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cle_bat_h$Rhome <- substring(TEMP$V1, 2); cle_bat_h$Rhome <- as.numeric(cle_bat_h$Rhome)
###Creating batting predictors for home games###
cle_bat_h$BAh <- cle_bat_h$BA
cle_bat_h$PAh <- cle_bat_h$PA
cle_bat_h$ABh <- cle_bat_h$AB
cle_bat_h$Rh <- cle_bat_h$R
cle_bat_h$Hh <- cle_bat_h$H
cle_bat_h$B2Bh <- cle_bat_h$X2B
cle_bat_h$B3Bh <- cle_bat_h$X3B
cle_bat_h$BHRh <- cle_bat_h$HR
cle_bat_h$RBIh <- cle_bat_h$RBI
cle_bat_h$BBBh <- cle_bat_h$BB
cle_bat_h$BSOh <- cle_bat_h$SO
cle_bat_h$SHh <- cle_bat_h$SH
cle_bat_h$SFh <- cle_bat_h$SF
cle_bat_h$SBh <- cle_bat_h$SB
###create home variable###
cle_bat_h$X <- ifelse(cle_bat_h$X =='', team, cle_bat_h$X) #use of team variable name here
names(cle_bat_h)[names(cle_bat_h) == 'X'] <- 'home'
###change class of Date
cle_bat_h$Date <- as.character(cle_bat_h$Date)
###select key vars
cle_bat_h <- cle_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: cle_pit###
cle_pit <- read.csv("cle_pit.txt")

###cle_pit_a for pitching variables (away)###
cle_pit_a <- cle_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
cle_pit_a <- cle_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(cle_pit_a)[names(cle_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(cle_pit_a)[names(cle_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
cle_pit_a$ERa <- cle_pit_a$ER
cle_pit_a$UERa <- cle_pit_a$UER
cle_pit_a$PBBa <- cle_pit_a$BB
cle_pit_a$PSOa <- cle_pit_a$SO
cle_pit_a$PHRa <- cle_pit_a$HR
cle_pit_a$BFa <- cle_pit_a$BF
cle_pit_a$Pita <- cle_pit_a$Pit
cle_pit_a$Stra <- cle_pit_a$Str
cle_pit_a$P2Ba <- cle_pit_a$X2B
cle_pit_a$P3Ba <- cle_pit_a$X3B
###create away variable###
cle_pit_a$X <- ifelse(cle_pit_a$X =='@', team, cle_pit_a$X) #use of team variable name here
names(cle_pit_a)[names(cle_pit_a) == 'X'] <- 'away'
###change class of Date
cle_pit_a$Date <- as.character(cle_pit_a$Date)
###select key vars
cle_pit_a <- cle_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###cle_pit_h for pitching variables (home)###
cle_pit_h <- cle_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
cle_pit_h <- cle_pit_h %>% filter(X == '') #home games
###splitting cle_pit_h vector###
names(cle_pit_h)[names(cle_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(cle_pit_h)[names(cle_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
cle_pit_h$ERh <- cle_pit_h$ER
cle_pit_h$UERh <- cle_pit_h$UER
cle_pit_h$PBBh <- cle_pit_h$BB
cle_pit_h$PSOh <- cle_pit_h$SO
cle_pit_h$PHRh <- cle_pit_h$HR
cle_pit_h$BFh <- cle_pit_h$BF
cle_pit_h$Pith <- cle_pit_h$Pit
cle_pit_h$Strh <- cle_pit_h$Str
cle_pit_h$P2Bh <- cle_pit_h$X2B
cle_pit_h$P3Bh <- cle_pit_h$X3B
###create home variable###
cle_pit_h$X <- ifelse(cle_pit_h$X =='', team, cle_pit_h$X) #use of team variable name here
names(cle_pit_h)[names(cle_pit_h) == 'X'] <- 'home'
###change class of Date
cle_pit_h$Date <- as.character(cle_pit_h$Date)
###filter down data
cle_pit_h <- cle_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
