library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
mia_bat <- read.csv("mia_bat.txt")

team <- 'MIA' #team name variable

###mia_homeaway for home/away###
mia_homeaway <- mia_bat %>% select(Date, Opp, X)
###create home variable
mia_homeaway$X <- as.character(mia_homeaway$X)
mia_homeaway$Opp <- as.character(mia_homeaway$Opp)
mia_homeaway$X <- ifelse(mia_homeaway$X =='@', mia_homeaway$Opp, mia_homeaway$X)
mia_homeaway$X <- ifelse(mia_homeaway$X =='', team, mia_homeaway$X) #use of team variable name here
names(mia_homeaway)[names(mia_homeaway) == 'X'] <- 'home'
###create away variable
mia_homeaway$Opp <- ifelse(mia_homeaway$Opp == mia_homeaway$home, team, mia_homeaway$Opp) #use of team variable name here
names(mia_homeaway)[names(mia_homeaway) == 'Opp'] <- 'away'
###change class of Date
mia_homeaway$Date <- as.character(mia_homeaway$Date)
###select key vars
mia_homeaway <- mia_homeaway %>% select(home, away, Date) 

###mia_bat_a for batting vars (away)###
mia_bat_a <- mia_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
mia_bat_a <- mia_bat_a %>% filter(X == '@') #away games
###runs for away games###
mia_bat_a$Rslt <- as.character(mia_bat_a$Rslt) #character vector
TEMP<- strsplit(x=mia_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mia_bat_a$Raway <- substring(TEMP$V1, 2); mia_bat_a$Raway <- as.numeric(mia_bat_a$Raway)
###Creating batting predictors for away games###
mia_bat_a$BAa <- mia_bat_a$BA
mia_bat_a$PAa <- mia_bat_a$PA
mia_bat_a$ABa <- mia_bat_a$AB
mia_bat_a$Ra <- mia_bat_a$R
mia_bat_a$Ha <- mia_bat_a$H
mia_bat_a$B2Ba <- mia_bat_a$X2B
mia_bat_a$B3Ba <- mia_bat_a$X3B
mia_bat_a$BHRa <- mia_bat_a$HR
mia_bat_a$RBIa <- mia_bat_a$RBI
mia_bat_a$BBBa <- mia_bat_a$BB
mia_bat_a$BSOa <- mia_bat_a$SO
mia_bat_a$SHa <- mia_bat_a$SH
mia_bat_a$SFa <- mia_bat_a$SF
mia_bat_a$SBa <- mia_bat_a$SB
###create away variable###
mia_bat_a$X <- ifelse(mia_bat_a$X =='@', team, mia_bat_a$X) #use of team variable name here
names(mia_bat_a)[names(mia_bat_a) == 'X'] <- 'away'
###change class of Date
mia_bat_a$Date <- as.character(mia_bat_a$Date)
###select key vars
mia_bat_a <- mia_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###mia_bat_h for batting vars (home)###
mia_bat_h <- mia_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
mia_bat_h <- mia_bat_h %>% filter(X == '') #home games
###runs for home games###
mia_bat_h$Rslt <- as.character(mia_bat_h$Rslt) #character vector
TEMP<- strsplit(x=mia_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mia_bat_h$Rhome <- substring(TEMP$V1, 2); mia_bat_h$Rhome <- as.numeric(mia_bat_h$Rhome)
###Creating batting predictors for home games###
mia_bat_h$BAh <- mia_bat_h$BA
mia_bat_h$PAh <- mia_bat_h$PA
mia_bat_h$ABh <- mia_bat_h$AB
mia_bat_h$Rh <- mia_bat_h$R
mia_bat_h$Hh <- mia_bat_h$H
mia_bat_h$B2Bh <- mia_bat_h$X2B
mia_bat_h$B3Bh <- mia_bat_h$X3B
mia_bat_h$BHRh <- mia_bat_h$HR
mia_bat_h$RBIh <- mia_bat_h$RBI
mia_bat_h$BBBh <- mia_bat_h$BB
mia_bat_h$BSOh <- mia_bat_h$SO
mia_bat_h$SHh <- mia_bat_h$SH
mia_bat_h$SFh <- mia_bat_h$SF
mia_bat_h$SBh <- mia_bat_h$SB
###create home variable###
mia_bat_h$X <- ifelse(mia_bat_h$X =='', team, mia_bat_h$X) #use of team variable name here
names(mia_bat_h)[names(mia_bat_h) == 'X'] <- 'home'
###change class of Date
mia_bat_h$Date <- as.character(mia_bat_h$Date)
###select key vars
mia_bat_h <- mia_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: mia_pit###
mia_pit <- read.csv("mia_pit.txt")

###mia_pit_a for pitching variables (away)###
mia_pit_a <- mia_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
mia_pit_a <- mia_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(mia_pit_a)[names(mia_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(mia_pit_a)[names(mia_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
mia_pit_a$ERa <- mia_pit_a$ER
mia_pit_a$UERa <- mia_pit_a$UER
mia_pit_a$PBBa <- mia_pit_a$BB
mia_pit_a$PSOa <- mia_pit_a$SO
mia_pit_a$PHRa <- mia_pit_a$HR
mia_pit_a$BFa <- mia_pit_a$BF
mia_pit_a$Pita <- mia_pit_a$Pit
mia_pit_a$Stra <- mia_pit_a$Str
mia_pit_a$P2Ba <- mia_pit_a$X2B
mia_pit_a$P3Ba <- mia_pit_a$X3B
###create away variable###
mia_pit_a$X <- ifelse(mia_pit_a$X =='@', team, mia_pit_a$X) #use of team variable name here
names(mia_pit_a)[names(mia_pit_a) == 'X'] <- 'away'
###change class of Date
mia_pit_a$Date <- as.character(mia_pit_a$Date)
###select key vars
mia_pit_a <- mia_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###mia_pit_h for pitching variables (home)###
mia_pit_h <- mia_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
mia_pit_h <- mia_pit_h %>% filter(X == '') #home games
###splitting mia_pit_h vector###
names(mia_pit_h)[names(mia_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(mia_pit_h)[names(mia_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
mia_pit_h$ERh <- mia_pit_h$ER
mia_pit_h$UERh <- mia_pit_h$UER
mia_pit_h$PBBh <- mia_pit_h$BB
mia_pit_h$PSOh <- mia_pit_h$SO
mia_pit_h$PHRh <- mia_pit_h$HR
mia_pit_h$BFh <- mia_pit_h$BF
mia_pit_h$Pith <- mia_pit_h$Pit
mia_pit_h$Strh <- mia_pit_h$Str
mia_pit_h$P2Bh <- mia_pit_h$X2B
mia_pit_h$P3Bh <- mia_pit_h$X3B
###create home variable###
mia_pit_h$X <- ifelse(mia_pit_h$X =='', team, mia_pit_h$X) #use of team variable name here
names(mia_pit_h)[names(mia_pit_h) == 'X'] <- 'home'
###change class of Date
mia_pit_h$Date <- as.character(mia_pit_h$Date)
###filter down data
mia_pit_h <- mia_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
