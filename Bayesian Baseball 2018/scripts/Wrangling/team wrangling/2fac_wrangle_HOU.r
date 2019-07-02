library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
hou_bat <- read.csv("hou_bat.txt")

team <- 'HOU' #team name variable

###hou_homeaway for home/away###
hou_homeaway <- hou_bat %>% select(Date, Opp, X)
###create home variable
hou_homeaway$X <- as.character(hou_homeaway$X)
hou_homeaway$Opp <- as.character(hou_homeaway$Opp)
hou_homeaway$X <- ifelse(hou_homeaway$X =='@', hou_homeaway$Opp, hou_homeaway$X)
hou_homeaway$X <- ifelse(hou_homeaway$X =='', team, hou_homeaway$X) #use of team variable name here
names(hou_homeaway)[names(hou_homeaway) == 'X'] <- 'home'
###create away variable
hou_homeaway$Opp <- ifelse(hou_homeaway$Opp == hou_homeaway$home, team, hou_homeaway$Opp) #use of team variable name here
names(hou_homeaway)[names(hou_homeaway) == 'Opp'] <- 'away'
###change class of Date
hou_homeaway$Date <- as.character(hou_homeaway$Date)
###select key vars
hou_homeaway <- hou_homeaway %>% select(home, away, Date) 

###hou_bat_a for batting vars (away)###
hou_bat_a <- hou_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
hou_bat_a <- hou_bat_a %>% filter(X == '@') #away games
###runs for away games###
hou_bat_a$Rslt <- as.character(hou_bat_a$Rslt) #character vector
TEMP<- strsplit(x=hou_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
hou_bat_a$Raway <- substring(TEMP$V1, 2); hou_bat_a$Raway <- as.numeric(hou_bat_a$Raway)
###Creating batting predictors for away games###
hou_bat_a$BAa <- hou_bat_a$BA
hou_bat_a$PAa <- hou_bat_a$PA
hou_bat_a$ABa <- hou_bat_a$AB
hou_bat_a$Ra <- hou_bat_a$R
hou_bat_a$Ha <- hou_bat_a$H
hou_bat_a$B2Ba <- hou_bat_a$X2B
hou_bat_a$B3Ba <- hou_bat_a$X3B
hou_bat_a$BHRa <- hou_bat_a$HR
hou_bat_a$RBIa <- hou_bat_a$RBI
hou_bat_a$BBBa <- hou_bat_a$BB
hou_bat_a$BSOa <- hou_bat_a$SO
hou_bat_a$SHa <- hou_bat_a$SH
hou_bat_a$SFa <- hou_bat_a$SF
hou_bat_a$SBa <- hou_bat_a$SB
###create away variable###
hou_bat_a$X <- ifelse(hou_bat_a$X =='@', team, hou_bat_a$X) #use of team variable name here
names(hou_bat_a)[names(hou_bat_a) == 'X'] <- 'away'
###change class of Date
hou_bat_a$Date <- as.character(hou_bat_a$Date)
###select key vars
hou_bat_a <- hou_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###hou_bat_h for batting vars (home)###
hou_bat_h <- hou_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
hou_bat_h <- hou_bat_h %>% filter(X == '') #home games
###runs for home games###
hou_bat_h$Rslt <- as.character(hou_bat_h$Rslt) #character vector
TEMP<- strsplit(x=hou_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
hou_bat_h$Rhome <- substring(TEMP$V1, 2); hou_bat_h$Rhome <- as.numeric(hou_bat_h$Rhome)
###Creating batting predictors for home games###
hou_bat_h$BAh <- hou_bat_h$BA
hou_bat_h$PAh <- hou_bat_h$PA
hou_bat_h$ABh <- hou_bat_h$AB
hou_bat_h$Rh <- hou_bat_h$R
hou_bat_h$Hh <- hou_bat_h$H
hou_bat_h$B2Bh <- hou_bat_h$X2B
hou_bat_h$B3Bh <- hou_bat_h$X3B
hou_bat_h$BHRh <- hou_bat_h$HR
hou_bat_h$RBIh <- hou_bat_h$RBI
hou_bat_h$BBBh <- hou_bat_h$BB
hou_bat_h$BSOh <- hou_bat_h$SO
hou_bat_h$SHh <- hou_bat_h$SH
hou_bat_h$SFh <- hou_bat_h$SF
hou_bat_h$SBh <- hou_bat_h$SB
###create home variable###
hou_bat_h$X <- ifelse(hou_bat_h$X =='', team, hou_bat_h$X) #use of team variable name here
names(hou_bat_h)[names(hou_bat_h) == 'X'] <- 'home'
###change class of Date
hou_bat_h$Date <- as.character(hou_bat_h$Date)
###select key vars
hou_bat_h <- hou_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: hou_pit###
hou_pit <- read.csv("hou_pit.txt")

###hou_pit_a for pitching variables (away)###
hou_pit_a <- hou_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
hou_pit_a <- hou_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(hou_pit_a)[names(hou_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(hou_pit_a)[names(hou_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
hou_pit_a$ERa <- hou_pit_a$ER
hou_pit_a$UERa <- hou_pit_a$UER
hou_pit_a$PBBa <- hou_pit_a$BB
hou_pit_a$PSOa <- hou_pit_a$SO
hou_pit_a$PHRa <- hou_pit_a$HR
hou_pit_a$BFa <- hou_pit_a$BF
hou_pit_a$Pita <- hou_pit_a$Pit
hou_pit_a$Stra <- hou_pit_a$Str
hou_pit_a$P2Ba <- hou_pit_a$X2B
hou_pit_a$P3Ba <- hou_pit_a$X3B
###create away variable###
hou_pit_a$X <- ifelse(hou_pit_a$X =='@', team, hou_pit_a$X) #use of team variable name here
names(hou_pit_a)[names(hou_pit_a) == 'X'] <- 'away'
###change class of Date
hou_pit_a$Date <- as.character(hou_pit_a$Date)
###select key vars
hou_pit_a <- hou_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###hou_pit_h for pitching variables (home)###
hou_pit_h <- hou_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
hou_pit_h <- hou_pit_h %>% filter(X == '') #home games
###splitting hou_pit_h vector###
names(hou_pit_h)[names(hou_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(hou_pit_h)[names(hou_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
hou_pit_h$ERh <- hou_pit_h$ER
hou_pit_h$UERh <- hou_pit_h$UER
hou_pit_h$PBBh <- hou_pit_h$BB
hou_pit_h$PSOh <- hou_pit_h$SO
hou_pit_h$PHRh <- hou_pit_h$HR
hou_pit_h$BFh <- hou_pit_h$BF
hou_pit_h$Pith <- hou_pit_h$Pit
hou_pit_h$Strh <- hou_pit_h$Str
hou_pit_h$P2Bh <- hou_pit_h$X2B
hou_pit_h$P3Bh <- hou_pit_h$X3B
###create home variable###
hou_pit_h$X <- ifelse(hou_pit_h$X =='', team, hou_pit_h$X) #use of team variable name here
names(hou_pit_h)[names(hou_pit_h) == 'X'] <- 'home'
###change class of Date
hou_pit_h$Date <- as.character(hou_pit_h$Date)
###filter down data
hou_pit_h <- hou_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
