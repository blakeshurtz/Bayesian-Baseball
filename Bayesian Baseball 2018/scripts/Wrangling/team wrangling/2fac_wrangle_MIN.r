library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
min_bat <- read.csv("min_bat.txt")

team <- 'MIN' #team name variable

###min_homeaway for home/away###
min_homeaway <- min_bat %>% select(Date, Opp, X)
###create home variable
min_homeaway$X <- as.character(min_homeaway$X)
min_homeaway$Opp <- as.character(min_homeaway$Opp)
min_homeaway$X <- ifelse(min_homeaway$X =='@', min_homeaway$Opp, min_homeaway$X)
min_homeaway$X <- ifelse(min_homeaway$X =='', team, min_homeaway$X) #use of team variable name here
names(min_homeaway)[names(min_homeaway) == 'X'] <- 'home'
###create away variable
min_homeaway$Opp <- ifelse(min_homeaway$Opp == min_homeaway$home, team, min_homeaway$Opp) #use of team variable name here
names(min_homeaway)[names(min_homeaway) == 'Opp'] <- 'away'
###change class of Date
min_homeaway$Date <- as.character(min_homeaway$Date)
###select key vars
min_homeaway <- min_homeaway %>% select(home, away, Date) 

###min_bat_a for batting vars (away)###
min_bat_a <- min_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
min_bat_a <- min_bat_a %>% filter(X == '@') #away games
###runs for away games###
min_bat_a$Rslt <- as.character(min_bat_a$Rslt) #character vector
TEMP<- strsplit(x=min_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
min_bat_a$Raway <- substring(TEMP$V1, 2); min_bat_a$Raway <- as.numeric(min_bat_a$Raway)
###Creating batting predictors for away games###
min_bat_a$BAa <- min_bat_a$BA
min_bat_a$PAa <- min_bat_a$PA
min_bat_a$ABa <- min_bat_a$AB
min_bat_a$Ra <- min_bat_a$R
min_bat_a$Ha <- min_bat_a$H
min_bat_a$B2Ba <- min_bat_a$X2B
min_bat_a$B3Ba <- min_bat_a$X3B
min_bat_a$BHRa <- min_bat_a$HR
min_bat_a$RBIa <- min_bat_a$RBI
min_bat_a$BBBa <- min_bat_a$BB
min_bat_a$BSOa <- min_bat_a$SO
min_bat_a$SHa <- min_bat_a$SH
min_bat_a$SFa <- min_bat_a$SF
min_bat_a$SBa <- min_bat_a$SB
###create away variable###
min_bat_a$X <- ifelse(min_bat_a$X =='@', team, min_bat_a$X) #use of team variable name here
names(min_bat_a)[names(min_bat_a) == 'X'] <- 'away'
###change class of Date
min_bat_a$Date <- as.character(min_bat_a$Date)
###select key vars
min_bat_a <- min_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###min_bat_h for batting vars (home)###
min_bat_h <- min_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
min_bat_h <- min_bat_h %>% filter(X == '') #home games
###runs for home games###
min_bat_h$Rslt <- as.character(min_bat_h$Rslt) #character vector
TEMP<- strsplit(x=min_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
min_bat_h$Rhome <- substring(TEMP$V1, 2); min_bat_h$Rhome <- as.numeric(min_bat_h$Rhome)
###Creating batting predictors for home games###
min_bat_h$BAh <- min_bat_h$BA
min_bat_h$PAh <- min_bat_h$PA
min_bat_h$ABh <- min_bat_h$AB
min_bat_h$Rh <- min_bat_h$R
min_bat_h$Hh <- min_bat_h$H
min_bat_h$B2Bh <- min_bat_h$X2B
min_bat_h$B3Bh <- min_bat_h$X3B
min_bat_h$BHRh <- min_bat_h$HR
min_bat_h$RBIh <- min_bat_h$RBI
min_bat_h$BBBh <- min_bat_h$BB
min_bat_h$BSOh <- min_bat_h$SO
min_bat_h$SHh <- min_bat_h$SH
min_bat_h$SFh <- min_bat_h$SF
min_bat_h$SBh <- min_bat_h$SB
###create home variable###
min_bat_h$X <- ifelse(min_bat_h$X =='', team, min_bat_h$X) #use of team variable name here
names(min_bat_h)[names(min_bat_h) == 'X'] <- 'home'
###change class of Date
min_bat_h$Date <- as.character(min_bat_h$Date)
###select key vars
min_bat_h <- min_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: min_pit###
min_pit <- read.csv("min_pit.txt")

###min_pit_a for pitching variables (away)###
min_pit_a <- min_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
min_pit_a <- min_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(min_pit_a)[names(min_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(min_pit_a)[names(min_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
min_pit_a$ERa <- min_pit_a$ER
min_pit_a$UERa <- min_pit_a$UER
min_pit_a$PBBa <- min_pit_a$BB
min_pit_a$PSOa <- min_pit_a$SO
min_pit_a$PHRa <- min_pit_a$HR
min_pit_a$BFa <- min_pit_a$BF
min_pit_a$Pita <- min_pit_a$Pit
min_pit_a$Stra <- min_pit_a$Str
min_pit_a$P2Ba <- min_pit_a$X2B
min_pit_a$P3Ba <- min_pit_a$X3B
###create away variable###
min_pit_a$X <- ifelse(min_pit_a$X =='@', team, min_pit_a$X) #use of team variable name here
names(min_pit_a)[names(min_pit_a) == 'X'] <- 'away'
###change class of Date
min_pit_a$Date <- as.character(min_pit_a$Date)
###select key vars
min_pit_a <- min_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###min_pit_h for pitching variables (home)###
min_pit_h <- min_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
min_pit_h <- min_pit_h %>% filter(X == '') #home games
###splitting min_pit_h vector###
names(min_pit_h)[names(min_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(min_pit_h)[names(min_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
min_pit_h$ERh <- min_pit_h$ER
min_pit_h$UERh <- min_pit_h$UER
min_pit_h$PBBh <- min_pit_h$BB
min_pit_h$PSOh <- min_pit_h$SO
min_pit_h$PHRh <- min_pit_h$HR
min_pit_h$BFh <- min_pit_h$BF
min_pit_h$Pith <- min_pit_h$Pit
min_pit_h$Strh <- min_pit_h$Str
min_pit_h$P2Bh <- min_pit_h$X2B
min_pit_h$P3Bh <- min_pit_h$X3B
###create home variable###
min_pit_h$X <- ifelse(min_pit_h$X =='', team, min_pit_h$X) #use of team variable name here
names(min_pit_h)[names(min_pit_h) == 'X'] <- 'home'
###change class of Date
min_pit_h$Date <- as.character(min_pit_h$Date)
###filter down data
min_pit_h <- min_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
