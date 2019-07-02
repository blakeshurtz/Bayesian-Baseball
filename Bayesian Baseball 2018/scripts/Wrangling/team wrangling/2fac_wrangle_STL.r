library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
stl_bat <- read.csv("stl_bat.txt")

team <- 'STL' #team name variable

###stl_homeaway for home/away###
stl_homeaway <- stl_bat %>% select(Date, Opp, X)
###create home variable
stl_homeaway$X <- as.character(stl_homeaway$X)
stl_homeaway$Opp <- as.character(stl_homeaway$Opp)
stl_homeaway$X <- ifelse(stl_homeaway$X =='@', stl_homeaway$Opp, stl_homeaway$X)
stl_homeaway$X <- ifelse(stl_homeaway$X =='', team, stl_homeaway$X) #use of team variable name here
names(stl_homeaway)[names(stl_homeaway) == 'X'] <- 'home'
###create away variable
stl_homeaway$Opp <- ifelse(stl_homeaway$Opp == stl_homeaway$home, team, stl_homeaway$Opp) #use of team variable name here
names(stl_homeaway)[names(stl_homeaway) == 'Opp'] <- 'away'
###change class of Date
stl_homeaway$Date <- as.character(stl_homeaway$Date)
###select key vars
stl_homeaway <- stl_homeaway %>% select(home, away, Date) 

###stl_bat_a for batting vars (away)###
stl_bat_a <- stl_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
stl_bat_a <- stl_bat_a %>% filter(X == '@') #away games
###runs for away games###
stl_bat_a$Rslt <- as.character(stl_bat_a$Rslt) #character vector
TEMP<- strsplit(x=stl_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
stl_bat_a$Raway <- substring(TEMP$V1, 2); stl_bat_a$Raway <- as.numeric(stl_bat_a$Raway)
###Creating batting predictors for away games###
stl_bat_a$BAa <- stl_bat_a$BA
stl_bat_a$PAa <- stl_bat_a$PA
stl_bat_a$ABa <- stl_bat_a$AB
stl_bat_a$Ra <- stl_bat_a$R
stl_bat_a$Ha <- stl_bat_a$H
stl_bat_a$B2Ba <- stl_bat_a$X2B
stl_bat_a$B3Ba <- stl_bat_a$X3B
stl_bat_a$BHRa <- stl_bat_a$HR
stl_bat_a$RBIa <- stl_bat_a$RBI
stl_bat_a$BBBa <- stl_bat_a$BB
stl_bat_a$BSOa <- stl_bat_a$SO
stl_bat_a$SHa <- stl_bat_a$SH
stl_bat_a$SFa <- stl_bat_a$SF
stl_bat_a$SBa <- stl_bat_a$SB
###create away variable###
stl_bat_a$X <- ifelse(stl_bat_a$X =='@', team, stl_bat_a$X) #use of team variable name here
names(stl_bat_a)[names(stl_bat_a) == 'X'] <- 'away'
###change class of Date
stl_bat_a$Date <- as.character(stl_bat_a$Date)
###select key vars
stl_bat_a <- stl_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###stl_bat_h for batting vars (home)###
stl_bat_h <- stl_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
stl_bat_h <- stl_bat_h %>% filter(X == '') #home games
###runs for home games###
stl_bat_h$Rslt <- as.character(stl_bat_h$Rslt) #character vector
TEMP<- strsplit(x=stl_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
stl_bat_h$Rhome <- substring(TEMP$V1, 2); stl_bat_h$Rhome <- as.numeric(stl_bat_h$Rhome)
###Creating batting predictors for home games###
stl_bat_h$BAh <- stl_bat_h$BA
stl_bat_h$PAh <- stl_bat_h$PA
stl_bat_h$ABh <- stl_bat_h$AB
stl_bat_h$Rh <- stl_bat_h$R
stl_bat_h$Hh <- stl_bat_h$H
stl_bat_h$B2Bh <- stl_bat_h$X2B
stl_bat_h$B3Bh <- stl_bat_h$X3B
stl_bat_h$BHRh <- stl_bat_h$HR
stl_bat_h$RBIh <- stl_bat_h$RBI
stl_bat_h$BBBh <- stl_bat_h$BB
stl_bat_h$BSOh <- stl_bat_h$SO
stl_bat_h$SHh <- stl_bat_h$SH
stl_bat_h$SFh <- stl_bat_h$SF
stl_bat_h$SBh <- stl_bat_h$SB
###create home variable###
stl_bat_h$X <- ifelse(stl_bat_h$X =='', team, stl_bat_h$X) #use of team variable name here
names(stl_bat_h)[names(stl_bat_h) == 'X'] <- 'home'
###change class of Date
stl_bat_h$Date <- as.character(stl_bat_h$Date)
###select key vars
stl_bat_h <- stl_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: stl_pit###
stl_pit <- read.csv("stl_pit.txt")

###stl_pit_a for pitching variables (away)###
stl_pit_a <- stl_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
stl_pit_a <- stl_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(stl_pit_a)[names(stl_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(stl_pit_a)[names(stl_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
stl_pit_a$ERa <- stl_pit_a$ER
stl_pit_a$UERa <- stl_pit_a$UER
stl_pit_a$PBBa <- stl_pit_a$BB
stl_pit_a$PSOa <- stl_pit_a$SO
stl_pit_a$PHRa <- stl_pit_a$HR
stl_pit_a$BFa <- stl_pit_a$BF
stl_pit_a$Pita <- stl_pit_a$Pit
stl_pit_a$Stra <- stl_pit_a$Str
stl_pit_a$P2Ba <- stl_pit_a$X2B
stl_pit_a$P3Ba <- stl_pit_a$X3B
###create away variable###
stl_pit_a$X <- ifelse(stl_pit_a$X =='@', team, stl_pit_a$X) #use of team variable name here
names(stl_pit_a)[names(stl_pit_a) == 'X'] <- 'away'
###change class of Date
stl_pit_a$Date <- as.character(stl_pit_a$Date)
###select key vars
stl_pit_a <- stl_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###stl_pit_h for pitching variables (home)###
stl_pit_h <- stl_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
stl_pit_h <- stl_pit_h %>% filter(X == '') #home games
###splitting stl_pit_h vector###
names(stl_pit_h)[names(stl_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(stl_pit_h)[names(stl_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
stl_pit_h$ERh <- stl_pit_h$ER
stl_pit_h$UERh <- stl_pit_h$UER
stl_pit_h$PBBh <- stl_pit_h$BB
stl_pit_h$PSOh <- stl_pit_h$SO
stl_pit_h$PHRh <- stl_pit_h$HR
stl_pit_h$BFh <- stl_pit_h$BF
stl_pit_h$Pith <- stl_pit_h$Pit
stl_pit_h$Strh <- stl_pit_h$Str
stl_pit_h$P2Bh <- stl_pit_h$X2B
stl_pit_h$P3Bh <- stl_pit_h$X3B
###create home variable###
stl_pit_h$X <- ifelse(stl_pit_h$X =='', team, stl_pit_h$X) #use of team variable name here
names(stl_pit_h)[names(stl_pit_h) == 'X'] <- 'home'
###change class of Date
stl_pit_h$Date <- as.character(stl_pit_h$Date)
###filter down data
stl_pit_h <- stl_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
