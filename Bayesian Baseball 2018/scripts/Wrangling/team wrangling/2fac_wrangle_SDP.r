library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
sdp_bat <- read.csv("sdp_bat.txt")

team <- 'SDP' #team name variable

###sdp_homeaway for home/away###
sdp_homeaway <- sdp_bat %>% select(Date, Opp, X)
###create home variable
sdp_homeaway$X <- as.character(sdp_homeaway$X)
sdp_homeaway$Opp <- as.character(sdp_homeaway$Opp)
sdp_homeaway$X <- ifelse(sdp_homeaway$X =='@', sdp_homeaway$Opp, sdp_homeaway$X)
sdp_homeaway$X <- ifelse(sdp_homeaway$X =='', team, sdp_homeaway$X) #use of team variable name here
names(sdp_homeaway)[names(sdp_homeaway) == 'X'] <- 'home'
###create away variable
sdp_homeaway$Opp <- ifelse(sdp_homeaway$Opp == sdp_homeaway$home, team, sdp_homeaway$Opp) #use of team variable name here
names(sdp_homeaway)[names(sdp_homeaway) == 'Opp'] <- 'away'
###change class of Date
sdp_homeaway$Date <- as.character(sdp_homeaway$Date)
###select key vars
sdp_homeaway <- sdp_homeaway %>% select(home, away, Date) 

###sdp_bat_a for batting vars (away)###
sdp_bat_a <- sdp_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
sdp_bat_a <- sdp_bat_a %>% filter(X == '@') #away games
###runs for away games###
sdp_bat_a$Rslt <- as.character(sdp_bat_a$Rslt) #character vector
TEMP<- strsplit(x=sdp_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sdp_bat_a$Raway <- substring(TEMP$V1, 2); sdp_bat_a$Raway <- as.numeric(sdp_bat_a$Raway)
###Creating batting predictors for away games###
sdp_bat_a$BAa <- sdp_bat_a$BA
sdp_bat_a$PAa <- sdp_bat_a$PA
sdp_bat_a$ABa <- sdp_bat_a$AB
sdp_bat_a$Ra <- sdp_bat_a$R
sdp_bat_a$Ha <- sdp_bat_a$H
sdp_bat_a$B2Ba <- sdp_bat_a$X2B
sdp_bat_a$B3Ba <- sdp_bat_a$X3B
sdp_bat_a$BHRa <- sdp_bat_a$HR
sdp_bat_a$RBIa <- sdp_bat_a$RBI
sdp_bat_a$BBBa <- sdp_bat_a$BB
sdp_bat_a$BSOa <- sdp_bat_a$SO
sdp_bat_a$SHa <- sdp_bat_a$SH
sdp_bat_a$SFa <- sdp_bat_a$SF
sdp_bat_a$SBa <- sdp_bat_a$SB
###create away variable###
sdp_bat_a$X <- ifelse(sdp_bat_a$X =='@', team, sdp_bat_a$X) #use of team variable name here
names(sdp_bat_a)[names(sdp_bat_a) == 'X'] <- 'away'
###change class of Date
sdp_bat_a$Date <- as.character(sdp_bat_a$Date)
###select key vars
sdp_bat_a <- sdp_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###sdp_bat_h for batting vars (home)###
sdp_bat_h <- sdp_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
sdp_bat_h <- sdp_bat_h %>% filter(X == '') #home games
###runs for home games###
sdp_bat_h$Rslt <- as.character(sdp_bat_h$Rslt) #character vector
TEMP<- strsplit(x=sdp_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sdp_bat_h$Rhome <- substring(TEMP$V1, 2); sdp_bat_h$Rhome <- as.numeric(sdp_bat_h$Rhome)
###Creating batting predictors for home games###
sdp_bat_h$BAh <- sdp_bat_h$BA
sdp_bat_h$PAh <- sdp_bat_h$PA
sdp_bat_h$ABh <- sdp_bat_h$AB
sdp_bat_h$Rh <- sdp_bat_h$R
sdp_bat_h$Hh <- sdp_bat_h$H
sdp_bat_h$B2Bh <- sdp_bat_h$X2B
sdp_bat_h$B3Bh <- sdp_bat_h$X3B
sdp_bat_h$BHRh <- sdp_bat_h$HR
sdp_bat_h$RBIh <- sdp_bat_h$RBI
sdp_bat_h$BBBh <- sdp_bat_h$BB
sdp_bat_h$BSOh <- sdp_bat_h$SO
sdp_bat_h$SHh <- sdp_bat_h$SH
sdp_bat_h$SFh <- sdp_bat_h$SF
sdp_bat_h$SBh <- sdp_bat_h$SB
###create home variable###
sdp_bat_h$X <- ifelse(sdp_bat_h$X =='', team, sdp_bat_h$X) #use of team variable name here
names(sdp_bat_h)[names(sdp_bat_h) == 'X'] <- 'home'
###change class of Date
sdp_bat_h$Date <- as.character(sdp_bat_h$Date)
###select key vars
sdp_bat_h <- sdp_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: sdp_pit###
sdp_pit <- read.csv("sdp_pit.txt")

###sdp_pit_a for pitching variables (away)###
sdp_pit_a <- sdp_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
sdp_pit_a <- sdp_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(sdp_pit_a)[names(sdp_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(sdp_pit_a)[names(sdp_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
sdp_pit_a$ERa <- sdp_pit_a$ER
sdp_pit_a$UERa <- sdp_pit_a$UER
sdp_pit_a$PBBa <- sdp_pit_a$BB
sdp_pit_a$PSOa <- sdp_pit_a$SO
sdp_pit_a$PHRa <- sdp_pit_a$HR
sdp_pit_a$BFa <- sdp_pit_a$BF
sdp_pit_a$Pita <- sdp_pit_a$Pit
sdp_pit_a$Stra <- sdp_pit_a$Str
sdp_pit_a$P2Ba <- sdp_pit_a$X2B
sdp_pit_a$P3Ba <- sdp_pit_a$X3B
###create away variable###
sdp_pit_a$X <- ifelse(sdp_pit_a$X =='@', team, sdp_pit_a$X) #use of team variable name here
names(sdp_pit_a)[names(sdp_pit_a) == 'X'] <- 'away'
###change class of Date
sdp_pit_a$Date <- as.character(sdp_pit_a$Date)
###select key vars
sdp_pit_a <- sdp_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###sdp_pit_h for pitching variables (home)###
sdp_pit_h <- sdp_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
sdp_pit_h <- sdp_pit_h %>% filter(X == '') #home games
###splitting sdp_pit_h vector###
names(sdp_pit_h)[names(sdp_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(sdp_pit_h)[names(sdp_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
sdp_pit_h$ERh <- sdp_pit_h$ER
sdp_pit_h$UERh <- sdp_pit_h$UER
sdp_pit_h$PBBh <- sdp_pit_h$BB
sdp_pit_h$PSOh <- sdp_pit_h$SO
sdp_pit_h$PHRh <- sdp_pit_h$HR
sdp_pit_h$BFh <- sdp_pit_h$BF
sdp_pit_h$Pith <- sdp_pit_h$Pit
sdp_pit_h$Strh <- sdp_pit_h$Str
sdp_pit_h$P2Bh <- sdp_pit_h$X2B
sdp_pit_h$P3Bh <- sdp_pit_h$X3B
###create home variable###
sdp_pit_h$X <- ifelse(sdp_pit_h$X =='', team, sdp_pit_h$X) #use of team variable name here
names(sdp_pit_h)[names(sdp_pit_h) == 'X'] <- 'home'
###change class of Date
sdp_pit_h$Date <- as.character(sdp_pit_h$Date)
###filter down data
sdp_pit_h <- sdp_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
