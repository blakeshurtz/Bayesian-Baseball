library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
sea_bat <- read.csv("sea_bat.txt")

team <- 'SEA' #team name variable

###sea_homeaway for home/away###
sea_homeaway <- sea_bat %>% select(Date, Opp, X)
###create home variable
sea_homeaway$X <- as.character(sea_homeaway$X)
sea_homeaway$Opp <- as.character(sea_homeaway$Opp)
sea_homeaway$X <- ifelse(sea_homeaway$X =='@', sea_homeaway$Opp, sea_homeaway$X)
sea_homeaway$X <- ifelse(sea_homeaway$X =='', team, sea_homeaway$X) #use of team variable name here
names(sea_homeaway)[names(sea_homeaway) == 'X'] <- 'home'
###create away variable
sea_homeaway$Opp <- ifelse(sea_homeaway$Opp == sea_homeaway$home, team, sea_homeaway$Opp) #use of team variable name here
names(sea_homeaway)[names(sea_homeaway) == 'Opp'] <- 'away'
###change class of Date
sea_homeaway$Date <- as.character(sea_homeaway$Date)
###select key vars
sea_homeaway <- sea_homeaway %>% select(home, away, Date) 

###sea_bat_a for batting vars (away)###
sea_bat_a <- sea_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
sea_bat_a <- sea_bat_a %>% filter(X == '@') #away games
###runs for away games###
sea_bat_a$Rslt <- as.character(sea_bat_a$Rslt) #character vector
TEMP<- strsplit(x=sea_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sea_bat_a$Raway <- substring(TEMP$V1, 2); sea_bat_a$Raway <- as.numeric(sea_bat_a$Raway)
###Creating batting predictors for away games###
sea_bat_a$BAa <- sea_bat_a$BA
sea_bat_a$PAa <- sea_bat_a$PA
sea_bat_a$ABa <- sea_bat_a$AB
sea_bat_a$Ra <- sea_bat_a$R
sea_bat_a$Ha <- sea_bat_a$H
sea_bat_a$B2Ba <- sea_bat_a$X2B
sea_bat_a$B3Ba <- sea_bat_a$X3B
sea_bat_a$BHRa <- sea_bat_a$HR
sea_bat_a$RBIa <- sea_bat_a$RBI
sea_bat_a$BBBa <- sea_bat_a$BB
sea_bat_a$BSOa <- sea_bat_a$SO
sea_bat_a$SHa <- sea_bat_a$SH
sea_bat_a$SFa <- sea_bat_a$SF
sea_bat_a$SBa <- sea_bat_a$SB
###create away variable###
sea_bat_a$X <- ifelse(sea_bat_a$X =='@', team, sea_bat_a$X) #use of team variable name here
names(sea_bat_a)[names(sea_bat_a) == 'X'] <- 'away'
###change class of Date
sea_bat_a$Date <- as.character(sea_bat_a$Date)
###select key vars
sea_bat_a <- sea_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###sea_bat_h for batting vars (home)###
sea_bat_h <- sea_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
sea_bat_h <- sea_bat_h %>% filter(X == '') #home games
###runs for home games###
sea_bat_h$Rslt <- as.character(sea_bat_h$Rslt) #character vector
TEMP<- strsplit(x=sea_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sea_bat_h$Rhome <- substring(TEMP$V1, 2); sea_bat_h$Rhome <- as.numeric(sea_bat_h$Rhome)
###Creating batting predictors for home games###
sea_bat_h$BAh <- sea_bat_h$BA
sea_bat_h$PAh <- sea_bat_h$PA
sea_bat_h$ABh <- sea_bat_h$AB
sea_bat_h$Rh <- sea_bat_h$R
sea_bat_h$Hh <- sea_bat_h$H
sea_bat_h$B2Bh <- sea_bat_h$X2B
sea_bat_h$B3Bh <- sea_bat_h$X3B
sea_bat_h$BHRh <- sea_bat_h$HR
sea_bat_h$RBIh <- sea_bat_h$RBI
sea_bat_h$BBBh <- sea_bat_h$BB
sea_bat_h$BSOh <- sea_bat_h$SO
sea_bat_h$SHh <- sea_bat_h$SH
sea_bat_h$SFh <- sea_bat_h$SF
sea_bat_h$SBh <- sea_bat_h$SB
###create home variable###
sea_bat_h$X <- ifelse(sea_bat_h$X =='', team, sea_bat_h$X) #use of team variable name here
names(sea_bat_h)[names(sea_bat_h) == 'X'] <- 'home'
###change class of Date
sea_bat_h$Date <- as.character(sea_bat_h$Date)
###select key vars
sea_bat_h <- sea_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: sea_pit###
sea_pit <- read.csv("sea_pit.txt")

###sea_pit_a for pitching variables (away)###
sea_pit_a <- sea_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
sea_pit_a <- sea_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(sea_pit_a)[names(sea_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(sea_pit_a)[names(sea_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
sea_pit_a$ERa <- sea_pit_a$ER
sea_pit_a$UERa <- sea_pit_a$UER
sea_pit_a$PBBa <- sea_pit_a$BB
sea_pit_a$PSOa <- sea_pit_a$SO
sea_pit_a$PHRa <- sea_pit_a$HR
sea_pit_a$BFa <- sea_pit_a$BF
sea_pit_a$Pita <- sea_pit_a$Pit
sea_pit_a$Stra <- sea_pit_a$Str
sea_pit_a$P2Ba <- sea_pit_a$X2B
sea_pit_a$P3Ba <- sea_pit_a$X3B
###create away variable###
sea_pit_a$X <- ifelse(sea_pit_a$X =='@', team, sea_pit_a$X) #use of team variable name here
names(sea_pit_a)[names(sea_pit_a) == 'X'] <- 'away'
###change class of Date
sea_pit_a$Date <- as.character(sea_pit_a$Date)
###select key vars
sea_pit_a <- sea_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###sea_pit_h for pitching variables (home)###
sea_pit_h <- sea_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
sea_pit_h <- sea_pit_h %>% filter(X == '') #home games
###splitting sea_pit_h vector###
names(sea_pit_h)[names(sea_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(sea_pit_h)[names(sea_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
sea_pit_h$ERh <- sea_pit_h$ER
sea_pit_h$UERh <- sea_pit_h$UER
sea_pit_h$PBBh <- sea_pit_h$BB
sea_pit_h$PSOh <- sea_pit_h$SO
sea_pit_h$PHRh <- sea_pit_h$HR
sea_pit_h$BFh <- sea_pit_h$BF
sea_pit_h$Pith <- sea_pit_h$Pit
sea_pit_h$Strh <- sea_pit_h$Str
sea_pit_h$P2Bh <- sea_pit_h$X2B
sea_pit_h$P3Bh <- sea_pit_h$X3B
###create home variable###
sea_pit_h$X <- ifelse(sea_pit_h$X =='', team, sea_pit_h$X) #use of team variable name here
names(sea_pit_h)[names(sea_pit_h) == 'X'] <- 'home'
###change class of Date
sea_pit_h$Date <- as.character(sea_pit_h$Date)
###filter down data
sea_pit_h <- sea_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
