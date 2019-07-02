library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
tor_bat <- read.csv("tor_bat.txt")

team <- 'TOR' #team name variable

###tor_homeaway for home/away###
tor_homeaway <- tor_bat %>% select(Date, Opp, X)
###create home variable
tor_homeaway$X <- as.character(tor_homeaway$X)
tor_homeaway$Opp <- as.character(tor_homeaway$Opp)
tor_homeaway$X <- ifelse(tor_homeaway$X =='@', tor_homeaway$Opp, tor_homeaway$X)
tor_homeaway$X <- ifelse(tor_homeaway$X =='', team, tor_homeaway$X) #use of team variable name here
names(tor_homeaway)[names(tor_homeaway) == 'X'] <- 'home'
###create away variable
tor_homeaway$Opp <- ifelse(tor_homeaway$Opp == tor_homeaway$home, team, tor_homeaway$Opp) #use of team variable name here
names(tor_homeaway)[names(tor_homeaway) == 'Opp'] <- 'away'
###change class of Date
tor_homeaway$Date <- as.character(tor_homeaway$Date)
###select key vars
tor_homeaway <- tor_homeaway %>% select(home, away, Date) 

###tor_bat_a for batting vars (away)###
tor_bat_a <- tor_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
tor_bat_a <- tor_bat_a %>% filter(X == '@') #away games
###runs for away games###
tor_bat_a$Rslt <- as.character(tor_bat_a$Rslt) #character vector
TEMP<- strsplit(x=tor_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tor_bat_a$Raway <- substring(TEMP$V1, 2); tor_bat_a$Raway <- as.numeric(tor_bat_a$Raway)
###Creating batting predictors for away games###
tor_bat_a$BAa <- tor_bat_a$BA
tor_bat_a$PAa <- tor_bat_a$PA
tor_bat_a$ABa <- tor_bat_a$AB
tor_bat_a$Ra <- tor_bat_a$R
tor_bat_a$Ha <- tor_bat_a$H
tor_bat_a$B2Ba <- tor_bat_a$X2B
tor_bat_a$B3Ba <- tor_bat_a$X3B
tor_bat_a$BHRa <- tor_bat_a$HR
tor_bat_a$RBIa <- tor_bat_a$RBI
tor_bat_a$BBBa <- tor_bat_a$BB
tor_bat_a$BSOa <- tor_bat_a$SO
tor_bat_a$SHa <- tor_bat_a$SH
tor_bat_a$SFa <- tor_bat_a$SF
tor_bat_a$SBa <- tor_bat_a$SB
###create away variable###
tor_bat_a$X <- ifelse(tor_bat_a$X =='@', team, tor_bat_a$X) #use of team variable name here
names(tor_bat_a)[names(tor_bat_a) == 'X'] <- 'away'
###change class of Date
tor_bat_a$Date <- as.character(tor_bat_a$Date)
###select key vars
tor_bat_a <- tor_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###tor_bat_h for batting vars (home)###
tor_bat_h <- tor_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
tor_bat_h <- tor_bat_h %>% filter(X == '') #home games
###runs for home games###
tor_bat_h$Rslt <- as.character(tor_bat_h$Rslt) #character vector
TEMP<- strsplit(x=tor_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tor_bat_h$Rhome <- substring(TEMP$V1, 2); tor_bat_h$Rhome <- as.numeric(tor_bat_h$Rhome)
###Creating batting predictors for home games###
tor_bat_h$BAh <- tor_bat_h$BA
tor_bat_h$PAh <- tor_bat_h$PA
tor_bat_h$ABh <- tor_bat_h$AB
tor_bat_h$Rh <- tor_bat_h$R
tor_bat_h$Hh <- tor_bat_h$H
tor_bat_h$B2Bh <- tor_bat_h$X2B
tor_bat_h$B3Bh <- tor_bat_h$X3B
tor_bat_h$BHRh <- tor_bat_h$HR
tor_bat_h$RBIh <- tor_bat_h$RBI
tor_bat_h$BBBh <- tor_bat_h$BB
tor_bat_h$BSOh <- tor_bat_h$SO
tor_bat_h$SHh <- tor_bat_h$SH
tor_bat_h$SFh <- tor_bat_h$SF
tor_bat_h$SBh <- tor_bat_h$SB
###create home variable###
tor_bat_h$X <- ifelse(tor_bat_h$X =='', team, tor_bat_h$X) #use of team variable name here
names(tor_bat_h)[names(tor_bat_h) == 'X'] <- 'home'
###change class of Date
tor_bat_h$Date <- as.character(tor_bat_h$Date)
###select key vars
tor_bat_h <- tor_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: tor_pit###
tor_pit <- read.csv("tor_pit.txt")

###tor_pit_a for pitching variables (away)###
tor_pit_a <- tor_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
tor_pit_a <- tor_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(tor_pit_a)[names(tor_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(tor_pit_a)[names(tor_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
tor_pit_a$ERa <- tor_pit_a$ER
tor_pit_a$UERa <- tor_pit_a$UER
tor_pit_a$PBBa <- tor_pit_a$BB
tor_pit_a$PSOa <- tor_pit_a$SO
tor_pit_a$PHRa <- tor_pit_a$HR
tor_pit_a$BFa <- tor_pit_a$BF
tor_pit_a$Pita <- tor_pit_a$Pit
tor_pit_a$Stra <- tor_pit_a$Str
tor_pit_a$P2Ba <- tor_pit_a$X2B
tor_pit_a$P3Ba <- tor_pit_a$X3B
###create away variable###
tor_pit_a$X <- ifelse(tor_pit_a$X =='@', team, tor_pit_a$X) #use of team variable name here
names(tor_pit_a)[names(tor_pit_a) == 'X'] <- 'away'
###change class of Date
tor_pit_a$Date <- as.character(tor_pit_a$Date)
###select key vars
tor_pit_a <- tor_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###tor_pit_h for pitching variables (home)###
tor_pit_h <- tor_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
tor_pit_h <- tor_pit_h %>% filter(X == '') #home games
###splitting tor_pit_h vector###
names(tor_pit_h)[names(tor_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(tor_pit_h)[names(tor_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
tor_pit_h$ERh <- tor_pit_h$ER
tor_pit_h$UERh <- tor_pit_h$UER
tor_pit_h$PBBh <- tor_pit_h$BB
tor_pit_h$PSOh <- tor_pit_h$SO
tor_pit_h$PHRh <- tor_pit_h$HR
tor_pit_h$BFh <- tor_pit_h$BF
tor_pit_h$Pith <- tor_pit_h$Pit
tor_pit_h$Strh <- tor_pit_h$Str
tor_pit_h$P2Bh <- tor_pit_h$X2B
tor_pit_h$P3Bh <- tor_pit_h$X3B
###create home variable###
tor_pit_h$X <- ifelse(tor_pit_h$X =='', team, tor_pit_h$X) #use of team variable name here
names(tor_pit_h)[names(tor_pit_h) == 'X'] <- 'home'
###change class of Date
tor_pit_h$Date <- as.character(tor_pit_h$Date)
###filter down data
tor_pit_h <- tor_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
