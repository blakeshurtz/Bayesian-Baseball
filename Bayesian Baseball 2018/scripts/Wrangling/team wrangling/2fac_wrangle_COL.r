library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
col_bat <- read.csv("col_bat.txt")

team <- 'COL' #team name variable

###col_homeaway for home/away###
col_homeaway <- col_bat %>% select(Date, Opp, X)
###create home variable
col_homeaway$X <- as.character(col_homeaway$X)
col_homeaway$Opp <- as.character(col_homeaway$Opp)
col_homeaway$X <- ifelse(col_homeaway$X =='@', col_homeaway$Opp, col_homeaway$X)
col_homeaway$X <- ifelse(col_homeaway$X =='', team, col_homeaway$X) #use of team variable name here
names(col_homeaway)[names(col_homeaway) == 'X'] <- 'home'
###create away variable
col_homeaway$Opp <- ifelse(col_homeaway$Opp == col_homeaway$home, team, col_homeaway$Opp) #use of team variable name here
names(col_homeaway)[names(col_homeaway) == 'Opp'] <- 'away'
###change class of Date
col_homeaway$Date <- as.character(col_homeaway$Date)
###select key vars
col_homeaway <- col_homeaway %>% select(home, away, Date) 

###col_bat_a for batting vars (away)###
col_bat_a <- col_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
col_bat_a <- col_bat_a %>% filter(X == '@') #away games
###runs for away games###
col_bat_a$Rslt <- as.character(col_bat_a$Rslt) #character vector
TEMP<- strsplit(x=col_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
col_bat_a$Raway <- substring(TEMP$V1, 2); col_bat_a$Raway <- as.numeric(col_bat_a$Raway)
###Creating batting predictors for away games###
col_bat_a$BAa <- col_bat_a$BA
col_bat_a$PAa <- col_bat_a$PA
col_bat_a$ABa <- col_bat_a$AB
col_bat_a$Ra <- col_bat_a$R
col_bat_a$Ha <- col_bat_a$H
col_bat_a$B2Ba <- col_bat_a$X2B
col_bat_a$B3Ba <- col_bat_a$X3B
col_bat_a$BHRa <- col_bat_a$HR
col_bat_a$RBIa <- col_bat_a$RBI
col_bat_a$BBBa <- col_bat_a$BB
col_bat_a$BSOa <- col_bat_a$SO
col_bat_a$SHa <- col_bat_a$SH
col_bat_a$SFa <- col_bat_a$SF
col_bat_a$SBa <- col_bat_a$SB
###create away variable###
col_bat_a$X <- ifelse(col_bat_a$X =='@', team, col_bat_a$X) #use of team variable name here
names(col_bat_a)[names(col_bat_a) == 'X'] <- 'away'
###change class of Date
col_bat_a$Date <- as.character(col_bat_a$Date)
###select key vars
col_bat_a <- col_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###col_bat_h for batting vars (home)###
col_bat_h <- col_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
col_bat_h <- col_bat_h %>% filter(X == '') #home games
###runs for home games###
col_bat_h$Rslt <- as.character(col_bat_h$Rslt) #character vector
TEMP<- strsplit(x=col_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
col_bat_h$Rhome <- substring(TEMP$V1, 2); col_bat_h$Rhome <- as.numeric(col_bat_h$Rhome)
###Creating batting predictors for home games###
col_bat_h$BAh <- col_bat_h$BA
col_bat_h$PAh <- col_bat_h$PA
col_bat_h$ABh <- col_bat_h$AB
col_bat_h$Rh <- col_bat_h$R
col_bat_h$Hh <- col_bat_h$H
col_bat_h$B2Bh <- col_bat_h$X2B
col_bat_h$B3Bh <- col_bat_h$X3B
col_bat_h$BHRh <- col_bat_h$HR
col_bat_h$RBIh <- col_bat_h$RBI
col_bat_h$BBBh <- col_bat_h$BB
col_bat_h$BSOh <- col_bat_h$SO
col_bat_h$SHh <- col_bat_h$SH
col_bat_h$SFh <- col_bat_h$SF
col_bat_h$SBh <- col_bat_h$SB
###create home variable###
col_bat_h$X <- ifelse(col_bat_h$X =='', team, col_bat_h$X) #use of team variable name here
names(col_bat_h)[names(col_bat_h) == 'X'] <- 'home'
###change class of Date
col_bat_h$Date <- as.character(col_bat_h$Date)
###select key vars
col_bat_h <- col_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: col_pit###
col_pit <- read.csv("col_pit.txt")

###col_pit_a for pitching variables (away)###
col_pit_a <- col_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
col_pit_a <- col_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(col_pit_a)[names(col_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(col_pit_a)[names(col_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
col_pit_a$ERa <- col_pit_a$ER
col_pit_a$UERa <- col_pit_a$UER
col_pit_a$PBBa <- col_pit_a$BB
col_pit_a$PSOa <- col_pit_a$SO
col_pit_a$PHRa <- col_pit_a$HR
col_pit_a$BFa <- col_pit_a$BF
col_pit_a$Pita <- col_pit_a$Pit
col_pit_a$Stra <- col_pit_a$Str
col_pit_a$P2Ba <- col_pit_a$X2B
col_pit_a$P3Ba <- col_pit_a$X3B
###create away variable###
col_pit_a$X <- ifelse(col_pit_a$X =='@', team, col_pit_a$X) #use of team variable name here
names(col_pit_a)[names(col_pit_a) == 'X'] <- 'away'
###change class of Date
col_pit_a$Date <- as.character(col_pit_a$Date)
###select key vars
col_pit_a <- col_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###col_pit_h for pitching variables (home)###
col_pit_h <- col_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
col_pit_h <- col_pit_h %>% filter(X == '') #home games
###splitting col_pit_h vector###
names(col_pit_h)[names(col_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(col_pit_h)[names(col_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
col_pit_h$ERh <- col_pit_h$ER
col_pit_h$UERh <- col_pit_h$UER
col_pit_h$PBBh <- col_pit_h$BB
col_pit_h$PSOh <- col_pit_h$SO
col_pit_h$PHRh <- col_pit_h$HR
col_pit_h$BFh <- col_pit_h$BF
col_pit_h$Pith <- col_pit_h$Pit
col_pit_h$Strh <- col_pit_h$Str
col_pit_h$P2Bh <- col_pit_h$X2B
col_pit_h$P3Bh <- col_pit_h$X3B
###create home variable###
col_pit_h$X <- ifelse(col_pit_h$X =='', team, col_pit_h$X) #use of team variable name here
names(col_pit_h)[names(col_pit_h) == 'X'] <- 'home'
###change class of Date
col_pit_h$Date <- as.character(col_pit_h$Date)
###filter down data
col_pit_h <- col_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
