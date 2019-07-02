library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
pit_bat <- read.csv("pit_bat.txt")

team <- 'PIT' #team name variable

###pit_homeaway for home/away###
pit_homeaway <- pit_bat %>% select(Date, Opp, X)
###create home variable
pit_homeaway$X <- as.character(pit_homeaway$X)
pit_homeaway$Opp <- as.character(pit_homeaway$Opp)
pit_homeaway$X <- ifelse(pit_homeaway$X =='@', pit_homeaway$Opp, pit_homeaway$X)
pit_homeaway$X <- ifelse(pit_homeaway$X =='', team, pit_homeaway$X) #use of team variable name here
names(pit_homeaway)[names(pit_homeaway) == 'X'] <- 'home'
###create away variable
pit_homeaway$Opp <- ifelse(pit_homeaway$Opp == pit_homeaway$home, team, pit_homeaway$Opp) #use of team variable name here
names(pit_homeaway)[names(pit_homeaway) == 'Opp'] <- 'away'
###change class of Date
pit_homeaway$Date <- as.character(pit_homeaway$Date)
###select key vars
pit_homeaway <- pit_homeaway %>% select(home, away, Date) 

###pit_bat_a for batting vars (away)###
pit_bat_a <- pit_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
pit_bat_a <- pit_bat_a %>% filter(X == '@') #away games
###runs for away games###
pit_bat_a$Rslt <- as.character(pit_bat_a$Rslt) #character vector
TEMP<- strsplit(x=pit_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
pit_bat_a$Raway <- substring(TEMP$V1, 2); pit_bat_a$Raway <- as.numeric(pit_bat_a$Raway)
###Creating batting predictors for away games###
pit_bat_a$BAa <- pit_bat_a$BA
pit_bat_a$PAa <- pit_bat_a$PA
pit_bat_a$ABa <- pit_bat_a$AB
pit_bat_a$Ra <- pit_bat_a$R
pit_bat_a$Ha <- pit_bat_a$H
pit_bat_a$B2Ba <- pit_bat_a$X2B
pit_bat_a$B3Ba <- pit_bat_a$X3B
pit_bat_a$BHRa <- pit_bat_a$HR
pit_bat_a$RBIa <- pit_bat_a$RBI
pit_bat_a$BBBa <- pit_bat_a$BB
pit_bat_a$BSOa <- pit_bat_a$SO
pit_bat_a$SHa <- pit_bat_a$SH
pit_bat_a$SFa <- pit_bat_a$SF
pit_bat_a$SBa <- pit_bat_a$SB
###create away variable###
pit_bat_a$X <- ifelse(pit_bat_a$X =='@', team, pit_bat_a$X) #use of team variable name here
names(pit_bat_a)[names(pit_bat_a) == 'X'] <- 'away'
###change class of Date
pit_bat_a$Date <- as.character(pit_bat_a$Date)
###select key vars
pit_bat_a <- pit_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###pit_bat_h for batting vars (home)###
pit_bat_h <- pit_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
pit_bat_h <- pit_bat_h %>% filter(X == '') #home games
###runs for home games###
pit_bat_h$Rslt <- as.character(pit_bat_h$Rslt) #character vector
TEMP<- strsplit(x=pit_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
pit_bat_h$Rhome <- substring(TEMP$V1, 2); pit_bat_h$Rhome <- as.numeric(pit_bat_h$Rhome)
###Creating batting predictors for home games###
pit_bat_h$BAh <- pit_bat_h$BA
pit_bat_h$PAh <- pit_bat_h$PA
pit_bat_h$ABh <- pit_bat_h$AB
pit_bat_h$Rh <- pit_bat_h$R
pit_bat_h$Hh <- pit_bat_h$H
pit_bat_h$B2Bh <- pit_bat_h$X2B
pit_bat_h$B3Bh <- pit_bat_h$X3B
pit_bat_h$BHRh <- pit_bat_h$HR
pit_bat_h$RBIh <- pit_bat_h$RBI
pit_bat_h$BBBh <- pit_bat_h$BB
pit_bat_h$BSOh <- pit_bat_h$SO
pit_bat_h$SHh <- pit_bat_h$SH
pit_bat_h$SFh <- pit_bat_h$SF
pit_bat_h$SBh <- pit_bat_h$SB
###create home variable###
pit_bat_h$X <- ifelse(pit_bat_h$X =='', team, pit_bat_h$X) #use of team variable name here
names(pit_bat_h)[names(pit_bat_h) == 'X'] <- 'home'
###change class of Date
pit_bat_h$Date <- as.character(pit_bat_h$Date)
###select key vars
pit_bat_h <- pit_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: pit_pit###
pit_pit <- read.csv("pit_pit.txt")

###pit_pit_a for pitching variables (away)###
pit_pit_a <- pit_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
pit_pit_a <- pit_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(pit_pit_a)[names(pit_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(pit_pit_a)[names(pit_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
pit_pit_a$ERa <- pit_pit_a$ER
pit_pit_a$UERa <- pit_pit_a$UER
pit_pit_a$PBBa <- pit_pit_a$BB
pit_pit_a$PSOa <- pit_pit_a$SO
pit_pit_a$PHRa <- pit_pit_a$HR
pit_pit_a$BFa <- pit_pit_a$BF
pit_pit_a$Pita <- pit_pit_a$Pit
pit_pit_a$Stra <- pit_pit_a$Str
pit_pit_a$P2Ba <- pit_pit_a$X2B
pit_pit_a$P3Ba <- pit_pit_a$X3B
###create away variable###
pit_pit_a$X <- ifelse(pit_pit_a$X =='@', team, pit_pit_a$X) #use of team variable name here
names(pit_pit_a)[names(pit_pit_a) == 'X'] <- 'away'
###change class of Date
pit_pit_a$Date <- as.character(pit_pit_a$Date)
###select key vars
pit_pit_a <- pit_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###pit_pit_h for pitching variables (home)###
pit_pit_h <- pit_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
pit_pit_h <- pit_pit_h %>% filter(X == '') #home games
###splitting pit_pit_h vector###
names(pit_pit_h)[names(pit_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(pit_pit_h)[names(pit_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
pit_pit_h$ERh <- pit_pit_h$ER
pit_pit_h$UERh <- pit_pit_h$UER
pit_pit_h$PBBh <- pit_pit_h$BB
pit_pit_h$PSOh <- pit_pit_h$SO
pit_pit_h$PHRh <- pit_pit_h$HR
pit_pit_h$BFh <- pit_pit_h$BF
pit_pit_h$Pith <- pit_pit_h$Pit
pit_pit_h$Strh <- pit_pit_h$Str
pit_pit_h$P2Bh <- pit_pit_h$X2B
pit_pit_h$P3Bh <- pit_pit_h$X3B
###create home variable###
pit_pit_h$X <- ifelse(pit_pit_h$X =='', team, pit_pit_h$X) #use of team variable name here
names(pit_pit_h)[names(pit_pit_h) == 'X'] <- 'home'
###change class of Date
pit_pit_h$Date <- as.character(pit_pit_h$Date)
###filter down data
pit_pit_h <- pit_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
