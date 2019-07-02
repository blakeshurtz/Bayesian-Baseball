library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
atl_bat <- read.csv("atl_bat.txt")

team <- 'ATL' #team name variable

###atl_homeaway for home/away###
atl_homeaway <- atl_bat %>% select(Date, Opp, X)
###create home variable
atl_homeaway$X <- as.character(atl_homeaway$X)
atl_homeaway$Opp <- as.character(atl_homeaway$Opp)
atl_homeaway$X <- ifelse(atl_homeaway$X =='@', atl_homeaway$Opp, atl_homeaway$X)
atl_homeaway$X <- ifelse(atl_homeaway$X =='', team, atl_homeaway$X) #use of team variable name here
names(atl_homeaway)[names(atl_homeaway) == 'X'] <- 'home'
###create away variable
atl_homeaway$Opp <- ifelse(atl_homeaway$Opp == atl_homeaway$home, team, atl_homeaway$Opp) #use of team variable name here
names(atl_homeaway)[names(atl_homeaway) == 'Opp'] <- 'away'
###change class of Date
atl_homeaway$Date <- as.character(atl_homeaway$Date)
###select key vars
atl_homeaway <- atl_homeaway %>% select(home, away, Date) 

###atl_bat_a for batting vars (away)###
atl_bat_a <- atl_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
atl_bat_a <- atl_bat_a %>% filter(X == '@') #away games
###runs for away games###
atl_bat_a$Rslt <- as.character(atl_bat_a$Rslt) #character vector
TEMP<- strsplit(x=atl_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
atl_bat_a$Raway <- substring(TEMP$V1, 2); atl_bat_a$Raway <- as.numeric(atl_bat_a$Raway)
###Creating batting predictors for away games###
atl_bat_a$BAa <- atl_bat_a$BA
atl_bat_a$PAa <- atl_bat_a$PA
atl_bat_a$ABa <- atl_bat_a$AB
atl_bat_a$Ra <- atl_bat_a$R
atl_bat_a$Ha <- atl_bat_a$H
atl_bat_a$B2Ba <- atl_bat_a$X2B
atl_bat_a$B3Ba <- atl_bat_a$X3B
atl_bat_a$BHRa <- atl_bat_a$HR
atl_bat_a$RBIa <- atl_bat_a$RBI
atl_bat_a$BBBa <- atl_bat_a$BB
atl_bat_a$BSOa <- atl_bat_a$SO
atl_bat_a$SHa <- atl_bat_a$SH
atl_bat_a$SFa <- atl_bat_a$SF
atl_bat_a$SBa <- atl_bat_a$SB
###create away variable###
atl_bat_a$X <- ifelse(atl_bat_a$X =='@', team, atl_bat_a$X) #use of team variable name here
names(atl_bat_a)[names(atl_bat_a) == 'X'] <- 'away'
###change class of Date
atl_bat_a$Date <- as.character(atl_bat_a$Date)
###select key vars
atl_bat_a <- atl_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###atl_bat_h for batting vars (home)###
atl_bat_h <- atl_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
atl_bat_h <- atl_bat_h %>% filter(X == '') #home games
###runs for home games###
atl_bat_h$Rslt <- as.character(atl_bat_h$Rslt) #character vector
TEMP<- strsplit(x=atl_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
atl_bat_h$Rhome <- substring(TEMP$V1, 2); atl_bat_h$Rhome <- as.numeric(atl_bat_h$Rhome)
###Creating batting predictors for home games###
atl_bat_h$BAh <- atl_bat_h$BA
atl_bat_h$PAh <- atl_bat_h$PA
atl_bat_h$ABh <- atl_bat_h$AB
atl_bat_h$Rh <- atl_bat_h$R
atl_bat_h$Hh <- atl_bat_h$H
atl_bat_h$B2Bh <- atl_bat_h$X2B
atl_bat_h$B3Bh <- atl_bat_h$X3B
atl_bat_h$BHRh <- atl_bat_h$HR
atl_bat_h$RBIh <- atl_bat_h$RBI
atl_bat_h$BBBh <- atl_bat_h$BB
atl_bat_h$BSOh <- atl_bat_h$SO
atl_bat_h$SHh <- atl_bat_h$SH
atl_bat_h$SFh <- atl_bat_h$SF
atl_bat_h$SBh <- atl_bat_h$SB
###create home variable###
atl_bat_h$X <- ifelse(atl_bat_h$X =='', team, atl_bat_h$X) #use of team variable name here
names(atl_bat_h)[names(atl_bat_h) == 'X'] <- 'home'
###change class of Date
atl_bat_h$Date <- as.character(atl_bat_h$Date)
###select key vars
atl_bat_h <- atl_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: atl_pit###
atl_pit <- read.csv("atl_pit.txt")

###atl_pit_a for pitching variables (away)###
atl_pit_a <- atl_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
atl_pit_a <- atl_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(atl_pit_a)[names(atl_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(atl_pit_a)[names(atl_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
atl_pit_a$ERa <- atl_pit_a$ER
atl_pit_a$UERa <- atl_pit_a$UER
atl_pit_a$PBBa <- atl_pit_a$BB
atl_pit_a$PSOa <- atl_pit_a$SO
atl_pit_a$PHRa <- atl_pit_a$HR
atl_pit_a$BFa <- atl_pit_a$BF
atl_pit_a$Pita <- atl_pit_a$Pit
atl_pit_a$Stra <- atl_pit_a$Str
atl_pit_a$P2Ba <- atl_pit_a$X2B
atl_pit_a$P3Ba <- atl_pit_a$X3B
###create away variable###
atl_pit_a$X <- ifelse(atl_pit_a$X =='@', team, atl_pit_a$X) #use of team variable name here
names(atl_pit_a)[names(atl_pit_a) == 'X'] <- 'away'
###change class of Date
atl_pit_a$Date <- as.character(atl_pit_a$Date)
###select key vars
atl_pit_a <- atl_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###atl_pit_h for pitching variables (home)###
atl_pit_h <- atl_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
atl_pit_h <- atl_pit_h %>% filter(X == '') #home games
###splitting atl_pit_h vector###
names(atl_pit_h)[names(atl_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(atl_pit_h)[names(atl_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
atl_pit_h$ERh <- atl_pit_h$ER
atl_pit_h$UERh <- atl_pit_h$UER
atl_pit_h$PBBh <- atl_pit_h$BB
atl_pit_h$PSOh <- atl_pit_h$SO
atl_pit_h$PHRh <- atl_pit_h$HR
atl_pit_h$BFh <- atl_pit_h$BF
atl_pit_h$Pith <- atl_pit_h$Pit
atl_pit_h$Strh <- atl_pit_h$Str
atl_pit_h$P2Bh <- atl_pit_h$X2B
atl_pit_h$P3Bh <- atl_pit_h$X3B
###create home variable###
atl_pit_h$X <- ifelse(atl_pit_h$X =='', team, atl_pit_h$X) #use of team variable name here
names(atl_pit_h)[names(atl_pit_h) == 'X'] <- 'home'
###change class of Date
atl_pit_h$Date <- as.character(atl_pit_h$Date)
###filter down data
atl_pit_h <- atl_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
