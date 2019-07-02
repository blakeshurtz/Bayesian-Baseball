library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
nym_bat <- read.csv("nym_bat.txt")

team <- 'NYM' #team name variable

###nym_homeaway for home/away###
nym_homeaway <- nym_bat %>% select(Date, Opp, X)
###create home variable
nym_homeaway$X <- as.character(nym_homeaway$X)
nym_homeaway$Opp <- as.character(nym_homeaway$Opp)
nym_homeaway$X <- ifelse(nym_homeaway$X =='@', nym_homeaway$Opp, nym_homeaway$X)
nym_homeaway$X <- ifelse(nym_homeaway$X =='', team, nym_homeaway$X) #use of team variable name here
names(nym_homeaway)[names(nym_homeaway) == 'X'] <- 'home'
###create away variable
nym_homeaway$Opp <- ifelse(nym_homeaway$Opp == nym_homeaway$home, team, nym_homeaway$Opp) #use of team variable name here
names(nym_homeaway)[names(nym_homeaway) == 'Opp'] <- 'away'
###change class of Date
nym_homeaway$Date <- as.character(nym_homeaway$Date)
###select key vars
nym_homeaway <- nym_homeaway %>% select(home, away, Date) 

###nym_bat_a for batting vars (away)###
nym_bat_a <- nym_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
nym_bat_a <- nym_bat_a %>% filter(X == '@') #away games
###runs for away games###
nym_bat_a$Rslt <- as.character(nym_bat_a$Rslt) #character vector
TEMP<- strsplit(x=nym_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nym_bat_a$Raway <- substring(TEMP$V1, 2); nym_bat_a$Raway <- as.numeric(nym_bat_a$Raway)
###Creating batting predictors for away games###
nym_bat_a$BAa <- nym_bat_a$BA
nym_bat_a$PAa <- nym_bat_a$PA
nym_bat_a$ABa <- nym_bat_a$AB
nym_bat_a$Ra <- nym_bat_a$R
nym_bat_a$Ha <- nym_bat_a$H
nym_bat_a$B2Ba <- nym_bat_a$X2B
nym_bat_a$B3Ba <- nym_bat_a$X3B
nym_bat_a$BHRa <- nym_bat_a$HR
nym_bat_a$RBIa <- nym_bat_a$RBI
nym_bat_a$BBBa <- nym_bat_a$BB
nym_bat_a$BSOa <- nym_bat_a$SO
nym_bat_a$SHa <- nym_bat_a$SH
nym_bat_a$SFa <- nym_bat_a$SF
nym_bat_a$SBa <- nym_bat_a$SB
###create away variable###
nym_bat_a$X <- ifelse(nym_bat_a$X =='@', team, nym_bat_a$X) #use of team variable name here
names(nym_bat_a)[names(nym_bat_a) == 'X'] <- 'away'
###change class of Date
nym_bat_a$Date <- as.character(nym_bat_a$Date)
###select key vars
nym_bat_a <- nym_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###nym_bat_h for batting vars (home)###
nym_bat_h <- nym_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
nym_bat_h <- nym_bat_h %>% filter(X == '') #home games
###runs for home games###
nym_bat_h$Rslt <- as.character(nym_bat_h$Rslt) #character vector
TEMP<- strsplit(x=nym_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nym_bat_h$Rhome <- substring(TEMP$V1, 2); nym_bat_h$Rhome <- as.numeric(nym_bat_h$Rhome)
###Creating batting predictors for home games###
nym_bat_h$BAh <- nym_bat_h$BA
nym_bat_h$PAh <- nym_bat_h$PA
nym_bat_h$ABh <- nym_bat_h$AB
nym_bat_h$Rh <- nym_bat_h$R
nym_bat_h$Hh <- nym_bat_h$H
nym_bat_h$B2Bh <- nym_bat_h$X2B
nym_bat_h$B3Bh <- nym_bat_h$X3B
nym_bat_h$BHRh <- nym_bat_h$HR
nym_bat_h$RBIh <- nym_bat_h$RBI
nym_bat_h$BBBh <- nym_bat_h$BB
nym_bat_h$BSOh <- nym_bat_h$SO
nym_bat_h$SHh <- nym_bat_h$SH
nym_bat_h$SFh <- nym_bat_h$SF
nym_bat_h$SBh <- nym_bat_h$SB
###create home variable###
nym_bat_h$X <- ifelse(nym_bat_h$X =='', team, nym_bat_h$X) #use of team variable name here
names(nym_bat_h)[names(nym_bat_h) == 'X'] <- 'home'
###change class of Date
nym_bat_h$Date <- as.character(nym_bat_h$Date)
###select key vars
nym_bat_h <- nym_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: nym_pit###
nym_pit <- read.csv("nym_pit.txt")

###nym_pit_a for pitching variables (away)###
nym_pit_a <- nym_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
nym_pit_a <- nym_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(nym_pit_a)[names(nym_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(nym_pit_a)[names(nym_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
nym_pit_a$ERa <- nym_pit_a$ER
nym_pit_a$UERa <- nym_pit_a$UER
nym_pit_a$PBBa <- nym_pit_a$BB
nym_pit_a$PSOa <- nym_pit_a$SO
nym_pit_a$PHRa <- nym_pit_a$HR
nym_pit_a$BFa <- nym_pit_a$BF
nym_pit_a$Pita <- nym_pit_a$Pit
nym_pit_a$Stra <- nym_pit_a$Str
nym_pit_a$P2Ba <- nym_pit_a$X2B
nym_pit_a$P3Ba <- nym_pit_a$X3B
###create away variable###
nym_pit_a$X <- ifelse(nym_pit_a$X =='@', team, nym_pit_a$X) #use of team variable name here
names(nym_pit_a)[names(nym_pit_a) == 'X'] <- 'away'
###change class of Date
nym_pit_a$Date <- as.character(nym_pit_a$Date)
###select key vars
nym_pit_a <- nym_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###nym_pit_h for pitching variables (home)###
nym_pit_h <- nym_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
nym_pit_h <- nym_pit_h %>% filter(X == '') #home games
###splitting nym_pit_h vector###
names(nym_pit_h)[names(nym_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(nym_pit_h)[names(nym_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
nym_pit_h$ERh <- nym_pit_h$ER
nym_pit_h$UERh <- nym_pit_h$UER
nym_pit_h$PBBh <- nym_pit_h$BB
nym_pit_h$PSOh <- nym_pit_h$SO
nym_pit_h$PHRh <- nym_pit_h$HR
nym_pit_h$BFh <- nym_pit_h$BF
nym_pit_h$Pith <- nym_pit_h$Pit
nym_pit_h$Strh <- nym_pit_h$Str
nym_pit_h$P2Bh <- nym_pit_h$X2B
nym_pit_h$P3Bh <- nym_pit_h$X3B
###create home variable###
nym_pit_h$X <- ifelse(nym_pit_h$X =='', team, nym_pit_h$X) #use of team variable name here
names(nym_pit_h)[names(nym_pit_h) == 'X'] <- 'home'
###change class of Date
nym_pit_h$Date <- as.character(nym_pit_h$Date)
###filter down data
nym_pit_h <- nym_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
