library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
ari_bat <- read.csv("ari_bat.txt")

team <- 'ARI' #team name variable

###ari_homeaway for home/away###
ari_homeaway <- ari_bat %>% select(Date, Opp, X)
###create home variable
ari_homeaway$X <- as.character(ari_homeaway$X)
ari_homeaway$Opp <- as.character(ari_homeaway$Opp)
ari_homeaway$X <- ifelse(ari_homeaway$X =='@', ari_homeaway$Opp, ari_homeaway$X)
ari_homeaway$X <- ifelse(ari_homeaway$X =='', team, ari_homeaway$X) #use of team variable name here
names(ari_homeaway)[names(ari_homeaway) == 'X'] <- 'home'
###create away variable
ari_homeaway$Opp <- ifelse(ari_homeaway$Opp == ari_homeaway$home, team, ari_homeaway$Opp) #use of team variable name here
names(ari_homeaway)[names(ari_homeaway) == 'Opp'] <- 'away'
###change class of Date
ari_homeaway$Date <- as.character(ari_homeaway$Date)
###select key vars
ari_homeaway <- ari_homeaway %>% select(home, away, Date) 

###ari_bat_a for batting vars (away)###
ari_bat_a <- ari_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
ari_bat_a <- ari_bat_a %>% filter(X == '@') #away games
###runs for away games###
ari_bat_a$Rslt <- as.character(ari_bat_a$Rslt) #character vector
TEMP<- strsplit(x=ari_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
ari_bat_a$Raway <- substring(TEMP$V1, 2); ari_bat_a$Raway <- as.numeric(ari_bat_a$Raway)
###Creating batting predictors for away games###
ari_bat_a$BAa <- ari_bat_a$BA
ari_bat_a$PAa <- ari_bat_a$PA
ari_bat_a$ABa <- ari_bat_a$AB
ari_bat_a$Ra <- ari_bat_a$R
ari_bat_a$Ha <- ari_bat_a$H
ari_bat_a$B2Ba <- ari_bat_a$X2B
ari_bat_a$B3Ba <- ari_bat_a$X3B
ari_bat_a$BHRa <- ari_bat_a$HR
ari_bat_a$RBIa <- ari_bat_a$RBI
ari_bat_a$BBBa <- ari_bat_a$BB
ari_bat_a$BSOa <- ari_bat_a$SO
ari_bat_a$SHa <- ari_bat_a$SH
ari_bat_a$SFa <- ari_bat_a$SF
ari_bat_a$SBa <- ari_bat_a$SB
###create away variable###
ari_bat_a$X <- ifelse(ari_bat_a$X =='@', team, ari_bat_a$X) #use of team variable name here
names(ari_bat_a)[names(ari_bat_a) == 'X'] <- 'away'
###change class of Date
ari_bat_a$Date <- as.character(ari_bat_a$Date)
###select key vars
ari_bat_a <- ari_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###ari_bat_h for batting vars (home)###
ari_bat_h <- ari_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
ari_bat_h <- ari_bat_h %>% filter(X == '') #home games
###runs for home games###
ari_bat_h$Rslt <- as.character(ari_bat_h$Rslt) #character vector
TEMP<- strsplit(x=ari_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
ari_bat_h$Rhome <- substring(TEMP$V1, 2); ari_bat_h$Rhome <- as.numeric(ari_bat_h$Rhome)
###Creating batting predictors for home games###
ari_bat_h$BAh <- ari_bat_h$BA
ari_bat_h$PAh <- ari_bat_h$PA
ari_bat_h$ABh <- ari_bat_h$AB
ari_bat_h$Rh <- ari_bat_h$R
ari_bat_h$Hh <- ari_bat_h$H
ari_bat_h$B2Bh <- ari_bat_h$X2B
ari_bat_h$B3Bh <- ari_bat_h$X3B
ari_bat_h$BHRh <- ari_bat_h$HR
ari_bat_h$RBIh <- ari_bat_h$RBI
ari_bat_h$BBBh <- ari_bat_h$BB
ari_bat_h$BSOh <- ari_bat_h$SO
ari_bat_h$SHh <- ari_bat_h$SH
ari_bat_h$SFh <- ari_bat_h$SF
ari_bat_h$SBh <- ari_bat_h$SB
###create home variable###
ari_bat_h$X <- ifelse(ari_bat_h$X =='', team, ari_bat_h$X) #use of team variable name here
names(ari_bat_h)[names(ari_bat_h) == 'X'] <- 'home'
###change class of Date
ari_bat_h$Date <- as.character(ari_bat_h$Date)
###select key vars
ari_bat_h <- ari_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: ari_pit###
ari_pit <- read.csv("ari_pit.txt")

###ari_pit_a for pitching variables (away)###
ari_pit_a <- ari_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
ari_pit_a <- ari_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(ari_pit_a)[names(ari_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(ari_pit_a)[names(ari_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
ari_pit_a$ERa <- ari_pit_a$ER
ari_pit_a$UERa <- ari_pit_a$UER
ari_pit_a$PBBa <- ari_pit_a$BB
ari_pit_a$PSOa <- ari_pit_a$SO
ari_pit_a$PHRa <- ari_pit_a$HR
ari_pit_a$BFa <- ari_pit_a$BF
ari_pit_a$Pita <- ari_pit_a$Pit
ari_pit_a$Stra <- ari_pit_a$Str
ari_pit_a$P2Ba <- ari_pit_a$X2B
ari_pit_a$P3Ba <- ari_pit_a$X3B
###create away variable###
ari_pit_a$X <- ifelse(ari_pit_a$X =='@', team, ari_pit_a$X) #use of team variable name here
names(ari_pit_a)[names(ari_pit_a) == 'X'] <- 'away'
###change class of Date
ari_pit_a$Date <- as.character(ari_pit_a$Date)
###select key vars
ari_pit_a <- ari_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###ari_pit_h for pitching variables (home)###
ari_pit_h <- ari_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
ari_pit_h <- ari_pit_h %>% filter(X == '') #home games
###splitting ari_pit_h vector###
names(ari_pit_h)[names(ari_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(ari_pit_h)[names(ari_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
ari_pit_h$ERh <- ari_pit_h$ER
ari_pit_h$UERh <- ari_pit_h$UER
ari_pit_h$PBBh <- ari_pit_h$BB
ari_pit_h$PSOh <- ari_pit_h$SO
ari_pit_h$PHRh <- ari_pit_h$HR
ari_pit_h$BFh <- ari_pit_h$BF
ari_pit_h$Pith <- ari_pit_h$Pit
ari_pit_h$Strh <- ari_pit_h$Str
ari_pit_h$P2Bh <- ari_pit_h$X2B
ari_pit_h$P3Bh <- ari_pit_h$X3B
###create home variable###
ari_pit_h$X <- ifelse(ari_pit_h$X =='', team, ari_pit_h$X) #use of team variable name here
names(ari_pit_h)[names(ari_pit_h) == 'X'] <- 'home'
###change class of Date
ari_pit_h$Date <- as.character(ari_pit_h$Date)
###filter down data
ari_pit_h <- ari_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
