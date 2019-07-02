library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
laa_bat <- read.csv("laa_bat.txt")

team <- 'LAA' #team name variable

###laa_homeaway for home/away###
laa_homeaway <- laa_bat %>% select(Date, Opp, X)
###create home variable
laa_homeaway$X <- as.character(laa_homeaway$X)
laa_homeaway$Opp <- as.character(laa_homeaway$Opp)
laa_homeaway$X <- ifelse(laa_homeaway$X =='@', laa_homeaway$Opp, laa_homeaway$X)
laa_homeaway$X <- ifelse(laa_homeaway$X =='', team, laa_homeaway$X) #use of team variable name here
names(laa_homeaway)[names(laa_homeaway) == 'X'] <- 'home'
###create away variable
laa_homeaway$Opp <- ifelse(laa_homeaway$Opp == laa_homeaway$home, team, laa_homeaway$Opp) #use of team variable name here
names(laa_homeaway)[names(laa_homeaway) == 'Opp'] <- 'away'
###change class of Date
laa_homeaway$Date <- as.character(laa_homeaway$Date)
###select key vars
laa_homeaway <- laa_homeaway %>% select(home, away, Date) 

###laa_bat_a for batting vars (away)###
laa_bat_a <- laa_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
laa_bat_a <- laa_bat_a %>% filter(X == '@') #away games
###runs for away games###
laa_bat_a$Rslt <- as.character(laa_bat_a$Rslt) #character vector
TEMP<- strsplit(x=laa_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
laa_bat_a$Raway <- substring(TEMP$V1, 2); laa_bat_a$Raway <- as.numeric(laa_bat_a$Raway)
###Creating batting predictors for away games###
laa_bat_a$BAa <- laa_bat_a$BA
laa_bat_a$PAa <- laa_bat_a$PA
laa_bat_a$ABa <- laa_bat_a$AB
laa_bat_a$Ra <- laa_bat_a$R
laa_bat_a$Ha <- laa_bat_a$H
laa_bat_a$B2Ba <- laa_bat_a$X2B
laa_bat_a$B3Ba <- laa_bat_a$X3B
laa_bat_a$BHRa <- laa_bat_a$HR
laa_bat_a$RBIa <- laa_bat_a$RBI
laa_bat_a$BBBa <- laa_bat_a$BB
laa_bat_a$BSOa <- laa_bat_a$SO
laa_bat_a$SHa <- laa_bat_a$SH
laa_bat_a$SFa <- laa_bat_a$SF
laa_bat_a$SBa <- laa_bat_a$SB
###create away variable###
laa_bat_a$X <- ifelse(laa_bat_a$X =='@', team, laa_bat_a$X) #use of team variable name here
names(laa_bat_a)[names(laa_bat_a) == 'X'] <- 'away'
###change class of Date
laa_bat_a$Date <- as.character(laa_bat_a$Date)
###select key vars
laa_bat_a <- laa_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###laa_bat_h for batting vars (home)###
laa_bat_h <- laa_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
laa_bat_h <- laa_bat_h %>% filter(X == '') #home games
###runs for home games###
laa_bat_h$Rslt <- as.character(laa_bat_h$Rslt) #character vector
TEMP<- strsplit(x=laa_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
laa_bat_h$Rhome <- substring(TEMP$V1, 2); laa_bat_h$Rhome <- as.numeric(laa_bat_h$Rhome)
###Creating batting predictors for home games###
laa_bat_h$BAh <- laa_bat_h$BA
laa_bat_h$PAh <- laa_bat_h$PA
laa_bat_h$ABh <- laa_bat_h$AB
laa_bat_h$Rh <- laa_bat_h$R
laa_bat_h$Hh <- laa_bat_h$H
laa_bat_h$B2Bh <- laa_bat_h$X2B
laa_bat_h$B3Bh <- laa_bat_h$X3B
laa_bat_h$BHRh <- laa_bat_h$HR
laa_bat_h$RBIh <- laa_bat_h$RBI
laa_bat_h$BBBh <- laa_bat_h$BB
laa_bat_h$BSOh <- laa_bat_h$SO
laa_bat_h$SHh <- laa_bat_h$SH
laa_bat_h$SFh <- laa_bat_h$SF
laa_bat_h$SBh <- laa_bat_h$SB
###create home variable###
laa_bat_h$X <- ifelse(laa_bat_h$X =='', team, laa_bat_h$X) #use of team variable name here
names(laa_bat_h)[names(laa_bat_h) == 'X'] <- 'home'
###change class of Date
laa_bat_h$Date <- as.character(laa_bat_h$Date)
###select key vars
laa_bat_h <- laa_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: laa_pit###
laa_pit <- read.csv("laa_pit.txt")

###laa_pit_a for pitching variables (away)###
laa_pit_a <- laa_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
laa_pit_a <- laa_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(laa_pit_a)[names(laa_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(laa_pit_a)[names(laa_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
laa_pit_a$ERa <- laa_pit_a$ER
laa_pit_a$UERa <- laa_pit_a$UER
laa_pit_a$PBBa <- laa_pit_a$BB
laa_pit_a$PSOa <- laa_pit_a$SO
laa_pit_a$PHRa <- laa_pit_a$HR
laa_pit_a$BFa <- laa_pit_a$BF
laa_pit_a$Pita <- laa_pit_a$Pit
laa_pit_a$Stra <- laa_pit_a$Str
laa_pit_a$P2Ba <- laa_pit_a$X2B
laa_pit_a$P3Ba <- laa_pit_a$X3B
###create away variable###
laa_pit_a$X <- ifelse(laa_pit_a$X =='@', team, laa_pit_a$X) #use of team variable name here
names(laa_pit_a)[names(laa_pit_a) == 'X'] <- 'away'
###change class of Date
laa_pit_a$Date <- as.character(laa_pit_a$Date)
###select key vars
laa_pit_a <- laa_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###laa_pit_h for pitching variables (home)###
laa_pit_h <- laa_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
laa_pit_h <- laa_pit_h %>% filter(X == '') #home games
###splitting laa_pit_h vector###
names(laa_pit_h)[names(laa_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(laa_pit_h)[names(laa_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
laa_pit_h$ERh <- laa_pit_h$ER
laa_pit_h$UERh <- laa_pit_h$UER
laa_pit_h$PBBh <- laa_pit_h$BB
laa_pit_h$PSOh <- laa_pit_h$SO
laa_pit_h$PHRh <- laa_pit_h$HR
laa_pit_h$BFh <- laa_pit_h$BF
laa_pit_h$Pith <- laa_pit_h$Pit
laa_pit_h$Strh <- laa_pit_h$Str
laa_pit_h$P2Bh <- laa_pit_h$X2B
laa_pit_h$P3Bh <- laa_pit_h$X3B
###create home variable###
laa_pit_h$X <- ifelse(laa_pit_h$X =='', team, laa_pit_h$X) #use of team variable name here
names(laa_pit_h)[names(laa_pit_h) == 'X'] <- 'home'
###change class of Date
laa_pit_h$Date <- as.character(laa_pit_h$Date)
###filter down data
laa_pit_h <- laa_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
