library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
tex_bat <- read.csv("tex_bat.txt")

team <- 'TEX' #team name variable

###tex_homeaway for home/away###
tex_homeaway <- tex_bat %>% select(Date, Opp, X)
###create home variable
tex_homeaway$X <- as.character(tex_homeaway$X)
tex_homeaway$Opp <- as.character(tex_homeaway$Opp)
tex_homeaway$X <- ifelse(tex_homeaway$X =='@', tex_homeaway$Opp, tex_homeaway$X)
tex_homeaway$X <- ifelse(tex_homeaway$X =='', team, tex_homeaway$X) #use of team variable name here
names(tex_homeaway)[names(tex_homeaway) == 'X'] <- 'home'
###create away variable
tex_homeaway$Opp <- ifelse(tex_homeaway$Opp == tex_homeaway$home, team, tex_homeaway$Opp) #use of team variable name here
names(tex_homeaway)[names(tex_homeaway) == 'Opp'] <- 'away'
###change class of Date
tex_homeaway$Date <- as.character(tex_homeaway$Date)
###select key vars
tex_homeaway <- tex_homeaway %>% select(home, away, Date) 

###tex_bat_a for batting vars (away)###
tex_bat_a <- tex_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
tex_bat_a <- tex_bat_a %>% filter(X == '@') #away games
###runs for away games###
tex_bat_a$Rslt <- as.character(tex_bat_a$Rslt) #character vector
TEMP<- strsplit(x=tex_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tex_bat_a$Raway <- substring(TEMP$V1, 2); tex_bat_a$Raway <- as.numeric(tex_bat_a$Raway)
###Creating batting predictors for away games###
tex_bat_a$BAa <- tex_bat_a$BA
tex_bat_a$PAa <- tex_bat_a$PA
tex_bat_a$ABa <- tex_bat_a$AB
tex_bat_a$Ra <- tex_bat_a$R
tex_bat_a$Ha <- tex_bat_a$H
tex_bat_a$B2Ba <- tex_bat_a$X2B
tex_bat_a$B3Ba <- tex_bat_a$X3B
tex_bat_a$BHRa <- tex_bat_a$HR
tex_bat_a$RBIa <- tex_bat_a$RBI
tex_bat_a$BBBa <- tex_bat_a$BB
tex_bat_a$BSOa <- tex_bat_a$SO
tex_bat_a$SHa <- tex_bat_a$SH
tex_bat_a$SFa <- tex_bat_a$SF
tex_bat_a$SBa <- tex_bat_a$SB
###create away variable###
tex_bat_a$X <- ifelse(tex_bat_a$X =='@', team, tex_bat_a$X) #use of team variable name here
names(tex_bat_a)[names(tex_bat_a) == 'X'] <- 'away'
###change class of Date
tex_bat_a$Date <- as.character(tex_bat_a$Date)
###select key vars
tex_bat_a <- tex_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###tex_bat_h for batting vars (home)###
tex_bat_h <- tex_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
tex_bat_h <- tex_bat_h %>% filter(X == '') #home games
###runs for home games###
tex_bat_h$Rslt <- as.character(tex_bat_h$Rslt) #character vector
TEMP<- strsplit(x=tex_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tex_bat_h$Rhome <- substring(TEMP$V1, 2); tex_bat_h$Rhome <- as.numeric(tex_bat_h$Rhome)
###Creating batting predictors for home games###
tex_bat_h$BAh <- tex_bat_h$BA
tex_bat_h$PAh <- tex_bat_h$PA
tex_bat_h$ABh <- tex_bat_h$AB
tex_bat_h$Rh <- tex_bat_h$R
tex_bat_h$Hh <- tex_bat_h$H
tex_bat_h$B2Bh <- tex_bat_h$X2B
tex_bat_h$B3Bh <- tex_bat_h$X3B
tex_bat_h$BHRh <- tex_bat_h$HR
tex_bat_h$RBIh <- tex_bat_h$RBI
tex_bat_h$BBBh <- tex_bat_h$BB
tex_bat_h$BSOh <- tex_bat_h$SO
tex_bat_h$SHh <- tex_bat_h$SH
tex_bat_h$SFh <- tex_bat_h$SF
tex_bat_h$SBh <- tex_bat_h$SB
###create home variable###
tex_bat_h$X <- ifelse(tex_bat_h$X =='', team, tex_bat_h$X) #use of team variable name here
names(tex_bat_h)[names(tex_bat_h) == 'X'] <- 'home'
###change class of Date
tex_bat_h$Date <- as.character(tex_bat_h$Date)
###select key vars
tex_bat_h <- tex_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: tex_pit###
tex_pit <- read.csv("tex_pit.txt")

###tex_pit_a for pitching variables (away)###
tex_pit_a <- tex_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
tex_pit_a <- tex_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(tex_pit_a)[names(tex_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(tex_pit_a)[names(tex_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
tex_pit_a$ERa <- tex_pit_a$ER
tex_pit_a$UERa <- tex_pit_a$UER
tex_pit_a$PBBa <- tex_pit_a$BB
tex_pit_a$PSOa <- tex_pit_a$SO
tex_pit_a$PHRa <- tex_pit_a$HR
tex_pit_a$BFa <- tex_pit_a$BF
tex_pit_a$Pita <- tex_pit_a$Pit
tex_pit_a$Stra <- tex_pit_a$Str
tex_pit_a$P2Ba <- tex_pit_a$X2B
tex_pit_a$P3Ba <- tex_pit_a$X3B
###create away variable###
tex_pit_a$X <- ifelse(tex_pit_a$X =='@', team, tex_pit_a$X) #use of team variable name here
names(tex_pit_a)[names(tex_pit_a) == 'X'] <- 'away'
###change class of Date
tex_pit_a$Date <- as.character(tex_pit_a$Date)
###select key vars
tex_pit_a <- tex_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###tex_pit_h for pitching variables (home)###
tex_pit_h <- tex_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
tex_pit_h <- tex_pit_h %>% filter(X == '') #home games
###splitting tex_pit_h vector###
names(tex_pit_h)[names(tex_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(tex_pit_h)[names(tex_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
tex_pit_h$ERh <- tex_pit_h$ER
tex_pit_h$UERh <- tex_pit_h$UER
tex_pit_h$PBBh <- tex_pit_h$BB
tex_pit_h$PSOh <- tex_pit_h$SO
tex_pit_h$PHRh <- tex_pit_h$HR
tex_pit_h$BFh <- tex_pit_h$BF
tex_pit_h$Pith <- tex_pit_h$Pit
tex_pit_h$Strh <- tex_pit_h$Str
tex_pit_h$P2Bh <- tex_pit_h$X2B
tex_pit_h$P3Bh <- tex_pit_h$X3B
###create home variable###
tex_pit_h$X <- ifelse(tex_pit_h$X =='', team, tex_pit_h$X) #use of team variable name here
names(tex_pit_h)[names(tex_pit_h) == 'X'] <- 'home'
###change class of Date
tex_pit_h$Date <- as.character(tex_pit_h$Date)
###filter down data
tex_pit_h <- tex_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)
