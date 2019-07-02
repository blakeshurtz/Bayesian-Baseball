library(tidyverse)

###join team-level data to EXISTING game-level dataset
###import gamelog and teamfielding manually
mydata <- gamelog
###select key variables###
teamfielding <- teamfielding %>% select(Tm, PO, A, E, DP)
###create home and away datasets###
hometeamfielding <- teamfielding #home
names(hometeamfielding) <- c("home", "PO_h", "A_h", "E_h", "DP_h")
awayteamfielding <- teamfielding #away
names(awayteamfielding) <- c("away", "PO_a", "A_a", "E_a", "DP_a")
###
mydata <- left_join(mydata, hometeamfielding, by="home")  #note- coerce home from factor to character
mydata <- left_join(mydata, awayteamfielding, by="away")  #note- coerce away from factor to character

write.csv(mydata, file = "mydata.csv")
