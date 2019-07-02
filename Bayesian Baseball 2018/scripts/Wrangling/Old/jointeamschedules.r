library(dplyr)

#after importing data as txts
mydata <- rbind(arizonadiamondbacks, atlantabraves, baltimoreorioles, bostonredsox, chicagocubs)
mydata <- rbind(mydata, chicagowhitesox, cincinnatireds, clevelandindians, coloradorockies, detroittigers)
mydata <- rbind(mydata, houstonastros, kansascityroyals, laangels, ladodgers, miamimarlins, milwaukeebrewers)
mydata <- rbind(mydata, minnesotatwins, nymets, nyyankees, oaklandas, philadelphiaphillies, pittsburghpirates)
mydata <- rbind(mydata, sandiegopadres, sanfranciscogiants, seattlemariners, stlouiscardinals, tampabayrays)
mydata <- rbind(mydata, texasrangers, torontobluejays, washingtonnationals)
write.csv(mydata, "teamschedules.csv")

#note, data can be imported from data/teamschedules/teamschedule.csv
teamschedules <- read_csv("data/teamschedules/teamschedules.csv")
teamschedules <- teamschedules %>% dplyr::filter(X.1 == "@") #@ means an away game, blank means home
teamschedules <- teamschedules %>% select(Tm, Opp, R, RA)
colnames(teamschedules) <- c("tmr", "tmra", "r", "ra")

#uploading data
library(RMySQL)
con <- dbConnect(MySQL(),
                 user = 'blakeobeans',
                 password = 'MrJ0nes123',
                 host = 'mydbinstance4.c1uducbod6js.us-west-1.rds.amazonaws.com',
                 dbname='blakeobeans')

###uploading baseball data
dbWriteTable(conn = con, name = 'teamschedule', value = teamschedules, overwrite = TRUE) 
