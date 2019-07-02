###Downloading the Data

library(RMariaDB)
###connect to AWS database to download data (pre-wrangled :)
con <- dbConnect(MariaDB(),
                 user = 'guest',
                 password = 'password',
                 host = 'mydbinstance4.c1uducbod6js.us-west-1.rds.amazonaws.com',
                 dbname='bayesianbaseball')

mydata <- dbReadTable(conn = con, name = 'mydata', value = mydata, overwrite = TRUE)
