###this script is for wrangling data pulled from baseball-reference.com
###data is manually downloaded and saved as a txt file

###standings data is team-level wins, losses, etc. for 2018
###import data manually
library(skimr)
class(standings)
standings <- standings[complete.cases(standings), ] #removed "total" row 
skim(standings) #30 observations for 30 teams

###teambatting data is team-level batting stats
###import data manually
class(teambatting)
skim(teambatting) #32 observations, (30 teams with two additional observatios for average and total)
library(tidyverse)
teambatting <- teambatting %>% dplyr::filter(Tm != "" & Tm != 'LgAvg') #remove average and total rows

###fielding data is team-level batting stats
###import data manually
class(fielding)
skim(fielding) #32 observations, (30 teams with two additional observatios for average and total)
library(tidyverse)
fielding <- fielding %>% dplyr::filter(Tm != "" & Tm != 'LgAvg') #remove average and total rows

###pitching data is team-level batting stats
###import data manually
class(pitching)
skim(pitching) #32 observations, (30 teams with two additional observatios for average and total)
library(tidyverse)
pitching <- pitching %>% dplyr::filter(Tm != "" & Tm != 'LgAvg') #remove average and total rows

###pitching data is team-level batting stats
###import data manually
class(management)
skim(management) #34 observations, (3 teams have two managers)
library(tidyverse)
management <- management %>% dplyr::filter(Tm != "" & Tm != 'LgAvg') #remove average and total rows
