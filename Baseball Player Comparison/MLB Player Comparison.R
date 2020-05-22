

library(baseballr)

# ===================================================
# Pythagorean Wins Question
# ===================================================

#pull final standings for last 5 years

standings2015 <- standings_on_date_bref('2015-10-05', division = c('AL Overall','NL Overall'))
standings2016 <- standings_on_date_bref('2016-10-05', division = c('AL Overall','NL Overall'))
standings2017 <- standings_on_date_bref('2017-10-05', division = c('AL Overall','NL Overall'))
standings2018 <- standings_on_date_bref('2018-10-05', division = c('AL Overall','NL Overall'))
standings2019 <- standings_on_date_bref('2019-10-05', division = c('AL Overall','NL Overall'))

#these all come as lists, so convert them to dataframes

standings2015df <- data.frame(Reduce(rbind, standings2015))
standings2016df <- data.frame(Reduce(rbind, standings2016))
standings2017df <- data.frame(Reduce(rbind, standings2017))
standings2018df <- data.frame(Reduce(rbind, standings2018))
standings2019df <- data.frame(Reduce(rbind, standings2019))

standings2015df$Year <- 2015
standings2016df$Year <- 2016
standings2017df$Year <- 2017
standings2018df$Year <- 2018
standings2019df$Year <- 2019

#combine all dataframes into 1

standings <- do.call('rbind', list(standings2015df,standings2016df,standings2017df,standings2018df,standings2019df))
standings <- subset(standings, select = c('Tm','W','L','RS','RA','Year'))

#create run differential and winning percentage
standings$RD <- with(standings, RS - RA)
standings$WinPct <- with(standings, W/(W+L))

#create expected win percentage and difference from actual
#first create runs ratio
standings$ScoreRatio <- with(standings, RS/RA)
standings$ExpWinPct <- with(standings, ScoreRatio^2 / ((ScoreRatio^2) + 1))
standings$WinPctDiff <- with(standings, WinPct - ExpWinPct)

#use log functions to create wins and runs ratios
standings$WratioLog <- log(standings$W / standings$L)
standings$RratioLog <- log(standings$RS / standings$RA)

#use a linear regression to determine the best exponent for Pythagorean Wins
ExpWins <- lm(WratioLog ~ 0 + RratioLog, data=standings)

exponent <- coef(summary(ExpWins))["RratioLog","Estimate"]

#add new columns that applies new exponent to calculation
#calculate final wins total 
standings$NewExpWinPct <- with(standings, ScoreRatio^exponent / ((ScoreRatio^exponent) + 1))
standings$NewW <- with(standings, (W+L)*NewExpWinPct)
standings$Wdiff <- with(standings, W - NewW)

standings[standings$Tm =="CLE",]

#test errors of original and new exponents
cat('RMSE w/ Exponent = 2:', sqrt(mean((standings$WinPct - standings$ExpWinPct)^2)))
cat('RMSE w/ Exponent = 1.762: ', sqrt(mean((standings$WinPct - standings$NewExpWinPct)^2)))

## ====================================================================================
## Over the last 5 seasons, 2015-2019, the Indians underperformed by 9 wins in total.
## Underperformed by nearly 4 and 6 wins in 2017 and 2018 alone
## error value went from 0.5076 to 0.0263
## ====================================================================================

#built scatter plots for last 3 seasons comparing runs scored and allowed
color <- ifelse(standings$Tm == 'CLE', "red","black")
symb <- ifelse(standings$Tm == 'CLE', 17,16)
size <- ifelse(standings$Tm == 'CLE', 2,1)
RunDiff2017 <- subset(standings, Year == 2017, select=c('RS','RA'))
RunDiff2018 <- subset(standings, Year == 2018, select=c('RS','RA'))
RunDiff2019 <- subset(standings, Year == 2019, select=c('RS','RA'))

par(mfrow=c(1,3))

plot(x=RunDiff2017$RS, y=RunDiff2017$RA
     ,main = '2017 RS vs RA', col = color, pch = symb, cex = size
     ,xlab = 'Runs Scored', ylab = 'Runs Allowed')
abline(h = mean(RunDiff2017$RA), lty = 2)
abline(v = mean(RunDiff2017$RS), lty = 2)

plot(x=RunDiff2018$RS, y=RunDiff2018$RA
     ,main = '2018 RS vs RA', col = color, pch = symb, cex = size
     ,xlab = 'Runs Scored', ylab = 'Runs Allowed')
abline(h = mean(RunDiff2018$RA), lty = 2)
abline(v = mean(RunDiff2018$RS), lty = 2)

plot(x=RunDiff2019$RS, y=RunDiff2019$RA
     ,main = '2019 RS vs RA', col = color, pch = symb, cex = size
     ,xlab = 'Runs Scored', ylab = 'Runs Allowed')
abline(h = mean(RunDiff2019$RA), lty = 2)
abline(v = mean(RunDiff2019$RS), lty = 2)


# ===================================================
# Player Analysis
# ===================================================

#Used FanGraphs to create custom csv files for each position group for 2019 data
#include only players with at least 100 plate appearances for each position

setwd('C:/Users/a54838/Desktop/Northwestern/MSDS 456/Baseball Assignment')
Pos2B <- read.csv('2B.csv', header = TRUE)
Pos2B <- subset(Pos2B, PA > 100)
PosSS <- read.csv('SS.csv', header = TRUE)
PosSS <- subset(PosSS, PA > 100)
Pos3B <- read.csv('3B.csv', header = TRUE)
Pos3B <- subset(Pos3B, PA > 100)

CLEPlayers <- list(9776, 12916, 13510)



# ==== Create a function to speed up processing for the different metrics for each position ====

measure <- function(x){
  #x is the data input
  ageAvg = mean(x$Age)
  OPSavg = mean(x$OPS)
  BABIPavg = mean(x$BABIP)
  KtoBB = mean(x$BB.K)
  ZoneContact = mean(x$InZoneContact)
  
  CLEAge = subset(x,is.element(playerid,CLEPlayers), select = Age)
  CLEOPS = subset(x, is.element(playerid,CLEPlayers), select = OPS)
  CLEBABIP = subset(x, is.element(playerid, CLEPlayers), select = BABIP)
  CLEKtoBB = subset(x, is.element(playerid, CLEPlayers), select = BB.K)
  CLEZoneContact = subset(x, is.element(playerid, CLEPlayers), select = InZoneContact)

  return(list("CLE Age" = CLEAge,
              "Avg Age" = ageAvg,
              "CLE OPS" = CLEOPS,
              "OPSAvg" = OPSavg,
              "CLE BABIP" = CLEBABIP,
              "Avg BABIP" = BABIPavg,
              "CLEKtoBB" = CLEKtoBB,
              "Avg KtoBB" = KtoBB,
              "CLE Contact" = CLEZoneContact,
              "Avg Contact" = ZoneContact))
}


# ==== Analysis for each Position ====

measure(Pos2B)
measure(PosSS)
measure(Pos3B)


# =================================================
# Runs Created Analysis
# Looking only at 2B position players
# =================================================

# Update metrics to 2B table for runs created
# total bases, season runs created, and per game runs created

Pos2B$TB <- with(Pos2B, X1B + 2*X2B + 3*X3B + 4*HR)

#using formulas as outlined in Chapter 2 of Mathletics
Pos2B$RCreated <- with(Pos2B, ((H + BB + HBP)*TB)/(AB+BB+HBP))
Pos2B$RCreatedGame <- with(Pos2B, (RCreated)/(((.982*AB)-H+GDP+SF+SH+CS)/26.72))

#Look at 2Bs in Tampa
Pos2B[Pos2B$Team == 'Rays',]


# create plot to show all Runs Created vs Runs Created per game for all 2B

par(mfrow = c(1,1))

color2B <- ifelse(Pos2B$playerid == 9776,"red", ifelse(Pos2B$Team == 'Rays',"blue", "black"))
symb2B <- ifelse(Pos2B$playerid == 9776, 17,ifelse(Pos2B$Team == 'Rays',15, 16))
size2B <- ifelse(Pos2B$playerid == 9776, 2,ifelse(Pos2B$Team == 'Rays', 2, 1))

plot(x=Pos2B$RCreated, y=Pos2B$RCreatedGame
     ,main = '2019 Runs Created', col = color2B, pch = symb2B, cex = size2B
     ,xlab = 'Runs Created - Season', ylab = 'Runs Created - Per Game')
text(x=52, y=7, labels='Brandon Lowe')
text(x=58, y=4, labels='Jason Kipnis') 




