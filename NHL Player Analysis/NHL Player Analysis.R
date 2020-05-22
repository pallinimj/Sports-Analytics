
## Load libraries that are used throughout

library(dplyr)
library(ggplot2)
library(NbClust)
library(sqldf)
library(teamcolors)

set.seed(1234)


## ===========================================
## Import data files
## ===========================================

setwd('C:/Users/a54838/Desktop/Northwestern/MSDS 456/Final Project')

# salary cap history
salary_cap <- read.csv('Salary Cap.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

# 2019 salary data
salaries <- read.csv('NHL Salaries.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

# team standings
team2018 <- read.csv('Standings 2018.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
team2019 <- read.csv('Standings 2019.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
team2020 <- read.csv('Standings 2020.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

teamstats <- do.call('rbind', list(team2018,team2019,team2020))
teamstats[is.na(teamstats)] <- 0

# goalie stats
goalie2018 <- read.csv('Goalies 2018.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
goalie2019 <- read.csv('Goalies 2019.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
goalie2020 <- read.csv('Goalies 2020.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

goaliestats <- do.call('rbind', list(goalie2018,goalie2019,goalie2020))
goaliestats[is.na(goaliestats)] <- 0

# player standard stats
players2018 <- read.csv('Players 2018.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
players2019 <- read.csv('Players 2019.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
players2020 <- read.csv('Players 2020.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

playerstats <- do.call('rbind', list(players2018,players2019,players2020))


# player advanced stats
advanced2018 <- read.csv('Advanced 2018.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
advanced2019 <- read.csv('Advanced 2019.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
advanced2020 <- read.csv('Advanced 2020.csv', stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

advancedstats <- do.call('rbind', list(advanced2018,advanced2019,advanced2020))

# combine standard and advanced stats for skaters

skaterstats <- merge(playerstats, advancedstats[,c('PlayerId','Tm','Season','CF','CA','FF','FA','PDO','OZoneStarts','DZoneStarts','TOIper60','TOIEven','SOGPercent')]
                     ,by = c('PlayerId','Tm','Season'))

# clean up data types
skaterstats$OZoneStarts <- as.numeric(skaterstats$OZoneStarts)
skaterstats$DZoneStarts <- as.numeric(skaterstats$DZoneStarts)
skaterstats$TOI <- as.numeric(skaterstats$TOI)
skaterstats$ATOI <- as.numeric(as.difftime(skaterstats$ATOI, format='%M:%S', units = 'mins'))
skaterstats$TOIper60 <- as.numeric(as.difftime(skaterstats$TOIper60, format='%M:%S', units='mins'))

#add total corsi/fenwick scores for each player
skaterstats$Corsi <- skaterstats$CA/skaterstats$CF
skaterstats$Fenwick <- skaterstats$FA/skaterstats$FF

#replace any NAs
skaterstats[is.na(skaterstats)] <- 0

## ====================================
## Clean up name fields
## needed to add in salary data later
## ====================================

Name <- function(col) {
  clean <- gsub(pattern = "([A-Za-z]\\s*)\\\\[0-9a-zA-Z\\s]+", replacement = "\\1", x=col)
  clean <- gsub(pattern = ".", replacement = "", x=clean, fixed=TRUE)
  clean <- gsub(pattern = "-", replacement = " ", x=clean, fixed=TRUE)
  clean <- gsub(pattern = "'", replacement = "", x=clean, fixed=TRUE)
  clean <- gsub(pattern = " ", replacement = ".", x=clean, fixed=TRUE)
  return(toupper(clean))
}

skaterstats$Player <- Name(skaterstats$Player)
goaliestats$Player <- Name(goaliestats$Player)
salaries$Player <- Name(salaries$Player)


## =========================================
## Build chart showing salary cap trend
## =========================================
salary_cap %>%
  ggplot(aes(x=Year, y=Cap)) +
    geom_line(color = '#69b3a2') +
    geom_point(color = '#69b3a2', size = 4) + geom_text(aes(label=Cap), hjust=.5, vjust=-1.5) +
    ggtitle('NHL Salary Cap by Year') +
    xlab('Year') +
    ylab('Salary Cap $ (millions)') +
    ylim(50, 90) + 
    theme_minimal()


## ==========================================================
## Create clustering for Goalie analysis
## using same # of clusters, but running for each season
## will show if/how players have changed year to year
## ==========================================================

head(goaliestats)

goaliestats$BadStartPercent <- goaliestats$BadStarts/goaliestats$GS
goaliestats$WinPercent <- goaliestats$W/goaliestats$GS
goaliestats$SOPercent <- goaliestats$SO/goaliestats$GS

# need at least 20 starts in a season
goaliestats <- subset(goaliestats, GS >= 20)

goaliestats[is.na(goaliestats)] <- 0

# use linear model that predicts wins to determine important features
goaliefeatures <- lm(W ~ GS + SavePercent + GAA + WinPercent + SOPercent + QualityStartPercent + BadStartPercent + 0,
                     data = goaliestats)

summary(goaliefeatures)

# create cluster for each season using only those features
# 2018 first
goalie2018 <- subset(goaliestats, Season == 2018,
                     select = c('Player','PlayerId','Tm','W','GS','SavePercent','WinPercent','QualityStartPercent','BadStartPercent'))
goalie2018_features <- subset(goalie2018, select = c('W','GS','SavePercent','WinPercent','QualityStartPercent','BadStartPercent'))
goalie2018_features <- scale(goalie2018_features)

goalie2018_clust <- kmeans(goalie2018_features, centers = 3, nstart = 5)

goalie2018$cluster <- goalie2018_clust$cluster

#look at averages for each cluster to define the numbers
goalie2018 %>%
  group_by(cluster) %>%
  summarise(Starts = mean(GS),
            Wins = mean(W),
            SavePercent = mean(SavePercent),
            WinPercent = mean(WinPercent),
            QualityStarts = mean(QualityStartPercent),
            BadStarts = mean(BadStartPercent))

goalie2018$Type <- ifelse(goalie2018$cluster == 1, 'Starter', ifelse(goalie2018$cluster == 2, 'Average', 'Backup'))

# 2019 cluster
goalie2019 <- subset(goaliestats, Season == 2019,
                     select = c('Player','PlayerId','Tm','W','GS','SavePercent','WinPercent','QualityStartPercent','BadStartPercent'))
goalie2019_features <- subset(goalie2019, select = c('W','GS','SavePercent','WinPercent','QualityStartPercent','BadStartPercent'))
goalie2019_features <- scale(goalie2019_features)

goalie2019_clust <- kmeans(goalie2019_features, centers = 3, nstart = 5)

goalie2019$cluster <- goalie2019_clust$cluster

goalie2019 %>%
  group_by(cluster) %>%
  summarise(Starts = mean(GS),
            Wins = mean(W),
            SavePercent = mean(SavePercent),
            WinPercent = mean(WinPercent),
            QualityStarts = mean(QualityStartPercent),
            BadStarts = mean(BadStartPercent))

goalie2019$Type <- ifelse(goalie2019$cluster == 3, 'Starter', ifelse(goalie2018$cluster == 1, 'Backup', 'Average'))


# 2020 cluster
goalie2020 <- subset(goaliestats, Season == 2020,
                     select = c('Player','PlayerId','Tm','W','GS','SavePercent','WinPercent','QualityStartPercent','BadStartPercent'))
goalie2020_features <- subset(goalie2020, select = c('W','GS','SavePercent','WinPercent','QualityStartPercent','BadStartPercent'))
goalie2020_features <- scale(goalie2020_features)

goalie2020_clust <- kmeans(goalie2020_features, centers = 3, nstart = 5)

goalie2020$cluster <- goalie2020_clust$cluster

goalie2020 %>%
  group_by(cluster) %>%
  summarise(Starts = mean(GS),
            Wins = mean(W),
            SavePercent = mean(SavePercent),
            WinPercent = mean(WinPercent),
            QualityStarts = mean(QualityStartPercent),
            BadStarts = mean(BadStartPercent))

goalie2020$Type <- ifelse(goalie2020$cluster == 2, 'Starter', ifelse(goalie2018$cluster == 1, 'Backup', 'Average'))


## ==========================================================
## Create clustering for Defensemen
## using same # of clusters, but running for each season
## will show if/how players have changed year to year
## ==========================================================

defensemen <- subset(skaterstats, Pos == 'D' & GP >= 20)

defensemen[is.na(defensemen)] <- 0

d_features <- lm(PlusMinus ~ PTS + PIM + BLK + HIT + Corsi + Fenwick + PDO + OZoneStarts + DZoneStarts + 0,
                 data = defensemen)

summary(d_features)

## 2018 clustering

d_2018 <- subset(defensemen, Season == 2018, select = c('Player','PlayerId','Tm','PlusMinus','PTS','Fenwick','PDO','OZoneStarts','DZoneStarts'))
d_2018_features <- subset(d_2018, select = c('PlusMinus','PTS','Fenwick','PDO','OZoneStarts','DZoneStarts'))
d_2018_features <- scale(d_2018_features)

d_2018_clust <- kmeans(d_2018_features, centers = 3, nstart = 5)

d_2018$cluster <- d_2018_clust$cluster

d_2018 %>%
  group_by(cluster) %>%
  summarise(PlusMinus = mean(PlusMinus),
            Points = mean(PTS),
            Fenwick = mean(Fenwick),
            PDO = mean(PDO),
            OZoneStarts = mean(OZoneStarts),
            DZoneStarts = mean(DZoneStarts))

d_2018$Type <- ifelse(d_2018$cluster == 1, 'Top Line', ifelse(d_2018$cluster == 2, 'Defensive', 'Offensive'))


## 2019 clustering

d_2019 <- subset(defensemen, Season == 2019, select = c('Player','PlayerId','Tm','PlusMinus','PTS','Fenwick','PDO','OZoneStarts','DZoneStarts'))
d_2019_features <- subset(d_2019, select = c('PlusMinus','PTS','Fenwick','PDO','OZoneStarts','DZoneStarts'))
d_2019_features <- scale(d_2019_features)

d_2019_clust <- kmeans(d_2019_features, centers = 3, nstart = 5)

d_2019$cluster <- d_2019_clust$cluster

d_2019 %>%
  group_by(cluster) %>%
  summarise(PlusMinus = mean(PlusMinus),
            Points = mean(PTS),
            Fenwick = mean(Fenwick),
            PDO = mean(PDO),
            OZoneStarts = mean(OZoneStarts),
            DZoneStarts = mean(DZoneStarts))

d_2019$Type <- ifelse(d_2019$cluster == 3, 'Top Line', ifelse(d_2019$cluster == 2, 'Defensive', 'Offensive'))


## 2020 clustering

d_2020 <- subset(defensemen, Season == 2020, select = c('Player','PlayerId','Tm','PlusMinus','PTS','Fenwick','PDO','OZoneStarts','DZoneStarts'))
d_2020_features <- subset(d_2020, select = c('PlusMinus','PTS','Fenwick','PDO','OZoneStarts','DZoneStarts'))
d_2020_features <- scale(d_2020_features)

d_2020_clust <- kmeans(d_2020_features, centers = 3, nstart = 5)

d_2020$cluster <- d_2020_clust$cluster

d_2020 %>%
  group_by(cluster) %>%
  summarise(PlusMinus = mean(PlusMinus),
            Points = mean(PTS),
            Fenwick = mean(Fenwick),
            PDO = mean(PDO),
            OZoneStarts = mean(OZoneStarts),
            DZoneStarts = mean(DZoneStarts))

d_2020$Type <- ifelse(d_2020$cluster == 3, 'Top Line', ifelse(d_2020$cluster == 2, 'Defensive', 'Offensive'))


## ==========================================================
## Create clustering for Forwards
## using same # of clusters, but running for each season
## will show if/how players have changed year to year
## ==========================================================

forwards <- subset(skaterstats, Pos != 'D' & GP >= 20)

forwards[is.na(forwards)] <- 0

f_features <- lm(PlusMinus ~ G + A + PIM + BLK + HIT + FO_Percent + PDO + OZoneStarts + DZoneStarts + SOGPercent + Corsi + Fenwick +0
                 ,data = forwards)

summary(f_features)


## 2018 clustering

f_2018 <- subset(forwards, Season == 2018, select = c('Player','PlayerId','Tm','PlusMinus','G','A','PDO','Fenwick','BLK','OZoneStarts','DZoneStarts'))
f_2018_features <- subset(f_2018, select = c('PlusMinus','G','A','PDO','Fenwick','BLK','OZoneStarts','DZoneStarts'))
f_2018_features <- scale(f_2018_features)

f_2018_clust <- kmeans(f_2018_features, centers = 4, nstart = 5)

f_2018$cluster <- f_2018_clust$cluster

f_2018 %>%
  group_by(cluster) %>%
  summarise(PlusMinus = mean(PlusMinus),
            Goals = mean(G),
            Assists = mean(A),
            PDO = mean(PDO),
            Fenwick = mean(Fenwick),
            Blocks = mean(BLK),
            OZoneStarts = mean(OZoneStarts),
            DZoneStarts = mean(DZoneStarts))

f_2018$Type <- ifelse(f_2018$cluster == 2, 'Top Line', 
                       ifelse(f_2018$cluster == 1, 'Defensive',
                              ifelse(f_2018$cluster == 3, 'Bottom 6', 'Two Way')))


## 2019 clustering

f_2019 <- subset(forwards, Season == 2019, select = c('Player','PlayerId','Tm','PlusMinus','G','A','PDO','Fenwick','BLK','OZoneStarts','DZoneStarts'))
f_2019_features <- subset(f_2019, select = c('PlusMinus','G','A','PDO','Fenwick','BLK','OZoneStarts','DZoneStarts'))
f_2019_features <- scale(f_2019_features)

f_2019_clust <- kmeans(f_2019_features, centers = 4, nstart = 5)

f_2019$cluster <- f_2019_clust$cluster

f_2019 %>%
  group_by(cluster) %>%
  summarise(PlusMinus = mean(PlusMinus),
            Goals = mean(G),
            Assists = mean(A),
            PDO = mean(PDO),
            Fenwick = mean(Fenwick),
            Blocks = mean(BLK),
            OZoneStarts = mean(OZoneStarts),
            DZoneStarts = mean(DZoneStarts))

f_2019$Type <- ifelse(f_2019$cluster == 1, 'Top Line', 
                      ifelse(f_2019$cluster == 3, 'Defensive',
                             ifelse(f_2019$cluster == 2, 'Bottom 6', 'Two Way')))


## 2020 clustering

f_2020 <- subset(forwards, Season == 2020, select = c('Player','PlayerId','Tm','PlusMinus','G','A','PDO','Fenwick','BLK','OZoneStarts','DZoneStarts'))
f_2020_features <- subset(f_2020, select = c('PlusMinus','G','A','PDO','Fenwick','BLK','OZoneStarts','DZoneStarts'))
f_2020_features <- scale(f_2020_features)

f_2020_clust <- kmeans(f_2020_features, centers = 4, nstart = 5)

f_2020$cluster <- f_2020_clust$cluster

f_2020 %>%
  group_by(cluster) %>%
  summarise(PlusMinus = mean(PlusMinus),
            Goals = mean(G),
            Assists = mean(A),
            PDO = mean(PDO),
            Fenwick = mean(Fenwick),
            Blocks = mean(BLK),
            OZoneStarts = mean(OZoneStarts),
            DZoneStarts = mean(DZoneStarts))

f_2020$Type <- ifelse(f_2020$cluster == 1, 'Top Line', 
                      ifelse(f_2020$cluster == 3, 'Defensive',
                             ifelse(f_2020$cluster == 4, 'Bottom 6', 'Two Way')))



## =============================================================================
## Include salary information
## combine with 2020 player tables
## =============================================================================

## Clean up salary table to include only records for current contracts

salary_current <- salaries %>%
  group_by(Player) %>%
  filter((n() > 1 & FreeAgentYear == 2020) | n() == 1)

## create table for future contracts
salary_future <- salaries %>%
  group_by(Player) %>%
  filter((n() > 1 & FreeAgentYear > 2020) | n() == 1)

# merge salaries with skaters
skaters2020 <- subset(skaterstats, Season == 2020)
skaters2020 <- merge(skaters2020, salary_current, by = 'Player')

# merge salaries with goalies
goalies2020 <- subset(goaliestats, Season == 2020)
goalies2020 <- merge(goalies2020, salary_current, by = 'Player')
goalies2020 <- goalies2020[!duplicated(goalies2020),]



# create total salary for each team
skatersalary <- skaters2020 %>%
                filter(Tm != 'TOT') %>%
                group_by(Tm) %>%
                summarise(Salary = sum(Average))

goaliesalary <- goalies2020 %>%
                filter(Tm != 'TOT') %>%
                group_by(Tm) %>%
                summarise(Salary = sum(Average))

# show total salaries for each team

teamsalary <- sqldf('Select a.Tm, a.GP, a.W, a.L, a.OL, a.PTS
                    ,b.Salary as SkaterSalary
                    ,c.Salary as GoalieSalary
                    ,b.Salary + c.Salary as TotalSalary
                    From team2020 a
                    left join skatersalary b
                    on a.Tm = b.Tm
                    left join goaliesalary c
                    on a.Tm = c.Tm')

teamsalary$salary_millions <- teamsalary$TotalSalary/1000000
  
## build team colors for plotting
nhl_colors <- teamcolors %>% filter(league == 'nhl')

chi_color <- nhl_colors %>% filter(name == 'Chicago Blackhawks') %>% pull(secondary)
bos_color <- nhl_colors %>% filter(name == 'Boston Bruins') %>% pull(primary)

# for highlighting just Chicago
highlight_chi <- teamsalary %>%
                  filter(Tm == 'CHI')

# for highlighting Boston (highest points)
highlight_bos <- teamsalary %>%
                  filter(Tm == 'BOS')

## plot for points vs salary

ggplot(teamsalary, aes(x = salary_millions, y = PTS)) +
      geom_point(color = '#999999', size = 2) +
      geom_point(data = highlight_chi, aes(x = salary_millions, y = PTS), color = 'red', size = 3) +
      geom_text(data = highlight_chi, aes(label=Tm), color = 'red', hjust=.5, vjust=-1.5) +
      geom_point(data = highlight_bos, aes(x = salary_millions, y = PTS), color = bos_color, size = 3) +
      geom_text(data = highlight_bos, aes(label = Tm), color = bos_color, hjust=.5, vjust=1.5) +
      scale_x_continuous(breaks = seq(45,90,5)) +
      scale_y_continuous(breaks=seq(35,95,10))+
      labs(
        x = 'Total Team Salary (Millions $)',
        y = 'Season Points',
        title = 'NHL Points vs Salary - 2020 Season'
      ) + theme_bw()


## =============================================================================
## Analyze average salary for each team by player type
## Show strip charts for F, D, G
## =============================================================================

# create functions to find the median salary for each player type by team
# based on player type across the league

# median salary

mean_salary <- function(df) {
  return (df %>% 
            filter(!is.na(Average)) %>%
            group_by(Tm, Type) %>%
            summarise(AvgSalary = mean(Average),
                      AvgSalary_Mil = mean(Average)/1000000)
          )
}


#update forwards to include salary info

f_2020 <- sqldf('Select a.*, b.Yrs, b.Dollars, b.Average, b.FreeAgentYear
                From f_2020 a
                left join skaters2020 b
                on a.PlayerId = b.PlayerId')


f_types_avg <- mean_salary(f_2020)



# distribution of Forwards
ggplot(f_types_avg, aes(x = Type, y = AvgSalary_Mil)) +
      geom_jitter(position=position_jitter(0.2), color = ifelse(f_types_avg$Tm == 'CHI', 'red','#999999'),
                  size = ifelse(f_types_avg$Tm == 'CHI', 3, 2)) +
      scale_x_discrete(limits=c('Top Line','Two Way','Defensive','Bottom 6')) +
      stat_summary(fun.y = mean, geom = 'point', color = 'black', size = 3) +
      labs(
          x = 'Player Type',
          y = 'Average Salary (Millions $)',
          title = 'Average Team Salary by Player Type - 2020 Season',
          subtitle = 'Forward types only'
        ) + theme_bw()


## Defensemen salary comparison

d_2020 <- sqldf('Select a.*, b.Yrs, b.Dollars, b.Average, b.FreeAgentYear
                From d_2020 a
                left join skaters2020 b
                on a.PlayerId = b.PlayerId')


d_types_avg <- mean_salary(d_2020)



# distribution of Defensemen
ggplot(d_types_avg, aes(x = Type, y = AvgSalary_Mil)) +
  geom_jitter(position=position_jitter(0.2), color = ifelse(d_types_avg$Tm == 'CHI', 'red','#999999'),
              size = ifelse(d_types_avg$Tm == 'CHI', 3, 2)) +
  scale_x_discrete(limits=c('Top Line','Offensive','Defensive')) +
  stat_summary(fun.y = mean, geom = 'point', color = 'black', size = 3) +
  labs(
    x = 'Player Type',
    y = 'Average Salary (Millions $)',
    title = 'Average Team Salary by Player Type - 2020 Season',
    subtitle = 'Defensemen types only'
  ) + theme_bw()


## Goalie salary comparison

goalie2020 <- sqldf('Select a.*, b.Yrs, b.Dollars, b.Average, b.FreeAgentYear
                From goalie2020 a
                left join goalies2020 b
                on a.PlayerId = b.PlayerId')


g_types_avg <- mean_salary(goalie2020)



# distribution of Goalies
ggplot(g_types_avg, aes(x = Type, y = AvgSalary_Mil)) +
  geom_jitter(position=position_jitter(0.2), color = ifelse(g_types_avg$Tm == 'CHI', 'red','#999999'),
              size = ifelse(g_types_avg$Tm == 'CHI', 3, 2)) +
  scale_x_discrete(limits=c('Starter','Average','Backup')) +
  stat_summary(fun.y = mean, geom = 'point', color = 'black', size = 3) +
  labs(
    x = 'Player Type',
    y = 'Average Salary (Millions $)',
    title = 'Average Team Salary by Player Type - 2020 Season',
    subtitle = 'Goalie types only'
  ) + theme_bw()


## ============================================================
## Show all Chicago player salaries
## ============================================================

chi_f <- f_2020 %>%
        filter(Tm == 'CHI' & !is.na(Average))

chi_f <- subset(chi_f, select = c('Player','Type','Yrs','Average','FreeAgentYear'))

chi_d <- d_2020 %>%
  filter(Tm == 'CHI' & !is.na(Average))

chi_d <- subset(chi_d, select = c('Player','Type','Yrs','Average','FreeAgentYear'))

chi_g <- goalie2020 %>%
  filter(Tm == 'CHI' & !is.na(Average))

chi_g <- subset(chi_g, select = c('Player','Type','Yrs','Average','FreeAgentYear'))

chi_all <- do.call('rbind', list(chi_f,chi_d,chi_g))
chi_all <- chi_all[!duplicated(chi_all),]
chi_all$Average_Mill <- round(chi_all$Average/1000000,2)


ggplot(data = chi_all, aes(x = reorder(Player, Average_Mill, sum), y = Average_Mill, fill = factor(Type))) +
  geom_bar(position = 'dodge', stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = Average_Mill), size = 3) +
  theme_bw() +
  labs(
    y = '2020 Salary (Millions $)',
    x = 'Player',
    title = '2020 Chicago Blackhawks Player Salary')
  
  
  
mean_salary(chi_salary(goalie2020))
  

  