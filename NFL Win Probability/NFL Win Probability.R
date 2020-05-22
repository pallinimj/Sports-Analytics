

## Clear Workspace

rm(list=ls())

#devtools::install_github(repo = "maksimhorowitz/nflscrapR", force = TRUE)


library(nflscrapR)
library(tidyverse)
library(teamcolors)
library(ggplot2)


## =============================================================
## Pull all data
## =============================================================

# pull play by play records from 2015-2019 seasons
# looking at regular season only
# nflscrapR package has been having issues, so pulling the CSV files directly from Github

pbp_2015 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2015.csv")
pbp_2016 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv")
pbp_2017 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")
pbp_2018 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
pbp_2019 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")

# combine all into one table
pbp_data <- do.call('rbind', list(pbp_2015, pbp_2016, pbp_2017, pbp_2018, pbp_2019))

# save play by play data to reload quicker
write.csv(pbp_data, 'pbp_data.csv', row.names = FALSE)

#pbp_data <- read.csv('pbp_data.csv', stringsAsFactors = FALSE)

# pull individual game info from same source

games2015 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2015.csv", stringsAsFactors = FALSE)
games2016 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2016.csv", stringsAsFactors = FALSE)
games2017 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2017.csv", stringsAsFactors = FALSE)
games2018 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2018.csv", stringsAsFactors = FALSE)
games2019 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2019.csv", stringsAsFactors = FALSE)

# combine all into one table
games <- do.call('rbind', list(games2015, games2016, games2017, games2018, games2019))
games <- subset(games, select = c('game_id','season','week','home_score','away_score','home_team','away_team'))
games$winner <- ifelse(games$home_score > games$away_score, games$home_team, games$away_team)
games <- subset(games, select = c('game_id','season','week','winner'))

# save games info to reload quicker
write.csv(games, 'games.csv', row.names = FALSE)

#games <- read.csv('games.csv', stringsAsFactors = FALSE)

## =============================================================
## Build Win Probability Model
## =============================================================

# create single dataframe for win probability
# add fields for final result of the game

game_data <- merge(games, pbp_data, by = 'game_id')

game_data$poswin <- ifelse(game_data$winner == game_data$posteam, 'Y', 'N')
game_data$qtr <- as.factor(game_data$qtr)
game_data$down <- as.factor(game_data$down)
game_data$poswin <- as.factor(game_data$poswin)
game_data$play_type <- as.factor(game_data$play_type)

# clean up records with no data
game_data <- game_data %>% filter(play_type != 'no_play' & down != 'NA')

# include only the fields that I want to include in the model

game_data_model <- game_data[c('game_id','posteam','home_team','away_team','poswin','yardline_100','qtr','down','ydstogo','play_type',
                               'posteam_timeouts_remaining','defteam_timeouts_remaining','score_differential','game_seconds_remaining')]


# build model
# using generalized linear model with binomial to make it a logistic regression

win_prob <- glm(poswin ~ qtr + down + ydstogo + yardline_100 + game_seconds_remaining + score_differential,
                     data = game_data_model, family = binomial, maxit=100)

summary(win_prob)

# make predictions on the model
pred <- predict(win_prob, game_data_model, type = 'response')

win_prob_results <- cbind(game_data_model, pred)
win_prob_results$homepred <- ifelse(win_prob_results$home_team == win_prob_results$posteam, win_prob_results$pred, 1-win_prob_results$pred)
win_prob_results$awaypred <- ifelse(win_prob_results$away_team == win_prob_results$posteam, win_prob_results$pred, 1-win_prob_results$pred)


## =============================================================
## Analyze a game from 2019
## =============================================================

# looking at Browns vs Rams from SNF

subset(games2019, home_team == 'CLE' & away_team == 'LA', select = c('game_id'))
  
cle_vs_la <- subset(win_prob_results, game_id == 2019092213)

# build colors

nfl_colors <- teamcolors %>% filter(league == 'nfl')

cle_color <- nfl_colors %>% filter(name == 'Cleveland Browns') %>% pull(primary)
lar_color <- nfl_colors %>% filter(name == 'Los Angeles Rams') %>% pull(primary)

cle_vs_la %>%
  filter(!is.na(homepred),
         !is.na(awaypred)) %>%
  dplyr::select(game_seconds_remaining,
                homepred,
                awaypred) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("LA", "CLE"),
                     values = c(lar_color, cle_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = "LA", color = lar_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = "CLE", color = cle_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = "2019 Week 3 Win Probability Chart",
    subtitle = "Los Angeles Rams at Cleveland Browns"
  ) + theme_bw()

  
## add play descriptions to game data
play_desc <- subset(pbp_data, game_id == 2019092213, select = c('game_seconds_remaining','desc'))
cle_vs_la <- merge(cle_vs_la, play_desc, by = 'game_seconds_remaining')

## drive 1 - end of 1st half
drive_1 <- subset(cle_vs_la, game_seconds_remaining >= 1800 & game_seconds_remaining <= 1828,
                  select = c('posteam','score_differential','game_seconds_remaining','qtr','down','ydstogo','yardline_100'
                             ,'play_type','homepred','desc'))

## drive 2 - late 3rd quarter - Goff INT
drive_2 <- subset(cle_vs_la, game_seconds_remaining >= 1020 & game_seconds_remaining <= 1118,
                  select = c('posteam','score_differential','game_seconds_remaining','qtr','down','ydstogo','yardline_100'
                             ,'play_type','homepred','desc'))

## drive 3 - late 4th quarter - Final CLE drive
drive_3 <- subset(cle_vs_la, game_seconds_remaining >= 33 & game_seconds_remaining <= 174,
                  select = c('posteam','score_differential','game_seconds_remaining','qtr','down','ydstogo','yardline_100'
                             ,'play_type','homepred','desc'))


