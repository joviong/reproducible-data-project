library(tidyverse)

# reading in the data for each spreadsheet
player_stats <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
team_stats_1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
team_stats_2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")
salary <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
team_salary <- read_csv("data/raw/2019-20_nba_team-payroll.csv")

# checking for na values within the datasets
sum(is.na(player_stats))
sum(is.na(team_stats_1))
sum(is.na(team_stats_2))
sum(is.na(salary))
sum(is.na(team_salary))

# discovering which line of data the na values are located in
which(is.na(player_stats), arr.ind = TRUE)
which(is.na(team_stats_1), arr.ind = TRUE)

# cleaning up the object names to conform with tidy data
player_stats <- rename(player_stats, player = `player_name`, FGp = `FG%`, x3P = `3P`, x3PA = `3PA`, x3Pp = `3P%`, x2P = `2P`, x2PA = `2PA`, x2Pp = `2P%`, eFGp = `eFG%`, FTp = `FT%`)
team_stats_1 <- rename(team_stats_1, rank = `Rk`, x3PAr = `3PAr`, TSp = `TS%`, eFGp = `eFG%`, TOVp = `TOV%`, ORBp = `ORB%`, DRBp = `DRB%`)
team_stats_2 <- rename(team_stats_2, rank = `Rk`, FGp = `FG%`, x3P = `3P`, x3PA = `3PA`, x3Pp = `3P%`, x2P = `2P`, x2PA = `2PA`, x2Pp = `2P%`, FTp = `FT%`)

# eliminating data that does not hold value within the process
team_stats_1 <- select(team_stats_1, -c(X23, X24, X25))

# creating new variable in player_stats to determine points score (seeking for starters)
player_stats <- mutate(player_stats, PTS_per_min = PTS / MP)

# utilising the equation for points per minute scored based on Kubatko et al. considering reserve players who played fewer minutes. Seeking for players who ay have the opportunity to play more minutes as a breakout (reserve bench player metric)
player_stats <- mutate(player_stats, PTS_per_min40 = PTS / MP * 40)

# creating mFG to use in player efficiency code
player_stats <- player_stats %>%
  mutate(player_stats, mFG = FGA - FG) %>%
  mutate(player_stats, mFT = FTA - FT)

# linear weights to evaluate player class as used by NBA.com
player_stats <- mutate(player_stats, eff_rate = (PTS + ORB + DRB + AST + STL + BLK) - (TOV + mFG + mFT))

# setting Total Score % for the player level according to Kubatko et al. 
player_stats <- mutate(player_stats, TSp = (PTS/2) / (FGA + 0.44 * FTA))

# sorting duplicates for traded players rows
df <- filter(player_stats, Tm != "TOT")

df <- df %>%
  group_by(player) %>%
  summarise(Pos = keep, Age = mean(Age), G = sum(G), GS = sum(GS), MP = sum(MP), FG = sum(FG), FGA = sum(FGA), FGp = round((FG/FGA), 3), x3P = sum(x3P), x3PA = sum(x3PA), x3Pp = round((x3P/x3PA), 3), x2P = sum(x2P), x2PA = sum(x2PA), x2Pp = round((x2P/x2PA), 3), eFGp = (round(((FG + .5 * x3P)/FGA), 3)), FT = sum(FT), FTA = sum(FTA), FTp = round((FT/FTA), 3), ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB), AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TOV = sum(TOV), PF = sum(PF), PTS = sum(PTS)) %>%
  ungroup()

#Lesson codes:

player_stats <- mutate(player_stats,
                       FG_z = (FG - mean(FG)) / sd(FG),
                       FG_category = if_else(condition = FG_z < 0,
                                             true = "below average", false = "above average"))

select(player_stats, player, Pos, PTS)
select(player_stats, player, FG:eFGp)

player_shooting <- select(player_stats, player, FG:eFGp)