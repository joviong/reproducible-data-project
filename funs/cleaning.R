library(tidyverse)

# reading in the data for each spreadsheet
file_names <- list.files("data/raw")

for (i in seq_along(file_names)) {
  
  players <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
  team_1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
  team_2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")
  salary <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
  team_salary <- read_csv("data/raw/2019-20_nba_team-payroll.csv")
}

# checking for structure
objects <- list(players, team_1, team_2, salary, team_salary)

for (i in seq_along(objects)) {
  str(players)
  str(team_1)
  str(team_2)
  str(salary)
  str(team_salary)
}

heads <- list(players, team_1, team_2, salary, team_salary)

for (i in seq_along(heads)) {
  head(team_1)
  head(team_2)
  head(team_salary)
  head(players)
  head(salary)
}

tails <- list(players, team_1, team_2, salary, team_salary)

for (i in seq_along(heads)) {
  tail(team_1)
  tail(team_2)
  tail(team_salary)
  tail(players)
  tail(salary)
}
# checking for na values within the datasets
sum(is.na(players))
sum(is.na(team_1))
sum(is.na(team_2))
sum(is.na(salary))
sum(is.na(team_salary))

# discovering which line of data the na values are located in
which(is.na(players), arr.ind = TRUE)
which(is.na(team_1), arr.ind = TRUE)

# cleaning up the object names to conform with tidy data
players <- rename(players, player = `player_name`, FGp = `FG%`, x3P = `3P`, x3PA = `3PA`, x3Pp = `3P%`, x2P = `2P`, x2PA = `2PA`, x2Pp = `2P%`, eFGp = `eFG%`, FTp = `FT%`)
team_1 <- rename(team_1, rank = `Rk`, x3PAr = `3PAr`, TSp = `TS%`, eFGp = `eFG%`, TOVp = `TOV%`, ORBp = `ORB%`, DRBp = `DRB%`)
team_2 <- rename(team_2, rank = `Rk`, FGp = `FG%`, x3P = `3P`, x3PA = `3PA`, x3Pp = `3P%`, x2P = `2P`, x2PA = `2PA`, x2Pp = `2P%`, FTp = `FT%`)
salary <- rename(salary, player = `player_name`)

#sorting Chicago Team players only
chi_team <- players %>%
  group_by(player) %>%
  filter(Tm == "CHI")

# sorting duplicates for traded players rows
players <- filter(players, Tm != "TOT")

players <- players %>%
  group_by(player, Pos) %>%
  summarise(Age = mean(Age), G = sum(G), GS = sum(GS), MP = sum(MP), FG = sum(FG), FGA = sum(FGA), FGp = round((FG/FGA), 3), x3P = sum(x3P), x3PA = sum(x3PA), x3Pp = round((x3P/x3PA), 3), x2P = sum(x2P), x2PA = sum(x2PA), x2Pp = round((x2P/x2PA), 3), eFGp = (round(((FG + .5 * x3P)/FGA), 3)), FT = sum(FT), FTA = sum(FTA), FTp = round((FT/FTA), 3), ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB), AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TOV = sum(TOV), PF = sum(PF), PTS = sum(PTS), .groups = 'keep') %>%
  ungroup()

# eliminating unwanted data that is irrelevant to the stats
team_1 <- select(team_1, -c(X23, X24, X25))

# Joining both team_stats_.. sheet to each other and running exploratory analysis based on that
team_final <- full_join(team_1, team_2, by = c("Team"))
team_final <- select(team_final, -c(rank.x, rank.y))

# Creating a PTS_per_game variable to allow for analysis
team_final <- mutate(team_final, PTS_per_game = PTS / G)


# creating a z score for FG to compare player efficiency in scoring
players <- mutate(df,FG_z = (FG - mean(FG)) / sd(FG),
             FG_category = if_else(condition = FG_z <0, 
                                   true = "below average", false = "above average"))

# creating new variable in players to determine points score (seeking for starters)
players <- mutate(players, PTS_per_min = PTS / MP)
         
players %>%
  summarise(mn = mean(PTS_per_min, na.rm = TRUE),
              sd = sd(PTS_per_min, na.rm = TRUE),
              count = n())

players <- players %>%
  mutate(PPM_category = if_else(condition = PTS_per_min < 0.4,
                                true = "below average", false = "above average"))

# seeking out players with above average rebound rates
players <- players %>%
  mutate(RB_per_game = TRB / G)

players %>%
  summarise(mn = mean(RB_per_game, na.rm = TRUE),
            sd = sd(RB_per_game, na.rm = TRUE),
            count = n())

players <- players %>%
  mutate(RB_category = if_else(condition = RB_per_game < 3.61, 
                               true = "below average", false = "above average"))

# seeking out players with above average TOV rates
players <- players %>%
  mutate(TOV_per_game = TOV / G)

players %>%
  summarise(mn = mean(TOV_per_game, na.rm = TRUE),
            sd = sd(TOV_per_game, na.rm = TRUE),
            count = n())

players <- players %>%
  mutate(TOV_category = if_else(condition = TOV_per_game < 1.06,
                                true = "below average", false = "above average"))

# seeking out players with above average AST rates
players <- players %>%
  mutate(AST_per_game = AST / G)

players %>%
  summarise(mn = mean(AST_per_game, na.rm = TRUE),
            sd = sd(AST_per_game, na.rm = TRUE),
            count = n())

players <- players %>%
  mutate(AST_category = if_else(condition = AST_per_game < 2,
                                true = "below average", false = "above average"))

# creating more per_game box score statistics for players
players <- players %>%
  mutate(FG_per_game = FG / G,
         x3P_per_game = x3P / G,
         x2P_per_game = x2P / G,
         FT_per_game = FT / G,
         PTS_per_game = PTS / G)

# creating new object to find player shooting statistics only
players_shooting <- select(df, player, Pos, FG:FTp, PTS:FG_category)

# Creating new object by player position to select team
## PG
players_PG <- filter(players, Pos == "PG")
players_PG
## SG
players_SG <- filter(players, Pos == "SG")
players_SG
## SF
players_SF <- filter(players, Pos == "SF")
players_SF
## PF
players_PF <- filter(players, Pos == "PF")
players_PF
## C
players_C <- filter(players, Pos == "C")
players_C



# Searching for mean of each shooting variable to filter players in the upper quartile
players_pos <- players %>%
  group_by(Pos, FG_category) %>%
  summarise(mn_2Pp = mean(x2Pp, na.rm = TRUE),
            sd_2Pp = sd(x2Pp, na.rm = TRUE),
            mn_3Pp = mean(x3Pp, na.rm = TRUE), 
            sd_3Pp = sd(x3Pp, na.rm = TRUE),
            mn_FTp = mean(FTp, na.rm = TRUE),
            sd_FTp = sd(FTp, na.rm = TRUE),
            count = n())
players_pos

# creating new variable in team_final for  statistical analysis
team_final <- team_final %>%
  mutate(possession = FGA + 0.5 * FTA - ORB + TOV -4)


# utilising the equation for points per minute scored based on Kubatko et al. considering reserve players who played fewer minutes. Seeking for players who ay have the opportunity to play more minutes as a breakout (reserve bench player metric)
players <- mutate(players, PTS_per_min40 = PTS / MP * 40)

# creating mFG to use in player efficiency code
player_stats <- player_stats %>%
  mutate(player_stats, mFG = FGA - FG) %>%
  mutate(player_stats, mFT = FTA - FT)

# linear weights to evaluate player class as used by NBA.com
player_stats <- mutate(player_stats, eff_rate = (PTS + ORB + DRB + AST + STL + BLK) - (TOV + mFG + mFT))

# setting Total Score % for the player level according to Kubatko et al. 
player_stats <- mutate(player_stats, TSp = (PTS/2) / (FGA + 0.44 * FTA))

chi_team <- players %>%
  group_by(player) %>%
  select(Tm == CHI)

#Lesson codes:

df <- mutate(df,
             FG_z = (FG - mean(FG)) / sd(FG),
             FG_category = if_else(condition = FG_z < 0,
                                   true = "below average", false = "above average"))

select(player_stats, player, Pos, PTS)
select(player_stats, player, FG:eFGp)

player_shooting <- select(player_stats, player, FG:eFGp)

players %>%
  filter(FG_z >= 0.2)

filter(players, FG >= mean(FG))
filter(players, Pos == "PG" & x3Pp > 0.38)
filter(players, Pos == "PG" | Pos == "C")
filter(players, between(MP, 1000, 2000))
