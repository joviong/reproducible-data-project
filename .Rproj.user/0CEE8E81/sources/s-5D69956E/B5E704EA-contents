# plotting the relationship between net efficiency ratings and wins

gg_1 <- ggplot(team_stats_1, aes(x = NRtg, y = W)) +
  geom_point()

## the plot shows a strong relationship with teams winning more games when their net efficiency rating is higher

# examining the relationship between rebounds and eFG%
team_stats_1 <- mutate(team_stats_1, RBp = ORBp + DRBp / 2)

gg_2 <- ggplot(team_stats_1, aes(RBp, eFGp)) +
  geom_point()
gg_2

## an unclear relationship can be seen between Rebound percentage and effective Field Goals %

# examining the relationship between Free Throw attempt rate and eFTG%
gg_3 <- ggplot(team_stats_1, aes(FTr, eFGp)) +
  geom_point()
gg_3

## an unclear relationship can be seen between Free Throw attempt rate and effective Field Goals %

# examining the relationship between Turnover % and eFTG%
gg_4 <- ggplot(team_stats_1, aes(TOVp, eFGp)) +
  geom_point()
gg_4

## an unclear relationship can be seen between Turnover percentage and effective Field Goals %