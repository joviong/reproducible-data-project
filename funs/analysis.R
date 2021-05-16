# Do teams with a higher ORtg win more games??

# plotting the relationship between offensive efficiency ratings and wins
lm_plot <- team_final %>%
  ggplot(aes(x = ORtg, y = W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red")


## the plot shows a strong relationship with teams winning more games when their offensive efficiency rating is higher

# plotting the relationship between defensive efficiency ratings and wins (just checking DO NOT include)
gg <- team_final %>%
  ggplot(aes(x = DRtg, y = W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") +
  geom_text(aes(label = Team), nudge_y = 1, nudge_x = -2)
## the plot shows a weak relationship with teams winning games based on their defensive efficiency rating

# plotting the relationship between nett efficiency ratings and wins (just checking DO NOT include)
lm_plot_nett <- team_final %>%
  ggplot(aes(x = NRtg, y = W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") 
## the plot shows a strong relationship with teams winning games based and the nett efficiency rating

# Pearsons' correlation coefficient for ORtg against Wins
cor(x = team_final$ORtg, y = team_final$W, method = "pearson")
## a correlation value of 0.841 shows that there is in fact a strong relationship between ORtg (offensive efficiency rating) and winning games

# Pearsons' correlation coefficient for DRtg against Wins (just checking DO NOT include)
cor(x = team_stats_final$DRtg, y = team_stats_final$W, method = "pearson")
## a correlation value of -0.760 shows that not only is there a weak relationship but DRtg probably does not affect outcome of games much

# Pearsons' correlation coefficient for NRtg against Wins (just checking DO NOT include)
cor(x = team_stats_final$NRtg, y = team_stats_final$W, method = "pearson")
## a correlation value of 0.980 shows that there is in fact a very strong relationship between NRtg (nett efficiency rating) and winning games

# running linear regression to obtain least squares estimate of ORtg
fit <- lm(ORtg ~ W, data = team_final)
summary(fit)


## intercept is the value of wins per game if ORtg = 0. ORtg increases by 1 chances of winning is by 3.3636
## multiple r-squared value of 0.7075 shows a variance of 70.75% of Wins is determined by ORtg

library(broom)

#running a tidy output of our previous model
tidy(fit, conf.int = TRUE)


## based on the output, we have 95% confidence that the ORtg is between 2.53 and 4.20 (1:09 of Linear lecture)

# Looking to predict the data on our regression line
exp_w <- predict(fit)
head(exp_w)

head(predict(fit))

# Detecting outliers in the game
std_res <- rstandard(fit)
points <- 1:length(std_res)


## Plotting the point of the outliers
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")


# Labelling the outlier points if the absolute value of a std_res is greater than 1 std_res
res_labels <- if_else(abs(std_res) >= 1.5, paste(points), "")

## Plotting a graph with labels of the outliers
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  geom_text(aes(label = res_labels), nudge_y = 0.15) +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")


## plotting it against original linear model graph of ORtg to W
lm_plot +
  geom_text(aes(label = res_labels), nudge_x = 0.01)


# Looking at hat values/diagonals in order to determine the leverage measurement which indicates it's potential
hats <- hatvalues(fit)

# Now we plot the hat values against the points
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

# Looking at hat values that are above the outliers
hat_labels <- if_else(hats >=0.075, paste(points), "")
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.004)

lm_plot +
  geom_text(aes(label = hat_labels), nudge_x = 0.004)

# Examining the influence when an ith data point is removed in order to fit the model using `cooks.distance()`
cook <- cooks.distance(fit)
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

## Looking into points that are away from the rest of the data. 
cook_labels <- if_else(cook >= 0.075, paste(points), "")
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.003)

lm_plot +
  geom_text(aes(label = cook_labels), nudge_x = 0.2)

# Testing for the independence of observations
car::durbinWatsonTest(fit)
## A D-W Statistical value of 1.08 shows that there is a positive auto-correlation of residuals where there is no correlation of residuals

# Assessing homoscedasticity for the values
res <- residuals(fit)
fitted <- predict(fit)

# Plotting values on the graph to determine homoscedasticity
ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "green4") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
## Randomness in the pattern within the plotted graph tells us that there is in fact homoscedasticity within the 
ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, colour = "magenta")

ggplot(data = NULL, aes(sample = res)) +
  stat_qq() + stat_qq_line()

ggplot(data = NULL, aes(x = res)) +
  geom_histogram(colour = "black", fill = "dodgerblue")
## WHAT THE FUCK HAPPENED???

## ???Require a team that can achieve a NTrg of 1 to be able to hopefully make it into the off-season playoffs


# examining the relationship between rebounds and win
team_stats_final <- mutate(team_stats_final, RBp = ORBp + DRBp / 2)

gg <- team_stats_final %>%
  ggplot(aes(RBp, W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") +
  geom_text(aes(label = Team))

## an unclear relationship can be seen between Rebound percentage and effective Field Goals %

# examining the relationship between Free Throw attempt rate and W
gg <- team_stats_final %>%
  ggplot(aes(FTr, W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") +
  geom_text(aes(label = Team))


## an unclear relationship can be seen between Free Throw attempt rate and effective Field Goals %

# examining the relationship between Turnover % and W
gg <- team_stats_final %>%
  ggplot(aes(TOVp, W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") +
  geom_text(aes(label = Team))


## an unclear relationship can be seen between Turnover percentage and effective Field Goals %

# examining the relationship between eFGp % and W
team_stats_final %>%
  ggplot(aes(eFGp, W)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") +
  geom_text(aes(label = Team))


# player statisctical analysis (transfered)
team_stats_final <- team_stats_final %>%
  mutate(possession = FGA + 0.5 * FTA - ORB + TOV -4) %>%
  summarise(Team, FG, FGA, FGp, possession)

ggplot(team_possession, aes(x = possession, y = FG)) +
  geom_point(colour = "green4") +
  geom_smooth(se = FALSE, method = "lm", colour = "red") +
  geom_text(aes(label = Team), nudge_y = 1, nudge_x = -2)

