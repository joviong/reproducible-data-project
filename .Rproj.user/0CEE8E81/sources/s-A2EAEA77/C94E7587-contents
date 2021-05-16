# Do teams with more FG per game score more PTS?

# After reading in data, we want to look at creating new variables for the players in the form of shooting metrics that apply to points scoring
dat <- players 

# We then plot the graph to see if there is a relationship
lm_plot <- ggplot(players, aes(x = FG_per_game, y = PTS_per_game)) + 
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm", se = FALSE)
lm_plot

# Do players with more RB_per_game score more points per game
ggplot(dat, aes(x = RB_per_game, y = PTS_per_game)) + 
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm", se = FALSE)

# Do players with more TOV_per_game score more points per game
ggplot(dat, aes(x = TOV_per_game, y = PTS_per_game)) + 
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm", se = FALSE)

# Do players with more FT_per_game score more points per game
ggplot(dat, aes(x = FT_per_game, y = PTS_per_game)) + 
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm", se = FALSE)

## Evidence that FT_per_game and FG_per_game relates to more PTS_per_game by using a multiple linear regression to model the relationship
fit <- lm(PTS_per_game ~ FG_per_game + FT_per_game + RB_per_game + TOV_per_game, data = dat)
tidy(fit, conf.int = TRUE)
summary(fit)

## The slope coefficient tells us that players who score 1 more FG_per_game than the average player scores 2.29 more PTS_per_game
## The slope coefficient also tells us that players who score 1 more FT_per_game than the average player scores 0.87 more PTS_per_game 
## To score more points we get more effective FG scorers, but they can be expensive. So let's look at getting players who are able to generate more off the board


# Checking the relationship for multicollinearity using the pairs plot between each scoring factor against points scored
pairs(formula = ~ FG_per_game + FT_per_game + RB_per_game + TOV_per_game, data = players)


# Detecting outliers in the game
std_res <- rstandard(fit)
points <- 1:length(std_res)


## Plotting the point of the outliers
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")


# Labelling the outlier points if the absolute value of a std_res is greater than 1 std_res
res_labels <- if_else(abs(std_res) >= 2, paste(points), "")

## Plotting a graph with labels of the outliers
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  geom_text(aes(label = res_labels), nudge_y = 0.2, nudge_x = 0.5) +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")


## plotting it against original linear model graph of FG_per_game to PTS_per_game
lm_plot +
  geom_text(aes(label = res_labels), nudge_x = 0.01)


# Looking at hat values/diagonals in order to determine the leverage measurement which indicates it's potential
hats <- hatvalues(fit)

# Now we plot the hat values against the points
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

# Looking at hat values that are above the outliers
hat_labels <- if_else(hats >=0.05, paste(points), "")
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.003)

lm_plot +
  geom_text(aes(label = hat_labels), nudge_x = -0.2)

# Examining the influence when an ith data point is removed in order to fit the model using `cooks.distance()`
cook <- cooks.distance(fit)
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

## Looking into points that are away from the rest of the data. 
cook_labels <- if_else(cook >= 0.1, paste(points), "")
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.02)

lm_plot +
  geom_text(aes(label = cook_labels), nudge_x = 0.2)

# Testing for the independence of observations
car::durbinWatsonTest(fit)


# Assessing homoscedasticity for the values
res <- residuals(fit)
fitted <- predict(fit)

# Plotting values on the graph to determine homoscedasticity
ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "green4") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
## Randomness in the pattern within the plotted graph tells us that there is in fact homoscedasticity within the data

# Testing for normally distributed figures
ggplot(data = NULL, aes(x = res)) +
  geom_histogram(colour = "darkblue", fill = "green4")

# Running test for linearity using the avPlots from `car` package
car::avPlots(fit)

# Looking at multicollinearity using the pairs plot or using the variance inflation factor from the `car` package, `car::vif()`:
pairs(formula = ~ FG_per_game + x2P_per_game + x3P_per_game + FT_per_game, data = dat)

# by using the VIF, we can explore how much larger the standard error increases compared to if that variable had 0 correlation to other predictors
car::vif(fit)
## the values show that the standard error of the coefficient for 1.71 times larger than if this variable had a 0 correlation with other predictors
## With a recommended cut-off point set at 5, this shows that the variables have independence in multicollinearity

## Create Player metric by first looking at a team metric that can normalise it for all players regardless of whether they make it or not. 
#(Number of shots per game)
nPT_per_game <- dat %>%
  group_by(Team) %>%
  summarise(nPT_per_game = sum(x2P + x3P + FT)/max(G)) %>%
  pull(nPT_per_game)%>%
  mean

nPT_per_game #(58.76 is number of points per game on average that a team scores)#

#(Points per minute per game)
PTS40_per_game <- dat %>%
  group_by(Team) %>%
  summarise(PTS_40 = ((PTS/MP) * 40) / G) %>%
  pull(PTS_40) %>%
  mean

PTS40_per_game

# Creating a new player metric which is number of scored buckets 
players <- players %>%
  group_by(player) %>%
  mutate(nPT = x2P + x3P + FT)

players <- players %>%
  group_by(player) %>%
  mutate(PTS_40 = ((PTS / MP) * 40) / G)

players <- players %>%
  mutate(x2P_per_game = x2P / G,
            x3P_per_game = x3P / G,
            FT_per_game = FT / G) %>%
  select(-G) %>%
  ungroup()

players <- mutate(players, exp_PTS = predict(fit, newdata = players))

players <- salary %>%
  select(player, salary) %>%
  right_join(players, by = "player")

#selecting guards based on AST_category, TOV_category, x3P_per_game and exp_PTS
players_PG %>%
  select(player, salary, AST_category, TOV_category, x3P_per_game, exp_PTS) %>%
  arrange(desc(exp_PTS), salary) %>%
  top_n(15)

players_SG %>%
  select(player, salary, AST_category, TOV_category, x3P_per_game, exp_PTS) %>%
  arrange(desc(exp_PTS), salary) %>%
  top_n(15)

players_SF %>%
  select(player, salary, AST_category, TOV_category, x3P_per_game, exp_PTS) %>%
  arrange(desc(exp_PTS), salary) %>%
  top_n(15)

# Selecting BIGs based on RB_category, FT_per_game, exp_PTS and FG_category
players_PF %>%
  select(player, salary, RB_category, TOV_category, FT_per_game, exp_PTS) %>%
  arrange(desc(exp_PTS), salary) %>%
  top_n(15)

players_C %>%
  select(player, salary, RB_category, TOV_category, FT_per_game, exp_PTS) %>%
  arrange(desc(exp_PTS), salary) %>%
  top_n(15)

players %>%
  ggplot(aes(x = salary/1000000, y = exp_PTS, colour = Pos)) +
  geom_point() +
  xlab("Salary (Millions)")
