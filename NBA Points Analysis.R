# Loadlibrary
library(ggplot2)

# Load dataset
attach(Player.Totals)


# Filter to include only NBA players from the 2022-2023 season
nba_2023 <- subset(Player.Totals, season == "2023")

# Group players into three categories: guards, forwards, and centers
nba_2023$PositionGroup <- ifelse(nba_2023$Position %in% c("PG", "SG"), "Guard",
                                 ifelse(nba_2023$Position %in% c("SF", "PF"), "Forward", "Center"))

# Multiple linear regression model with field goals, free throws, minutes played, and position group
model <- lm(pts ~ fg + ft + mp + pos, data = Player.Totals)

# Summary of the model
summary(model)

# Create scatter plots for each predictor
ggplot(nba_2023, aes(x = fg, y = pts)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + ggtitle("Field Goals vs Total Points")
ggplot(nba_2023, aes(x = ft, y = pts)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + ggtitle("Free Throws vs Total Points")
ggplot(nba_2023, aes(x = mp, y = pts)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + ggtitle("Minutes Played vs Total Points")

# Residuals plot for the linear model
plot(model$residuals)

