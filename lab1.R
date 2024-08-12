

library(statsr)
library(dplyr)
library(ggplot2)


data(mlb11)


ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point()

mlb11 %>%
  summarise(cor(runs, at_bats))

# The first argument in the function lm is a formula that takes the form y ~ x. 
# Here it can be read that we want to make a linear model of runs as a function of at_bats. 
# The second argument specifies that R should look in the mlb11 data frame to find the runs and at_bats variables.

m1 = lm(runs ~ at_bats, data = mlb11)

m2 = lm(runs ~ homeruns, data = mlb11)


ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)


mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)



# Note, doing a residuals where the x axis is X or y_hat tells 
# the same thing, we should get zero mean random noise scatter 
# in both cases

ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")


ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m1, aes(sample = .resid)) +
  stat_qq()


summary(lm(runs ~ at_bats, data = mlb11))$r.squared
summary(lm(runs ~ hits, data = mlb11))$r.squared
summary(lm(runs ~ wins, data = mlb11))$r.squared
summary(lm(runs ~ bat_avg, data = mlb11))$r.squared


m4 <- lm(runs ~ new_obs, data = mlb11)
plot(mlb11$runs ~ mlb11$new_obs)
abline(m4)
summary(m4)$r.squared
plot(m4$residuals ~ mlb11$hits)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0



m4 <- lm(runs ~ new_obs, data = mlb11)
plot(mlb11$runs ~ mlb11$new_obs)
abline(m4)
summary(m4)$r.squared
plot(m4$residuals ~ mlb11$hits)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

m5 <- lm(runs ~ new_slug, data = mlb11)
plot(mlb11$runs ~ mlb11$new_slug)
abline(m5)
summary(m5)$r.squared
plot(m5$residuals ~ mlb11$hits)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

m6 <- lm(runs ~ new_onbase, data = mlb11)
plot(mlb11$runs~ mlb11$new_onbase)
abline(m6)
summary(m6)$r.squared
plot(m6$residuals ~ mlb11$hits)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

