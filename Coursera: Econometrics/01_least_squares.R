#### Introduction ####

x <- c(23, 15, 46, NA)
z <- c(5, 6, NA, 8)

mean(x)
mean(x, na.rm = TRUE)
mean(z, na.rm = TRUE)

sum(x)
sum(x, na.rm = TRUE)

d <- data.frame(rost = x, ves = z)
d

d[4, 1]
d[3, 1]
d[2, ]
d[, 2]

d$rost
d$ves

my_list <- list(a = 7, b = 10:20, table = d)
my_list$a
my_list$b
my_list$table

my_list[[1]]
my_list[[2]]

#### Data in R ####

library(tidyverse)
library(GGally)
library(psych)

?predict
help(describe)

d <- cars
glimpse(d)
head(d)
tail(d)
describe(d)
ncol(d)
nrow(d)
str(d)

mean(d$speed)

d2 <- mutate(d, speed = 1.67*speed, dist = 0.3*dist, ratio = dist/speed)
glimpse(d2)

?cars
qplot(data = d2, dist, xlab = 'Stopping distance (m)', ylab = 'Car quantity',
      main = '1920s data')
qplot(data = d2, speed, dist)

#### Least squares: example 1 ####

model <- lm(data = d2, dist ~ speed)
model

beta_hat <- coef(model)
beta_hat

eps_hat <- residuals(model)
eps_hat

y <- d2$dist
y_hat <- fitted(model)
y
y_hat

RSS <- deviance(model)
RSS

TSS <- sum( (y - mean(y))^2 )
TSS

ESS <- TSS - RSS

R2 <- ESS / TSS
R2

cor(y, y_hat)^2

x <- model.matrix(model)
x

nd <- data.frame(speed = c(40, 60))
nd

predict(model, nd)

qplot(data=d2, speed, dist) + stat_smooth(method = "lm")

#### Least squares: example 2 ####

t <- swiss
?swiss
glimpse(t)
describe(t)

ggpairs(t)

model2 <- lm(data = t, Fertility ~ Agriculture + Education + Catholic)
coef(model2)
fitted(model2)
residuals(model2)
deviance(model2)

report <- summary(model2)
report

cor(t$Fertility, fitted(model2))^2
nd2 <- data.frame(Agriculture = 0.5, Catholic = 0.5, Education = 20)
predict(model2, nd2)

#### Test ####

data(sleep)
sleep

sleep[6, 1]
mean(sleep$extra)^2
min(sleep$extra) * max(sleep$extra)
var(sleep$extra[10:20])

data("mtcars")
glimpse(mtcars)
model3 <- lm(data = mtcars, mpg ~ disp + hp + wt + am)
summary(model3)
coef(model3)

models = list(
  model1 = mpg ~ disp + hp + wt,
  model2 = mpg ~ cyl + hp + wt,
  model3 = mpg ~ disp + cyl + wt,
  model4 = mpg ~ disp + hp + cyl
)

sapply( models, function(f) summary( lm(data = mtcars, f) )$r.squared )