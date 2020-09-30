library(tidyverse)
library(memisc)
library(psych)
library(lmtest)
library(sjPlot)
library(sgof)
library(foreign)
library(car)
library(hexbin)
library(sjmisc)

#### Random variables ####

# z_1, ..., z_100 ~ N(5, 9)
z <- rnorm(100, mean = 5, sd = 3)
z[56]
z[2:9]
qplot(z)

# density function
x <- seq(-10, 15, by = 0.5)
y <- dnorm(x, mean = 5, sd = 3)
qplot(x, y)
qplot(x, y, geom = "line")

# P(Z < 3) = F(3)
pnorm(3, mean = 5, sd = 3)

# P(Z \in [4;9])
pnorm(9, mean = 5, sd = 3) - pnorm(4, mean = 5, sd = 3)

# P (Z < a) = 0.7 a?
qnorm(0.7, mean = 5, sd = 3)

#### Multiple regression, coefficient hypotheses #### 

h <- swiss
glimpse(h)
help("swiss")

model <- lm(data = h, Fertility ~ Catholic + Agriculture + Examination)
summary(model)

coeftest(model)
confint(model)
plot_model(model, show.values = TRUE)

# hypothesis about b_Cath = b_Agri
model_aux <- lm(data = h, 
                Fertility ~ Catholic + I(Catholic + Agriculture) + Examination)
summary(model_aux)
linearHypothesis(model, 'Catholic - Agriculture = 0')

#### Scaled coefficients and false significant predictors ####

# scale
h_st <- mutate_each(h, 'scale')
glimpse(h_st)
model_st <- lm(data = h_st,
               Fertility ~ Catholic + Agriculture + Examination)
summary(model_st)
plot_model(model_st, show.values = TRUE)

# artificial data
D <- matrix(rnorm(100 * 41), nrow = 100)
df <- data.frame(D)
glimpse(df)
model_empty <- lm(data = df, X1 ~ .)
summary(model_empty)

# model comparison
model2 <- lm(data = h, Fertility ~ Catholic + Agriculture)
comparison_12 <- mtable(model, model2)
comparison_12

#### Data saving and loading ####

# saving
stuff <- list(data = h, model = model2)
saveRDS(stuff, file = 'mydata.RDS')

# loading
mylist <- readRDS('mydata.RDS')
summary(mylist$model)

# csv
?read.csv

#### RLMS ####

library(rlms)
h <- read.rlms()

#### Quiz ####

pt(30 / 5, df = 50) > 0.995
tcr <- qt(0.95, df = 27)
5 - tcr * 1

pnorm(9, mean = 7, sd = 2)

data <- diamonds
mean(data$price)

table(data$cut)
model <- lm(data = data, price ~ 0 + carat + clarity)
summary(model)

model <- lm(data = data, price ~ carat + table)
summary(model)

model <- lm(data = data, price ~ carat)
summary(model)

model <- lm(data = data, price ~ carat + x + y + z)
summary(model)

model1 <- lm(data = data, price ~ carat)
model2 <- lm(data = data, price ~ carat + y + x)
mtable(model1, model2)

confint(model2, level = 0.9)

summary(lm(data = data, price ~ carat + x + y + table))
