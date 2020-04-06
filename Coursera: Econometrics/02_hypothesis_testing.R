library(tidyverse)

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
