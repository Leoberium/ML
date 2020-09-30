#### Выборка. Описательная статистика. ####

# Порядковые статистики
x <- scan("dataset_26210_4.txt", sep = ",")
sort(x)[c(1, 3, 7)]

# Гистограмма
hist(rexp(50, 0.2))
hist(rnorm(50, 5, 2))
hist(rnorm(50, 5, 0.5))
hist(rpois(50, 5))
hist(runif(50, 1, 9))
hist(rgeom(50, 0.3))

# Частоты в интервалах
x <- scan("dataset_26210_10.txt", sep = ",")
table(cut(x, seq(from = 15, to = 21, by = 0.75)))/length(x)

# Box-plots
boxplot(rexp(50, 0.2), ylim = c(0, 25))
boxplot(rgeom(50, 0.3), ylim = c(0, 25))
boxplot(runif(50, 1, 9), ylim = c(0, 25))
boxplot(rnorm(50, 5, 2), ylim = c(0, 25))
boxplot(rnorm(50, 5, 0.5), ylim = c(0, 25))

# Задания

# 1
library(dplyr)
taskdata <- read.table("https://stepik.org/media/attachments/lesson/40531/13_6", sep = "\t")
names(taskdata) <- c("type", "date", "Fr6", "Fr13", "supermarket")
taskdata <- taskdata[2:5]
options(digits = 10)
taskdata %>% summarise(mean(Fr13), median(Fr13), 44*var(Fr13)/45)

kurtosis <- function(x) {
  moment4 <- sum((x-mean(x))^4)/length(x)
  s <- sqrt(sum((x-mean(x))^2)/length(x))
  return(moment4/s^4-3)}
asymmetry <- function(x) {
  moment3 <- sum((x-mean(x))^3)/length(x)
  s <- sqrt(sum((x-mean(x))^2)/length(x))
  return(moment3/s^3)}
taskdata %>% mutate(diffnum = Fr13 - Fr6) %>% 
  summarise(mean = mean(diffnum), kurtosis = kurtosis(diffnum), asymmetry = asymmetry(diffnum))
taskdata <- taskdata %>% mutate(diffnum = Fr13 - Fr6)

par(mfrow = c(2, 2))
boxplot(x = taskdata[c(2, 3, 5)])
hist(x = taskdata$Fr13)
hist(x = taskdata$Fr6)
hist(x = taskdata$diffnum)

# 2
taskdata <- read.table(file = "https://stepik.org/media/attachments/lesson/40531/colleges.txt", 
                       header = TRUE, sep = "\t", check.names = FALSE)
par(mfrow = c(3, 2))
for (i in 3:8) boxplot(taskdata[[i]] ~ taskdata[[2]], main = colnames(taskdata)[i])
taskdata.splitted <- split(taskdata, f = taskdata$School_Type)
par(mfrow = c(2, 6))
for (i in 3:8) hist(taskdata.splitted$`Lib Arts`[[i]], main = colnames(taskdata[i]))
for (i in 3:8) hist(taskdata.splitted$Univ[[i]], main = colnames(taskdata[i]))
mean(taskdata.splitted$`Lib Arts`$SAT)
quantile(x = taskdata.splitted$Univ$Acceptance, probs = 0.75)

#### Точечные оценки и их свойства ####

setwd('~/Downloads/')
x <- as.numeric(unlist(strsplit(readLines('dataset_39625_3.txt'), split = ', ')))
mean(x)

x <- c(17.4, 19.9, 19.9, 19.0, 20.7, 18.2, 18.6, 20.3, 21.7, 20.8, 16.7, 20.3,
       22.5, 18.3, 21.1, 16.3, 22.0, 19.5, 18.1, 21.1, 20.0, 21.6, 21.5, 21.1,
       23.3, 23.0, 17.8, 19.5, 21.6, 22.9)
var(x) * (length(x) - 1) / length(x) # biased

# task: maximum likelihood for uniformly distributed random variable
x <- as.numeric(unlist(strsplit(readLines('dataset_39625_11.txt'), split = ', ')))
sort(x)

# task: poisson parameter estimation
sample_var <- function(x) sum((x - mean(x))**2) / length(x)
sample_var(x)

lambda = 5
pois10 = rpois(10, lambda = lambda)
pois100 = rpois(100, lambda = lambda)
pois1000 = rpois(1000, lambda = lambda)
pois10000 = rpois(10000, lambda = lambda)

sample_var(pois10); sample_var(pois100); sample_var(pois1000); sample_var(pois10000)
mean(pois10); mean(pois100); mean(pois1000); mean(pois10000)

# task: p and g max likelihood
setwd('~/Downloads/')
sampleset <- read.table('sample_2_4.txt', stringsAsFactors = FALSE)
colnames(sampleset) <- c('trials', 'result')

ll <- function(p, g, set) {
  i <- (set$trials - 1)
  res <- ifelse(set$result == 'F', p, (1 - p) * g)
  res <- res * (1 - p)**i * (1 - g)**i
  res <- sum(log(res))
  return(res)
}
ll(0.1, 0.5, set = sampleset)

lik <- numeric(length = 1e6)
cnt <- 0
for (p in seq(0.001, 1, 0.001)) {
  for (g in seq(0.001, 1, 0.001)) {
    cnt <- cnt + 1
    lik[cnt] <- ll(p = p, g = g, set = sampleset)
  }
}
p <- which.max(lik) %/% 1000 / 1000
g <- which.max(lik) %% 1000 / 1000

# task: poisson modelling

lambda <- 2
p10 <- rpois(n = 10, lambda = lambda)
p50 <- rpois(n = 50, lambda = lambda)
p100 <- rpois(n = 100, lambda = lambda)
p500 <- rpois(n = 500, lambda = lambda)
p1000 <- rpois(n = 1000, lambda = lambda)
p10000 <- rpois(n = 10000, lambda = lambda)
v <- function(x) sum((x - mean(x)) ** 2) / length(x)
mean(p10); mean(p50); mean(p100); mean(p500); mean(p1000); mean(p10000)
v(p10); v(p50); v(p100); v(p500); v(p1000); v(p10000)
hist(replicate(100, mean(c(rpois(n = 10000, lambda = lambda), rnorm(n = 100, mean = 2.5, sd = 3)))))
hist(replicate(100, v(c(rpois(n = 10000, lambda = lambda), rnorm(n = 100, mean = 2, sd = 3)))))

#### Доверительные интервалы. Стратифицированные выборки ####

# task 1
s1 <- c(282, 226, 188, 327, 344, 304, 414, 224, 335, 270)
s2 <- c(417,  851,  742, 1217, 1160,  993,  864,  852, 1286,  988)
w1 <- 0.4; w2 <- 0.6
xs <- w1 * mean(s1) + w2 * mean(s2)
disp <- function(x) sum((x - mean(x))^2) / length(x)
ds1 <- w1 * disp(s1) + w2 * disp(s2)
ds2 <- w1 * (xs - mean(s1))^2 + w2 * (xs - mean(s2))^2
ds <- ds1 + ds2

# task 2
qf(0.9, 5, 15)


# task 3
setwd('~/Downloads/')
x <- as.numeric(unlist(strsplit(readLines('dataset_16004_10.txt'), split = ', ')))
x <- c(0.47, 0.13, -0.98, 0.74, -2.11, -3.36, -0.35, -2.21, 1.14, -0.13, 1.12, 1.49, 0.77, 0.79, 1.13, -1.45, 0.92, -0.08, 0.62, -0.51)
s_sq <- function(x) sum(x^2) / length(x)
y <- length(x) * s_sq(x)
s_low <- y / qchisq(p = 0.975, df = length(x))
s_high <- y / qchisq(p = 0.025, df = length(x))
print(c(s_low, s_high))

# task 4
x <- as.numeric(unlist(strsplit(readLines('dataset_16004_16.txt'), split = ', ')))
x <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
m <- sum(x)
n <- length(x)
term <- sqrt(m * (1 - m / n) / n**2)
s_low <- round(m/n - qnorm(0.975) * term, 4)
s_high <- round(m/n + qnorm(0.975) * term, 4)

# task 5
std_err <- 20
credible_prob <- 0.9
max_interval <- 30
n <- 1
while(qnorm(0.95, mean = 0, sd = std_err / sqrt(n)) > max_interval / 2) {
  n <- n + 1
}
n
round(qnorm(0.95), 2)

# task 6
n <- 10000
m <- 4000
term <- sqrt(m * (1 - m / n) / n**2)
s_low <- round(m/n - qnorm(0.975) * term, 3)
s_high <- round(m/n + qnorm(0.975) * term, 3)

#### Критерии о параметрах нормального распределения ####

# task 1
n <- 100
x <- 3.3
s <- 1
qt(0.1, n - 1)
pt((x - 3.5) * sqrt(n), df = n - 1)

# task 2
n <- 40
x <- 14.4
s <- sqrt(n) * 2.915 / sqrt(n - 1)
z <- sqrt(n) * (x - 13.2) / s
1 - pt(z, df = n - 1)

# task 3
ceiling(4 * (qnorm(0.95) - qnorm(0.1)) ** 2 / 0.25)

# task 4
x <- c(986, 1005, 991, 994, 983, 1002, 996, 998, 1002, 983)
x_ <- mean(x)
alpha <- 0.05
a <- 1000
df <- length(x) - 1
s <- sqrt(sum((x - x_) ** 2) / df)
z <- (x_ - a) * sqrt(length(x)) / s
p_value <- 2 * min(pt(z, df = df), 1 - pt(z, df = df))
p_value < alpha

# task 5
alpha <- 0.1
n <- 20
qchisq(alpha, df = n - 1)

# task 6
sigma_2 <- 9
s_2 <- 13.5
n <- 25
z <- (n - 1) * s_2 / sigma_2
alpha <- 0.05
qchisq(1 - alpha, df = n - 1)
p_value <- 1 - pchisq(z, df = n - 1)

# task 7
sigma_2 <- 21
alpha <- 0.01
df <- 20
qchisq(alpha, df)

# task 8
m <- 135
n <- 225
p0 <- 0.5
alpha <- 0.01
left <- n * p0 + sqrt(n * p0 * (1 - p0)) * qnorm(1 - alpha)

# task 9
m <- 60
n <- 747
p0 <- 0.25
alpha <- 0.02
right <- n * p0 + sqrt(n * p0 * (1 - p0)) * qnorm(alpha)

# task 10
x <- c(1380, 1344, 1356, 1291, 1308, 1271, 1371, 1430,
       1381, 1457, 1492, 1240, 1256, 1466, 1214, 1448,
       1510, 1395, 1507, 1264, 1293, 1251, 1380, 1386,
       1411, 1434, 1302, 1529, 1352, 1494, 1348, 1464,
       1286, 1345, 1491, 1259, 1541, 1214, 1310, 1286)
n <- length(x)
df <- n - 1
a <- 1222
x_ <- mean(x)
s <- sqrt(var(x))
z <- (x_ - a) * sqrt(n) / s
p_value <- 1 - pt(z, df = df)
p_value

#### Параметрические критерии однородности ####

# task 1
n_a <- 9
n_b <- 11
s_a <- 5.9 * n_a / (n_a - 1)
s_b <- 23.3 * n_b / (n_b - 1)
quantile <- qf(p = 0.6, df1 = n_a - 1, df2 = n_b - 1)
alpha <- 0.01
z <- s_a / s_b
p1 <- pf(q = z, df1 = n_a - 1, df2 = n_b - 1)
p2 <- 1 - pf(q = z, df1 = n_a - 1, df2 = n_b - 1)
quantile_v_1 <- qf(p = 0.005, df1 = n_a - 1, df2 = n_b - 1)
quantile_v_2 <- qf(p = 0.995, df1 = n_a - 1, df2 = n_b - 1)

# task 2
x_1 <- 12.57; d_1 <- 0.91; n_1 <- 16
x_2 <- 11.87; d_2 <- 1.51; n_2 <- 9
alpha <- 0.02
s_1 <- d_1 * n_1 / (n_1 - 1); s_2 <- d_2 * n_2 / (n_2 - 1)
k <- (s_1 / n_1 + s_2 / n_2) ** 2 /
  ((s_1 / n_1) ** 2 / (n_1 - 1) + (s_2 / n_2) ** 2 / (n_2 - 1))
qt(0.3, df = k)
z <- (x_1 - x_2) / sqrt(s_1 / n_1 + s_2 / n_2)
p_value <- 2 * (1 - pt(z, df = k))

# task 3
dataset1 <- read.table('Downloads/cholesterol.txt', header = TRUE)
n <- nrow(dataset1)
qt(0.9, n - 1)
q <- mean(dataset1$Day4) - mean(dataset1$Day2)
s_q <- sum((dataset1$Day4 - dataset1$Day2 - q) ** 2) / (n - 1)
z <- q * sqrt(n) / sqrt(s_q)
p_value <- 2 * min(1 - pt(z, df = n - 1), pt(z, df = n - 1))

#### Непараметрические критерии однородности ####

# task 1
x <- c(156, 171, 133, 102, 129, 150, 120, 110, 112, 130)
y <- c(73, 81, 103, 88, 131, 106, 107, 111, 122, 108)
m <- length(y)
n <- length(x)
w <- sum(match(y, sort(c(x, y))))
alpha <- 0.05
left <- m * (m + 1) / 2 + qwilcox(alpha / 2, m = m, n = n)
right <- m * (m + 1) / 2 + qwilcox(1 - alpha / 2, m = m, n = n)

# task 2
x <- c(69, 80, 92, 81, 70, 79, 78, 66, 57, 77)
y <- c(60, 84, 87, 79, 73, 71, 72, 67, 59, 70)
z <- x - y
a <- sum(z < 0)
w <- 12.5
w_sign <- qsignrank(0.05, n = 10)
qbinom(0.05, size = 10, prob = 1 / 2)

#### Однофакторный дисперсионный анализ ####

# task 1
x_1 <- c(5.9, 6.0, 7.0, 6.5, 5.5, 7.0, 8.1, 7.5, 6.2, 6.4, 7.1, 6.9)
x_2 <- c(4.0, 5.1, 6.2, 5.3, 4.5, 4.4, 5.3, 5.4, 5.6, 5.2)
x_3 <- c(8.2, 6.8, 8.0, 7.5, 7.2, 7.9, 8.1, 8.5, 7.8, 8.1)
n <- length(c(x_1, x_2, x_3))
q_1 <- sum((x_1 - mean(x_1)) ** 2) +
  sum((x_2 - mean(x_2)) ** 2) +
  sum((x_3 - mean(x_3)) ** 2)
x_m <- mean(c(x_1, x_2, x_3))
q_2 <- length(x_1) * (mean(x_1) - x_m) ** 2 +
  length(x_2) * (mean(x_2) - x_m) ** 2 +
  length(x_3) * (mean(x_3) - x_m) ** 2
d <- q_1 / (length(x_1) + length(x_2) + length(x_3) - 3)
alpha <- 0.1
f <- (q_2 / 2) / (q_1 / (n - 3))
df1 <- 2
df2 <- n - 3
qf(1 - alpha, df1 = df1, df2 = df2)
lk12 <- mean(x_1) - mean(x_2)
s_lk12 <- q_1 / ((n - 3) * length(x_1)) + q_1 / ((n - 3) * length(x_2))
left <- lk12 - sqrt(s_lk12 * df1 * qf(1 - alpha, df1 = df1, df2 = df2))
right <- lk12 + sqrt(s_lk12 * df1 *qf(1 - alpha, df1 = df1, df2 = df2))

# task 2
light_blonde <- c(62, 60, 71, 55, 48)
dark_blonde <- c(63, 57, 52, 41, 43)
light_brunette <- c(42, 50, 44, 37)
dark_brunette <- c(32, 39, 51, 30, 35)
all <- sort(c(light_blonde, dark_blonde, light_brunette, dark_brunette))
light_blonde_mean <- mean(match(light_blonde, all))
dark_blonde_mean <- mean(match(dark_blonde, all))
light_brunette_mean <- mean(match(light_brunette, all))
dark_brunette_mean <- mean(match(dark_brunette, all))
n <- length(all)
H <- 12 / (n * (n + 1)) * 
  (
    light_blonde_mean ** 2 * length(light_blonde) +
    dark_blonde_mean ** 2 * length(dark_blonde) +
    light_brunette_mean ** 2 * length(light_brunette) +
    dark_brunette_mean ** 2 * length(dark_brunette)
  )  - 3 * (n + 1)
left <- qchisq(1 - 0.05, df = 3)

# task 3
rugby <- read.table(file = 'Downloads/rugby.txt', header = TRUE)
k <- length(unique(rugby$Game))
n <- nrow(rugby)
alpha <- 0.05
qleft <- qf(1 - alpha, df1 = k - 1, df2 = n - k)
q1 <- sum(tapply(rugby$Time, rugby$Game, function(x) {
  sum((x - mean(x)) ** 2)
}))
total_mean <- mean(rugby$Time)
q2 <- sum(tapply(rugby$Time, rugby$Game, function(x) {
  length(x) * (mean(x) - total_mean) ** 2
}))
f <- q2 * (n - k) / (q1 * (k - 1))

# task 4
pulse <- read.table(file = 'Downloads/pulse.txt', header = TRUE)
x <- pulse$Pulse1 - pulse$Pulse2
s <- sum((x - mean(x)) ** 2) / (length(x) - 1)
alpha <- 0.02
left <- mean(x) - qnorm(1 - alpha / 2) * sqrt(s) / sqrt(length(x))
right <- mean(x) + qnorm(1 - alpha / 2) * sqrt(s) / sqrt(length(x))

#### Критерии согласия хи-квадрат и Колмогорова ####

# task 1
x <- c(0.29, 0.01, 0.50, 0.21, 0.65, 0.34, 0.75, 0.07,
       0.07, 0.25, 1.26, 0.11, 0.22, 0.95, 0.63, 0.93,
       0.73, 0.37, 0.80, 1.10)
n <- 20
lambda <- 2
left <- 0.2; right <- 0.5
p2 <- pexp(right, rate = lambda) - pexp(left, rate = lambda)
p1 <- pexp(left, rate = lambda)
p3 <- 1 - p2 - p1
n1 <- sum(x <= left)
n2 <- sum(left < x & x <= right)
n3 <- sum(right < x)
all(n * p1 > 5, n * p2 > 5, n * p3 > 5)
alpha <- 0.01
df <- 2
qchisq(1 - alpha, df = df)
schi <- (n1 - n * p1) ** 2 / (n * p1) +
  (n2 - n * p2) ** 2 / (n * p2) +
  (n3 - n * p3) ** 2 / (n * p3)

# task 2
x <- c(444, 447, 450, 451, 454)
m <- ks.test(x, pnorm, mean = 450, sd = 4)
sqrt(length(x)) * m$statistic

# task 3
library(purrr)
lambda = round(445 / 757, 1)
x <- c(427, 235, 72, 21, 1, 1, 0)
failures <- sum(x)
expected_0 <- failures * dpois(x = 0, lambda = lambda)
expected_1 <- failures * dpois(x = 1, lambda = lambda)
expected_2 <- failures * dpois(x = 2, lambda = lambda)
expected_3 <- failures * dpois(x = 3, lambda = lambda)
expected_4 <- failures * dpois(x = 4, lambda = lambda)
expected_5 <- failures * dpois(x = 5, lambda = lambda)
x_new <- c(427, 235, 72, 23)
p3 <- 1 - dpois(x = 0, lambda = lambda) - dpois(x = 1, lambda = lambda) - dpois(x = 2, lambda = lambda)
chistat <- (x_new[1] - expected_0) ** 2 / expected_0 +
  (x_new[2] - expected_1) ** 2 / expected_1 +
  (x_new[3] - expected_2) ** 2 / expected_2 +
  (x_new[4] - p3 * failures) ** 2 / (p3 * failures)
alpha <- 0.01
l <- 1 # number of parameters
df <- 4 - 1 - l # because complex hypothesis, the distribution is unknown
left <- qchisq(1 - alpha, df = df)

#### Критерии нормальности ####

# task 1
x <- c(3.4, 7.0, 3.9, 6.1, 0.9, 4.6, 6.4, 3.2,
      1.8, 4.8, 3.8, 5.6, 1.3, 3.3, 3.7, 2.9,
      5.2, 3.7, 5.0, 3.9)
n <- length(x)
s <- sum((x - mean(x)) ** 2) / n
mu3 <- sum((x - mean(x)) ** 3) / n
mu4 <- sum((x - mean(x)) ** 4) / n
sk <- mu3 / s ** (3 / 2)
k <- mu4 / s ** (4 / 2) - 3
jb <- n * (sk ** 2 + k ** 2 / 4) / 6

# task 2
# x is the same as in task 1
a <- -n
x <- sort(x)
s <- sqrt(sum((x - mean(x)) ** 2) / (n - 1))
for (i in 1:n) {
  a <- a - (2 * i - 1) * (
    log(pnorm(q = x[i], mean = mean(x), sd = s)) +
    log(1 - pnorm(q = x[n + 1 - i], mean = mean(x), sd = s))
  ) / n
}


# task 3
x1 <- c(156, 171, 133, 102, 129, 150, 120, 110, 112, 130)
x2 <- c(73, 81, 103, 88, 131, 106, 107, 111, 122, 108)
f <- ecdf(x1)
g <- ecdf(x2)
d <- max(map_dbl(50:200, function(x) abs(f(x) - g(x))))
sqrt(5) * d
ks.test(x1, x2)

# task 4
b <- c(200, 190, 195, 190, 194, 202, 202, 207, 205, 220, 213, 196, 203)
v <- c(205, 202, 218, 213, 198, 198, 195, 204, 197, 202, 201, 205, 201, 197, 198, 197, 209)
e1 <- c(sum(b >= 189 & b <= 200), sum(v >= 189 & v <= 200))
e2 <- c(sum(b > 200 & b <= 210), sum(v > 200 & v <= 210))
e3 <- c(sum(b > 210 & b <= 221), sum(v > 210 & v <= 221))
vu1 <- sum(e1); vu2 <- sum(e2); vu3 <- sum(e3)
n <- length(b) + length(v)
p1 <- vu1 / n; p2 <- vu2 / n; p3 <- vu3 / n
c(p1, p2, p3) * length(b)
c(p1, p2, p3) * length(v)
# the union of 2nd and 3rd is required
p2 <- p2 + p3
e2 <- e2 + e3
vu2 <- vu2 + vu3
rm(e3); rm(p3); rm(vu3)
n1 <- length(b); n2 <- length(v)
z <- n * (
  (e1[1] - p1 * n1) ** 2 / (n1 * vu1) +
  (e2[1] - p2 * n1) ** 2 / (n1 * vu2) +
  (e1[2] - p1 * n2) ** 2 / (n2 * vu1) +
  (e2[2] - p2 * n2) ** 2 / (n2 * vu2)
)
z_corrected <- n * (
  (abs(e1[1] - p1 * n1) - 0.5) ** 2 / (n1 * vu1) +
  (abs(e2[1] - p2 * n1) - 0.5) ** 2 / (n1 * vu2) +
  (abs(e1[2] - p1 * n2) - 0.5) ** 2 / (n2 * vu1) +
  (abs(e2[2] - p2 * n2) - 0.5) ** 2 / (n2 * vu2)
)
alpha <- 0.05
qchisq(1 - alpha, df = (2 - 1) * (2 - 1))

#### Таблицы сопряженности ####

# task 1
m <- matrix(c(20, 23, 19, 11, 17, 16), nrow = 3, ncol = 2)
n <- sum(m)
m1 <- sum(m[1, ])
m2 <- sum(m[2, ])
m3 <- sum(m[3, ])
n1 <- sum(m[, 1])
n2 <- sum(m[, 2])
z <- (m[1, 1] - m1 * n1 / n) ** 2 / (m1 * n1 / n) +
  (m[1, 2] - m1 * n2 / n) ** 2 / (m1 * n2 / n) +
  (m[2, 1] - m2 * n1 / n) ** 2 / (m2 * n1 / n) +
  (m[2, 2] - m2 * n2 / n) ** 2 / (m2 * n2 / n) +
  (m[3, 1] - m3 * n1 / n) ** 2 / (m3 * n1 / n) +
  (m[3, 2] - m3 * n2 / n) ** 2 / (m3 * n2 / n)
a <- 0.05
qchisq(1 - a, df = (nrow(m) - 1) * (ncol(m) - 1))

# task 2
m <- matrix(c(3, 1, 1, 2), nrow = 2, ncol = 2)
m1 <- sum(m[1, ]); m2 <- sum(m[2, ])
n1 <- sum(m[, 1]); n2 <- sum(m[, 2])
n <- sum(m)
p3 <- factorial(m1) * factorial(n1) * factorial(m2) * factorial(n2) /
  factorial(m[1, 1]) / factorial(m[1, 2]) /
  factorial(m[2, 1]) / factorial(m[2, 2]) /
  factorial(n)
p0; p1; p2; p3
p1 + p3 + p0
fisher.test(m)

# task 3
alpha <- 0.01
x <- c(13.0, 9.8, 10.2, 12.9, 10.4)
sqrt(length(x)) * ks.test(x, punif, min = 9, max = 15)$statistic

# task 4
install.packages('nortest')
x <- read.table(file = 'Downloads/tcereals.txt', header = TRUE)
hist(x$rating)
hist(log(x$rating))
x <- log(x$rating)
alpha <- 0.03
shapiro.test(x)
library(nortest)
ad.test(x)
lillie.test(x)

# task 5
m <- matrix(c(120, 28, 2, 74, 59, 17), nrow = 2, byrow = TRUE)
alpha <- 0.05
chisq.test(m)
qchisq(1 - alpha, df = 2)

#### Множественная линейная регрессия, Анализ остатков ####

# task 1
x <- read.table(file = 'Downloads/car_regr.txt', header = TRUE)
n <- nrow(x)
betta1 <- cov(x = x$year, y = x$price) / var(x$year)
betta0 <- mean(x$price) - betta1 * mean(x$year)
e <- x$price - (betta0 + betta1 * x$year)
rss <- sum(e ** 2)
s <- sqrt(sum((e - mean(e)) ** 2) / (n - 2))
alpha <- 0.05
d_year <- sum((x$year - mean(x$year)) ** 2) / n
qt(1 - alpha / 2, df = n - 2) * s * sqrt(1 / n / d_year)
y_2012 <- betta0 + betta1 * 2012
q_l <- (y_2012 - 290) / s / sqrt((1 + (2012 - mean(x$year)) ** 2 / d_year) / n)
1 - pt(q_l, df = n - 2)

# task 2
# the data from the task 1
x$z <- ifelse(x$auto == 'MT', 1, 0)
# using lm
lm(price ~ year + mileage + z, x)$coefficients
# using matrix solving
a <- cbind(rep(x = 1, times = n), x$year, x$mileage, x$z)
ata <- t(a) %*% a
aty <- t(a) %*% x$price
round(solve(ata, aty), 2)
# identity matrix to get inverse of ata
identity_4 <- matrix(rep(0, 16), nrow = 4, ncol = 4)
diag(identity_4) <- rep(1, 4)
inv_ata <- solve(ata, identity_4)
# confidence interval value for betta2
alpha <- 0.05
e <- lm(price ~ year + mileage + z, x)$residuals
k <- 3
s <- sqrt(t(e) %*% e / (n - k - 1))
qt(1 - alpha / 2, df = n - k - 1) * s * sqrt(inv_ata[3, 3])
r_2 <- 1 - t(e) %*% e / sum((x$price - mean(x$price)) ** 2)
f <- r_2 / (1 - r_2) * (n - k - 1) / k
qf(1 - alpha, df1 = k, df2 = n - k - 1)
d2 <- e[2] / s / sqrt(1 - diag(a %*% inv_ata %*% t(a))[2])

# task 3
x <- seq(1, 10, 0.5)
y <- c(4.7, 8.9, 6.2, 7.8, 8.1, 11.7, 7.2, 15.8,
       1.1, 6.8, 9.1, 4.6, 21.5, 7.6, 6.2, 13.6,
       30.1, 25.5, -0.1)
y_ <- function(x) 4.66 + 1.03 * x
e <- y - y_(x)
s <- sqrt(t(e) %*% e / (n - 2))[1, 1]
d <- e / s
plot(y_(x), d)
plot(y, d)
plot(x, d)
plot(1:19, d)
qqnorm(d)
plot(x, y)

#### Корреляционный анализ ####

# task 1
x <- c(3.25, 2.51, 1.46, 2.37, 2.45, -0.07, 3.78, 0.53, 2.09, 1.74)
y <- c(0.62, 0.95, 0.37, 1.64, 0.60, 0.19, 1.81, 0.24, 1.26, -0.93)
n <- length(x)
r <- sum((x - mean(x)) * (y - mean(y))) / n / sqrt(
  sum((x - mean(x)) ** 2) / n *
  sum((y - mean(y)) ** 2) / n
)
t <- r * sqrt(n - 2) / sqrt(1 - r ** 2)
alpha <- 0.05
qt(alpha / 2, df = n - 2)
qt(1 - alpha / 2, df = n - 2)

# task 2
x <- c(7, 2, 5, 9, 8, 1, 10, 4, 6, 3)
y <- c(7, 4, 5, 10, 8, 2, 9, 3, 6, 1)
cor.test(x, y, method = 'kendall')

# task 3
weights <- read.table(file = '~/Downloads/weights.txt', header = TRUE)
model <- lm(formula = weight ~ race + educ + preg + smoke, data = weights)
e <- model$residuals
n <- nrow(weights); k <- 4
rss <- (t(e) %*% e)[1, 1]
s <- rss / (n - k - 1)[1]
a <- cbind(rep(1, n), weights$race, weights$educ,
           weights$preg, weights$smoke)
ata <- t(a) %*% a
inv_ata <- solve(a = ata, b = diag(nrow = 5))
alpha <- 0.05
betta3 <- model$coefficients[4]
round(qt(1 - alpha / 2, df = n - k - 1) * sqrt(s * inv_ata[4, 4]), 2)
r_2 <- 1 - rss / sum((weights$weight - mean(weights$weight)) ** 2)
f <- r_2 * (n - k - 1) / (1 - r_2) / k
1 - pf(f, df1 = k, df2 = n - k - 1)
summary(model)
AIC(model)
AIC(lm(formula = weight ~ race + preg, data = weights))

#### Итоговый тест ####

# task 3
x <- c(128, 107, 107, 92, 118, 114, 117, 109,
       115, 106, 109, 106, 109, 120, 112, 128, 
       114, 119, 110, 130)
median(x)
mean(x)
sum((x - mean(x)) ** 2) / length(x)

# task 7
x <- c(128, 107, 107, 92, 118, 114, 117, 109,
       115, 106, 109, 106, 109, 120, 112, 128, 
       114, 119, 110, 130)
alpha <- 0.03
n <- length(x)
s <- function(x) sqrt(sum((x - mean(x)) ** 2) / (length(x) - 1))
t <- sqrt(n) * (mean(x) - 110) / s(x)
1 - pt(t, df = n - 1)
t.test(x, mu = 110, alternative = 'greater')

# task 8
n <- 9
alpha <- 0.05
qbinom(p = 1 - alpha, size = n, prob = 0.5)

# task 9
alpha <- 0.05
x_means <- c(3.17, 2.72, 2.63, 2.29, 2.12)
x_vars <- c(0.5476, 0.5041, 0.5329, 0.49, 0.5184)
x_vars_mod <- x_vars * 200 / 199
total_mean <- mean(x_means)
q1 <- sum(x_vars)
q2 <- sum((x_means - total_mean) ** 2)
N <- 1000
k <- 5
f <- (q2 / (k - 1)) / (q1 / (N - k))
qf(1 - alpha, df1 = k - 1, df2 = N - k)

# task 10
retina1 <- c(19.5, 15.0, 13.5, 23.3, 6.3, 2.5, 13.0, 1.8, 6.5, 1.8)
retina2 <- c(0.0, 38.5, 59.0, 97.4, 119.2, 129.5, 198.7, 248.7, 318.0, 438.5)
lmfit <- lm(formula = retina2 ~ retina1)
summary(lmfit)
e <- lmfit$residuals
rss <- sum(e ** 2)
s_2 <- rss / 8
anova(lmfit)
