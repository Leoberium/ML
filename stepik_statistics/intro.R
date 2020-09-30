#### Основы статистики-I ####

genthrp <- read.csv("https://stepik.org/media/attachments/lesson/8083/genetherapy.csv")
summary(aov(expr ~ Therapy, data = genthrp)) # One-factor ANOVA
# Нулевая гипотеза отклоняется

library(ggplot2)
p <- ggplot(genthrp, aes(Therapy, expr))
p + geom_boxplot(fill = "white", colour = "#3366FF") + geom_jitter(width = 0.2)

atsc <- read.csv("https://stepik.org/media/attachments/lesson/9250/atherosclerosis.csv")
summary(aov(expr ~ age + dose, data = atsc)) # Two-way ANOVA

birds <- read.csv("https://stepik.org/media/attachments/lesson/9250/birds.csv") 
summary(aov(var4 ~ sex + hormone + sex*hormone, data = birds))

states <- read.csv("https://stepik.org/media/attachments/lesson/8086/states.csv")
plot(states$hs_grad, states$poverty, lwd = 5, pch = 19)
cor.test(states$hs_grad, states$poverty)
fit <- lm(poverty ~ hs_grad, data = states)
summary(fit)
fit2 <- lm(poverty ~ hs_grad + white, data = states)
summary(fit2)
fit3 <- lm(poverty ~ . -state, data = states)
summary(fit3)

library(corrplot)
M <- cor(states[2:6])
corrplot(M)

fit4 <- lm(poverty ~ . -state-female_house, data = states)
summary(fit4)
#### Основы статистики-II Module 1 ####

# Расчёт ожидаемых значений в R
O <- matrix(c(20, 11, 7, 15, 12, 9), ncol = 2)
E <- outer(rowSums(O), colSums(O)) / sum(O)

print(addmargins(O))
print(addmargins(E))

print(chisq.test(O)$expected)
all.equal(chisq.test(O)$expected, E)
chisq.test(O)

# График для таблицы частот
patients <- rbind(c(18, 7), c(6, 13))
colnames(patients) <- c("Yes", "No")
rownames(patients) <- c("Placebo", "Aspirin")
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group")
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")

# Сдвинем распределение
patients2 <- rbind(c(25, 1), c(3, 30))
colnames(patients2) <- c("Yes", "No")
rownames(patients2) <- c("Placebo", "Aspirin")
mosaicplot(patients2, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")

# Точный критерий Фишера
fisher.test(cbind(c(1, 3), c(3, 1)))

# Задачи 1-й модуль
NA_position  <- function(x, y){
  all(is.na(x) == is.na(y))
}

smart_test <-  function(x){
  y <- table(x)
  if (all(y >= 5)) {
    z <- chisq.test(y)
    return(c(z$statistic, z$parameter, z$p.value))
  } else {
    z <- fisher.test(y)
    return(z$p.value)
  }
}
smart_test(mtcars[1:20,c("am", "vs")])

most_significant <- function(x){
  pvs <- sapply(x, function(i) chisq.test(table(i))$p.value)
  names(pvs[pvs == min(pvs)])
}
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant(test_data)

iris$important_cases <- as.factor(sapply(1:nrow(iris), function(i) {
  means <- colMeans(iris[, 1:4])
  compv <- iris[i, 1:4] > means
  if (sum(compv) >= 3) return("Yes") else return("No")
}))


get_important_cases <- function(x) {
  numcols <- sapply(x, is.numeric)
  means <- colMeans(x[, numcols])
  numlen <- sum(numcols)
  x$important_cases <- apply(x[, numcols], 1, function(i) sum(i>means)/numlen > 1/2)
  x$important_cases <- factor(x$important_cases, levels <- c(TRUE, FALSE), 
                              labels = c("Yes", "No"))
  return(x)
}
get_important_cases(iris)

stat_mode <- function(x) {
  y <- sort(table(x), decreasing = TRUE)
  as.numeric(names(y)[unique(y)[1] == y])
}

stat_mode(c(1, 1, 1, 2, 3, 3, 3))

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
str(test_data)

max_resid <- function(x){
  y <- chisq.test(table(x))$stdres
  result <- colnames(y)[col(y)[max(y) == y]]
  drugs <- rownames(y)[row(y)[max(y) == y]]
  return(c(drugs, result))
}
max_resid(test_data)

# Ещё реализация:
max_resid2 <- function(data) {
  df <- as.data.frame(chisq.test(table(data))$stdres)
  sapply(subset(df, Freq == max(Freq), 1:2), as.character)
}
# Интересное свойство функции as.data.frame в том, что если на вход она получает table, то преобразует данные из матричного вида в так называемый melted формат, когда каждое значение матрицы представлено отдельной строкой.

library(ggplot2)
str(diamonds)
obj <- ggplot(diamonds, aes(x = color, fill = cut)) +
  stat_count(position = position_dodge())
obj

obj2 <- ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar(position = "dodge")
obj2

#### Основы статистики-II Module 2 ####
library(dplyr)
library(ggplot2)
library(vcd)

# Модель без предикторов (Intercept model only)
titanic <- read.csv("https://stepic.org/media/attachments/course/524/train.csv")
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)
table(titanic$Survived)
odds <- 290/424
log(odds)
summary(simple_fit)

# Модель с одним номинативным предиктором
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
coef(fit1)
table(titanic$Survived, titanic$Sex)

odds_male <-  93/360
odds_female <-  197/64
log(odds_female)
log(odds_male)

odds_ratio <- odds_male / odds_female
log(odds_ratio)

# Сравнение моделей
anova(simple_fit, fit1, test = "Chisq")
anova(fit1, test = "Chisq")

# Модель с двумя категориальными предикторами
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
coef(fit2)
summary(fit2)

table(titanic$Survived, titanic$Pclass, titanic$Sex)
mosaic(~ Sex + Survived | Pclass, data = titanic)

# Intercept
female_p1_odds <- 82/3
log(female_p1_odds)

# SexMale
male_p1_odds <- 40/61
log(male_p1_odds)
log(male_p1_odds/female_p1_odds)

# PclassSecond
female_p2_odds <- 68/6
log(female_p2_odds)
log(female_p2_odds/female_p1_odds)

# PclassThird
female_p3_odds <- 47/55
log(female_p3_odds)
log(female_p3_odds/female_p1_odds)

# SexMale:PclassSecond
male_p2_odds <- 15/84
log(male_p2_odds)
log(male_p2_odds/female_p2_odds) - log(male_p1_odds/female_p1_odds)

# SexMale:PclassThird
male_p3_odds <- 38/215
log(male_p3_odds)
log(male_p3_odds/female_p3_odds) - log(male_p1_odds/female_p1_odds)

anova(fit1, fit2, test = "Chisq")
anova(fit2, test = "Chisq")

# Предсказание новых данных
new_data <- data.frame(Sex = "Female", Pclass = "First")
predict(fit2, newdata = new_data, type = "response")

# Модель с разными переменными
fit3 <- glm(Survived ~ Sex + Pclass + Age, titanic, family = "binomial")
summary(fit3)
anova(fit3, test = "Chisq")

# Тест на нормальность Shapiro-Wilk, симуляция:
library(dplyr)
excNum <- 1e4
excSize <- 3
normpoints <- data.frame(x = rnorm(excNum * excSize),
                         exc = rep(1:excNum, each = excSize))
groupedpoints <- normpoints %>% group_by(exc) %>% arrange(x, .by_group = TRUE)
groupedpoints$n <- rep(1:excSize, times = excNum)
groupedpoints
ggplot(groupedpoints, aes(x, fill = factor(n))) +
  geom_density(alpha = .5)

# Симуляция для t-testa (from comments):
t_test_dist <- function(dist = "rnorm", mu = 0, n = 100, rep = 1000) {
  fun <- match.fun(dist)
  data <- matrix(fun(n * rep), ncol = n) # симуляция данных
  test <- apply(data, 1, function(x) t.test(x, mu = mu)$statistic) # расчёт статистики
  hist(test, freq = FALSE) # гистограмма
  lines(density(test)) # плотность распределения
}
t_test_dist()

# QQplot: корреляция при проверке на нормальность
x <- runif(1000)
cor(sort(x), qnorm(ppoints(length(x))))


# Задачи 2-й модуль

# 1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
# переведем переменные в фактор
test_data  <- transform(test_data, x = factor(x), y = factor(y))
get_coefficients <- function(dataset) {
  fit <- glm(y ~ x, data = dataset, family = "binomial")
  return(exp(coef(fit)))
}
get_coefficients(test_data)

# 2
test_data <- read.csv("https://stepik.org/media/attachments/course/524/cen_data.csv")
var_names <- c("X4", "X2", "X1")
centered <- function(dataset, varcols) {
  dataset[, varcols] <- scale(x = dataset[, varcols], scale = FALSE)
  return(dataset)
}
centered(test_data, var_names)
centered(test_data, "X1")

# 3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
str(test_data)
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
str(test_data2)
get_features <- function(dataset) {
  fit <- glm(is_prohibited ~ ., data = dataset, family = "binomial")
  result <- anova(fit, test = "Chisq")
  ssp <- rownames(result)[-1][result$`Pr(>Chi)`[-1] < 0.05]
  if (length(ssp) == 0) return("Prediction makes no sense")
  else return(ssp)
}
get_features(test_data)
get_features(test_data2)

# 4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
str(test_data)
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
str(data_for_predict)
most_suspicious <- function(dataset, datasetpredict){
  fit <- glm(is_prohibited ~., data = dataset, family = "binomial")
  datasetpredict$odds <- predict(fit, newdata = datasetpredict)
  danger <- max(datasetpredict$odds)
  as.character(datasetpredict$passangers[datasetpredict$odds == danger])
}
most_suspicious(test_data, data_for_predict)

# 5
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
normality_test <- function(dataset) {
  numcols <- sapply(dataset, is.numeric)
  apply(dataset[, numcols], 2, function(i) shapiro.test(i)$p.value)
}
normality_test(test_data)
normality_test(iris)

normality_test2 <- function(df){
  library(dplyr)
  unlist(df %>% summarise_if(is.numeric, funs(shapiro.test(.)$p.value)))
}
normality_test2(test_data)
normality_test2(iris)

# 6
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
aggregate(x = test_data$x, by = list(test_data$y), mean)
smart_anova <- function(dataset) {
  library(dplyr)
  normality <- dataset %>% group_by(y) %>% 
    summarise(ps = shapiro.test(x)$p.value) %>% 
    pull()
  variance <- bartlett.test(x ~ y, data = dataset)$p.value
  test <- rbind(variance, normality)
  if (all(test >= 0.05)) {
    paov <- summary(aov(x ~ y, data = dataset))[[1]]$"Pr(>F)"[1]
    names(paov) <- "ANOVA"
    return(paov)
  } else {
    pksl <- kruskal.test(x ~ y, data = dataset)$p.value
    names(pksl) <- "KW"
    return(pksl)
  }
}
smart_anova(test_data)

# 7
normality_by <- function(dataset){
  # library(dplyr)
  # result <- dataset %>% group_by_at(.vars = vars(2:3))
  #   summarise_at(.vars = vars(1), function(i) shapiro.test(i)$p.value)
  # return(result)
  result <- aggregate(x = dataset[, 1], by = dataset[, -1], FUN = function(i) shapiro.test(i)$p.value)
  names(result)[3] <- "p_value"
  return(result)
}

normality_by2 <- function(dataset){
  library(dplyr)
  result <- dataset %>% group_by_at(.vars = 2:3) %>% 
    summarise_at(.vars=1, function(i) shapiro.test(i)$p.value)
  names(result)[3] <- "p_value"
  return(as.data.frame(result))
}
normality_by(mtcars[, c("mpg", "am", "vs")])
normality_by2(mtcars[, c("mpg", "am", "vs")])

# 8
str(iris)
obj <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5)
obj

#### Основы статистики-II Module 3 ####

# Кластерный анализ методом k-средних

# Визуализация 1
library(ggplot2)
dataset <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(dataset, 3)
dataset$clusters <- factor(fit$cluster)

ggplot(dataset, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

# Визуализация 2
library(cluster)
KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "Hartigan-Wong")
clusplot(iris[1:4], KM$cluster, color = TRUE, 
         shade = TRUE, labels=2,
         main = 'Cluster Analysis for Iris')

# Визуализация 3
library(factoextra)
theme_set(theme_minimal())
d <- iris[, c("Sepal.Length", "Petal.Width")]
fit <- kmeans(d, 3)
fviz_cluster(fit, d)

# Как определить оптимальное число кластеров?

# 1
library(NbClust)
library(doMC)
registerDoMC(8)
data(iris)
dt <- iris[, 1:4]
N <- NbClust(dt, distance = "euclidean",
             min.nc = 2, max.nc = 15, method = "complete", 
             index = "alllong")
# 2
fpc::kmeansruns(data = dt, krange = 2:10)

# Иерархическая кластеризация
library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 2) # укажите желаемое число кластеров, сейчас стоит 2

library(ape)
set.seed(222)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
#левое дерево
plot.phylo(tr) 
#правое дерево 
plot.phylo(tr, use.edge.length=FALSE)

# Введение в метод анализа главных компонент
data(swiss)
fit <- prcomp(swiss, center = T)
plot(fit, type = "l")
summary(fit)
biplot(fit)

# В комментариях под 5-м видео есть реализация графического отображения PCA получше

install.packages("pca3d")
library(pca3d)

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
pca3d(fit, group = dt$is_catholic,
      fancy = T, 
      new=T)

fit <- factanal(swiss, factors = 2, rotation = "varimax")
print(fit)

# Задачи

# 1
smart_hclust <- function(test_data, cluster_number) {
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  test_data$cluster <- factor(cutree(fit, cluster_number))
  return(test_data)
}
smart_hclust(read.csv(
  "https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
  )

# 2
test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
get_difference <- function(dataset, n_cluster = 2) {
  dist_matrix <- dist(dataset)
  fit <- hclust(dist_matrix)
  dataset$cluster <- factor(cutree(fit, n_cluster))
  aovres <- lapply(subset(dataset, select = -cluster),
                    function(i) aov(i ~ cluster, data = dataset))
  result <- sapply(aovres, function(i) summary(i)[[1]]$`Pr(>F)`)[1, ]
  return(names(result)[result < 0.05])
}
get_difference(test_data1)
get_difference(test_data2)

# 3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
lol <- prcomp(test_data)
lol2 <- summary(lol)
lol2$importance[3, ] <= 0.9
get_pca <- function(dataset) {
  return(cbind(dataset, prcomp(dataset)$x[, 1:2]))
}
get_pc(test_data)

# 4
get_pca2 <- function(dataset) {
  fit <- prcomp(dataset)
  maxcomp <- which.max(summary(fit)$importance[3, ] > 0.9)
  return(cbind(dataset, fit$x[, 1:maxcomp]))
}
get_pca2(swiss)

# 5
test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
test_data3 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data4 <- as.data.frame(list(V1 = c(-13, -7, 7, 3, 2), V2 = c(18, 18, 6, 9, 4), 
                                V3 = c(19, 13, -1, 3, 4), V4 = c(1, 9, 8, 10, 0), 
                                V5 = c(12, 12, 0, 3, -2)))
test_data5 <- as.data.frame(list(V1 = c(7, -2, 4, 13, 20), V2 = c(-2, 7, 1, -8, -15),
                                 V3 = c(5, 13, 31, 14, 3), V4 = c(13, 37, 0, 3, 24),
                                 V5 = c(4, 19, 3, 6, 25), V6 = c(6, 0, 8, 15, 15),
                                 V7 = c(9, 24, 8, 11, 30)))

is_multicol <- function(dataset) {
  colpreds <- vector()
  for (i in names(dataset)) {
    for (j in names(dataset[names(dataset) != i])) {
      tsa1 <- dataset[[i]] - dataset[[j]]
      tsa2 <- dataset[[i]] + dataset[[j]]
      tsd <- sapply(2:length(tsa1), function(i) {
        isTRUE(all.equal(tsa1[1], tsa1[i]))
      })
      tss <- sapply(2:length(tsa2), function(i) {
        isTRUE(all.equal(tsa2[1], tsa2[i]))
      })
      if ( ( all(tss) || all(tsd) )&& !(c(i, j) %in% colpreds)) {
        colpreds <- append(colpreds, c(i, j))
      }
    }
  }
  if (length(colpreds) != 0) return(colpreds)
  else return("There is no collinearity in the data")
}

lol <- data.frame(x = rep(1, times = 100), y = rep(2, times = 100))
is_multicol(lol)
is_multicol(test_data1)
is_multicol(test_data2)
is_multicol(test_data3)
is_multicol(test_data4)

# 6
swiss$cluster <- factor(cutree(hclust(dist(swiss)), 2))
library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) +
  geom_point() + geom_smooth(method = "lm")
my_plot

#### Основы статистики-III Module 1 ####

# Линейность взаимосвязи

library(ggplot2)
qplot(x = hp, y = mpg, data = mtcars)
qplot(x = hp^0.5, y = mpg, data = mtcars)
qplot(x = hp^-0.5, y = mpg, data = mtcars)
qplot(x = -hp^-0.5, y = mpg, data = mtcars)

fit1 <- lm(mpg ~ hp, mtcars)
fit2 <- lm(mpg ~ I(-hp^-0.7), mtcars)
summary(fit1)
summary(fit2)
hist(fit1$residuals) # без трансформации переменных остатки распределены ненормально
shapiro.test(fit1$residuals)
hist(fit2$residuals) # остатки распределены нормально
shapiro.test(fit2$residuals)

find_lambda <- function(x, y, from = -5, to = 5, by = 0.01) {
  lambda <- seq(from, to, by)
  transX <- outer(x, lambda, "^")
  transX[, lambda < 0] <- -transX[, lambda < 0]
  transX[, lambda == 0] <- log(x)
  r <- cor(transX, y)[, 1]
  return(lambda[which.max(abs(r))])
  # return(transX)
}
find_lambda(x = mtcars$hp, y = mtcars$mpg)

# Логарифмическая трансформация переменных

qplot(x = log(hp), y = log(mpg), data = mtcars)
fit3 <- lm(log(mpg) ~ log(hp), mtcars)
summary(fit3)
hist(fit3$residuals) # тоже нормальные остатки
shapiro.test(fit3$residuals)

# Проблема гетероскедастичности

library(dplyr)
diamonds_2 <- sample_n(diamonds, 500)
qplot(x = price, y = carat, data = diamonds_2) +
  geom_smooth(method = lm)
fit1 <- lm(carat ~ price, diamonds_2)
shapiro.test(fit1$residuals)
coefficients(fit1)
plot(fit1)
summary(fit1)

# Гетероскедастичность vs гомоскедастичность
qplot(fit1$residuals, diamonds_2$price)
summary(aov(carat ~ price, diamonds_2))
summary(lm(fit1$residuals ~ diamonds_2$price))
summary(lm((fit1$residuals)^2 ~ diamonds_2$price))
summary(lm(price ~ carat, diamonds_2))

artdata <- data.frame(x = seq(10, 5000, length.out = 500))
artdata$y <- artdata$x*0.05 + rnorm(500)
qplot(x = x, y = y, data = artdata)
fit2 <- lm(y ~ x, artdata)
summary(fit2)
summary(aov(y ~ x, artdata))
summary(lm(fit2$residuals ~ artdata$x))
summary(lm(fit2$residuals^2 ~ artdata$x))
summary(lm(x ~ y, artdata))

qplot(x = log(price), y = log(carat), data = diamonds_2) +
  geom_smooth(method = lm)

library(lmtest)
bptest(fit1) # тест на гетероскедастичность
fit3 <- lm(log(carat) ~ log(price), diamonds_2)
bptest(fit3) 
shapiro.test(fit3$residuals)
plot(fit3)

# Мультиколлинеарность

library(dplyr)
set.seed(42)
d4 <- data_frame(y = rnorm(30), x_1 = rnorm(30), x_2 = x_1, x_3 = rnorm(30))
pairs(d4)
fit <- lm(y ~ ., d4)
summary(fit)

head(cars)
qplot(x = speed, y = dist, data = cars)
fit_1 <- lm(dist ~ speed, cars)
summary(fit_1)

cars <- mutate(cars, speed_2 = speed^2, speed_3 = speed^3)
pairs(cars)
fit_2 <- lm(dist ~ ., cars)
str(summary(fit_2))

head(swiss)
fit_1 <- lm(Fertility ~ ., swiss)
summary(fit_1)
cor.test(~ Fertility + Examination, swiss)
vif(fit_1)
fit_2 <- lm(Fertility ~ ., select(swiss, -Examination))
summary(fit_2)
vif(fit_2)

# Задачи

# 1
NA_position <- function(x, y) {
  return(all(is.na(x) == is.na(y)))
}

# 2
hetero_test <-  function(dataset){
  mainfit <- lm(dataset[, 1] ~ ., dataset[-1])  
  auxfit <- lm(mainfit$residuals^2 ~ ., dataset[-1])
  return(summary(auxfit)$r.squared)
}
hetero_test(mtcars)

# 3
VIF <-  function(dataset){
  vifs <- sapply(names(dataset[-1]), function(i) {
    1/(1 - summary(lm(as.formula(paste0(i, " ~ .")), dataset[-1]))$r.squared)
  })
  return(vifs)
}
VIF(mtcars)

# 4
smart_model <-  function(dataset){
  VIF <-  function(dataset){
    sapply(names(dataset[-1]), function(i) {
      1/(1 - summary(lm(as.formula(paste0(i, " ~ .")), dataset[-1]))$r.squared)
    })
  }
  vifs <- VIF(dataset)
  while (any(vifs > 10)) {
    dataset <- dataset[-(which.max(vifs) + 1)]
    vifs <- VIF(dataset)
  }
  lm(dataset[[1]] ~ ., dataset[-1])$coefficients
}
smart_model(mtcars)

# 5
transform_x <-  function(dataset){
  lba <- seq(from = -2, to = 2, by = 0.1)
  corcfs <- sapply(lba, function(i) {
    if (i != 0) {return(cor(i*dataset$x^i/abs(i), dataset$y))}
    else {return(cor(log(dataset$x), dataset$y))}
  })
  px <- lba[which.max(abs(corcfs))]
  if (px != 0) {return(px*dataset$x^px/abs(px))}
  else {return(log(dataset$x))}
}
set.seed(42)
test_data <- data_frame(y = rnorm(10, 10, 1), x = rnorm(10, 10, 1))
transform_x(test_data)

#### Основы статистики-III Module 2 ####

# Смешанные регрессионные модели

library(lme4)
library(mlmRev)
library(ggplot2)
data("Exam")
str(Exam)

ggplot(data = Exam, aes(x = standLRT, y = normexam)) +
  geom_point()
ggplot(data = Exam, aes(x = standLRT, y = normexam, col = school)) +
  geom_point()

# Один главный эффект
Model1 <- lm(normexam ~ standLRT, data = Exam)
summary(Model1)
Exam$Model1_pred <- predict(Model1)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) +
  geom_point() +
  geom_line(data = Exam, aes(x = standLRT, y = Model1_pred),
            col = "blue")

# Главный эффект + случайный свободный член
Model2 <- lmer(normexam ~ standLRT + (1 | school), data = Exam)
summary(Model2)
Exam$Model2_pred <- predict(Model2)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) +
  geom_point(alpha = 0.2) +
  geom_line(data = Exam, aes(x = standLRT, y = Model2_pred, col = school))

# Главный эффект + случайный свободный член + случайный угловой коэффициент
Model3 <- lmer(normexam ~ standLRT + (1 + standLRT | school), data = Exam)
summary(Model3)
Exam$Model3_pred <- predict(Model3)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) +
  geom_point(alpha = 0.2) +
  geom_line(data = Exam, aes(x = standLRT, y = Model3_pred, col = school))

# Главный эффект + случайный угловой коэффициент
Model4 <- lmer(normexam ~ standLRT + (0 + standLRT | school), data = Exam)
summary(Model4)
Exam$Model4_pred <- predict(Model4)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) +
  geom_point(alpha = 0.2) +
  geom_line(data = Exam, aes(x = standLRT, y = Model4_pred, col = school))

# Нескоррелированные случайные эффекты
Model5 <- lmer(normexam ~ standLRT + (1 | school) + (0 + standLRT | school),
               data = Exam)
summary(Model5)
Exam$Model5_pred <- predict(Model5)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) +
  geom_point(alpha = 0.2) +
  geom_line(data = Exam, aes(x = standLRT, y = Model5_pred, col = school))
# Чтобы указать, что коэффициенты не коррелируют можно также использовать ||:
(1 + standLRT || school)

# Статистическая значимость, обобщённые модели и случайные эффекты

# Сравнение моделей
Model2 <- lmer(normexam ~ standLRT + (1 | school), REML = FALSE, data = Exam)
summary(Model2)
Model0 <- lmer(normexam ~ 1 + (1 | school), REML = FALSE, data = Exam)
summary(Model0)
anova(Model0, Model2)

# p-значения
library(lmerTest)
Model2 <- lmer(normexam ~ standLRT + (1 | school), REML = FALSE, data = Exam)
summary(Model2)

# Обобщённые смешанные модели
Exam$school_type <- ifelse(Exam$type == 'Mxd', 1, 0)
Model6 <- glmer(school_type ~ normexam + (1 | school), family = "binomial",
                data = Exam)
summary(Model6)

# Предсказание на новых датасетах
predict(Model2, Exam)
new_Exam <- Exam[sample(1:nrow(Exam), 100), ]
new_Exam$school <- sample(101:200)
predict(Model2, new_Exam, allow.new.levels = TRUE)

# Исследование случайных эффектов
fixef(Model3)
ranef(Model3)

# Задачи

# 1
exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
str(exp_data)
library(ggplot2)
plot_1 <- ggplot(exp_data, aes(x = factor(scenario), y = frequency, fill = attitude)) +
  geom_boxplot()
plot_1

# 2
plot_2 <- ggplot(exp_data, aes(x = frequency, fill = subject)) +
  geom_density(alpha = 0.5) +
  facet_grid(gender ~ .)

# 3
library(lme4)
fit_1 <- lmer(frequency ~ attitude + (1 | subject) + (1 | scenario), data = exp_data)

# 4
fit_2 <- lmer(frequency ~ attitude + gender + (1 | subject) + (1 | scenario),
              data = exp_data)

# 5 
fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude | subject)
              + (1 + attitude | scenario), data = exp_data)

#### Основы статистики-III Module 3 ####

# Jackknife - исправление оценки

bad_var_estimator <- function(x) {
  n <- length(x)
  return(var(x) * (n-1)/n) # испортили исправленную оценку дисперсии
}

JN_bias_correction <- function(x, estimator) {
  n = length(x)
  theta_stars <- vector("numeric", n)
  ind <- 1:n
  for (i in ind) {
    samplen <- x[ind != i]
    theta_stars[i] <- estimator(samplen)
  }
  theta_hat <- estimator(x) # смещённая оценка 
  theta_dot <- mean(theta_stars)
  
  bias_jack <- (theta_dot - theta_hat) * (n - 1)
  theta_hat_jack <- theta_hat - bias_jack
  return(theta_hat_jack)
}

start <- 3
sample_sizes <- start:50
tests <- 100
results_good <- sample_sizes
results_bad <- sample_sizes
results_corrected <- sample_sizes

for (n in sample_sizes) {
  samples <- matrix(rnorm(n*tests), n)
  good_estimations <- apply(samples, 2, var)
  bad_estimations <- apply(samples, 2, bad_var_estimator)
  corrected_estimations <- apply(samples, 2, JN_bias_correction,
                                 estimator = bad_var_estimator)
  results_good[n - start + 1] <- mean(good_estimations)
  results_bad[n - start + 1] <- mean(bad_estimations)
  results_corrected[n - start + 1] <- mean(corrected_estimations)
}

simdataset <- data.frame(x = rep(sample_sizes, 3),
                         y = c(results_good, results_bad, results_corrected),
                         gr = factor(rep(1:3, each = length(sample_sizes)),
                                     labels = c("results_good", "results_bad",
                                                "results_corrected")))
library(ggplot2)
ggplot(simdataset, aes(x, y, col = gr)) +
  geom_jitter()

start <- 2
sample_sizes <- start:100
tests <- 100
results <- sample_sizes
results_corrected <- sample_sizes

for (n in sample_sizes) {
  samples <- matrix(runif(n*tests, max = 1), n)
  estimations <- apply(samples, 2, max)
  corrected_estimations <- apply(samples, 2, JN_bias_correction,
                                 estimator = max)
  results[n - start + 1] <- mean(estimations)
  results_corrected[n - start + 1] <- mean(corrected_estimations)
}

simdataset2 <- data.frame(sample_size = rep(sample_sizes, 2),
                          mean_estimated_theta = c(results, results_corrected),
                          gr = factor(rep(1:2, each = length(sample_sizes)),
                                      labels = c("results", "results_corrected")))
ggplot(simdataset2, aes(sample_size, mean_estimated_theta, col = gr)) +
  geom_jitter()

# Bootstrap

# tasks

# 1
y <- rnorm(100)
median_cl_boot <- function(x) {
  mdn <- median(x)
  bmdn <- apply(replicate(1000, sample(x, replace = TRUE)), 2, median)
  quantile(mdn - bmdn, probs = c(.025, .975)) + mdn
}
median_cl_boot(y)

# 2
slope_cl_boot <- function(dataset) {
  slopec <- lm(y ~ x, dataset)$coefficients[2] 
  bslopec <- replicate(1000, lm(y ~ x, dataset[sample(nrow(dataset), replace = TRUE), ])$coefficients[2])
  quantile(slopec - bslopec, probs = c(.025, .975)) + slopec
}
test_data <- data.frame(x = rnorm(100), y = rnorm(100, 1.5, 3))
slope_cl_boot(test_data)
