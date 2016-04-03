#' ---
#' title: "Teen Gambling Data Set"
#' author: "Ivan Abramov"
#' ---

library(lattice)
library(gridExtra)
library(e1071)

teenGamblingData <- read.csv("~/GitHub/StatLearning/HW2.Intersections/task2/teengambling.txt", sep = " ")


# Постараемся интерпретировать данные
splom(teenGamblingData, groups = teenGamblingData$sex)

# Выводы (предположения):
# 1) подростки мужского пола тратят больше денег на азартные игры, чем подростки женского пола (розовые точки сгруппированы в окрестности 0);
# 2) возможно, если линейная зависимость для gamble-income, так как с увеличением дохода у male увеличивается и gamble
# 3) было бы разумно, если бы с увеличением verbal уменьшалось знакчение gamble (что-то похожее есть)

# Посмотрим на параметры линейной модели
linear <- lm(gamble ~., data = teenGamblingData)
summary(linear)
# Вывод: возможно status и verbal не информативные

p1 <- densityplot(~income, data = teenGamblingData)
p2 <- densityplot(~status, data = teenGamblingData)

grid.arrange(p1, p2)

# Вывод: income имеет логнормальное распределение
teenGamblingData.log <- teenGamblingData
teenGamblingData.log$income <- log(teenGamblingData$income)

linear.log <- lm(gamble ~., data = teenGamblingData.log)

# Сравниваем гипотезы: linear и linear.log

# linear.log хуже
AIC(linear, linear.log)
#linear.log хуже
BIC(linear, linear.log)
#linear.log хуже
anova(linear, linear.log)

# Смотрим на параметры linear.log
summary(linear.log)
# verbal и status сильно коррелируют, возможно, стоит их убрать
cor(linear.log$model)

linear.log_minus_verbal <- update(linear.log, gamble ~ . - verbal)
linear.log_minus_status <- update(linear.log, gamble ~ . - status)

tune(lm, linear.log_minus_verbal$call$formula, data = teenGamblingData.log, tunecontrol = tune.control(sampling = "cross", cross = 10))
tune(lm, linear.log_minus_status$call$formula, data = teenGamblingData.log, tunecontrol = tune.control(sampling = "cross", cross = 10))
tune(lm, linear.log$call$formula, data = teenGamblingData.log, tunecontrol = tune.control(sampling = "cross", cross = 10))

# Вывод: так как данных мало, то исключая признаки мы только ухудшаем модель
# Получается, что исходная гипотеза linear была лучшей

