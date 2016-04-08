#' ---
#' title: "Teen Gambling Data Set"
#' author: "Ivan Abramov"
#' ---

library(lattice)
library(latticeExtra)
library(gridExtra)
library(e1071)
library(MASS)

teenGamblingData <- read.csv("~/GitHub/StatLearning/HW2.Intersections/task2/teengambling.txt", sep = " ")
teenGamblingData$sex <- factor(teenGamblingData$sex, levels = c("0", "1"), labels = c("male", "female")) 


# Интерпретируем данные
#
# Выводы (предположения):
# 1) подростки мужского пола тратят больше денег на азартные игры, чем подростки женского пола 
#   (розовые точки сгруппированы в окрестности 0);
# 2) возможно, если линейная зависимость для gamble-income, так как с увеличением дохода у male увеличивается и gamble;
# 3) было бы разумно, если бы с увеличением verbal уменьшалось значение gamble (что-то похожее есть);
splom(teenGamblingData, groups = teenGamblingData$sex)
summary(teenGamblingData)

# Еще раз отмечаем тот факт, что девушки меньше тратят деньги на азартные игры
marginal.plot(subset(teenGamblingData, select=-sex), groups = teenGamblingData$sex)

# Посмотрим на параметры линейной модели
# Предположение: возможно status и verbal неинформативные
linear <- lm(gamble ~., data = teenGamblingData)
summary(linear)

p1 <- densityplot(~income, data = teenGamblingData)
p2 <- densityplot(~status, data = teenGamblingData)
p3 <- densityplot(~verbal, data = teenGamblingData)
p4 <- densityplot(~gamble, data = teenGamblingData)

# Вывод: income и gamble, скорее всего, имеют логнормальное распределение
grid.arrange(p1, p2, p3, p4)


# Корректируем модель
teenGamblingData.log <- teenGamblingData
# Заметим, что для gamble есть значения == 0, поэтому добавляем порог, чтобы log() работал корректно
teenGamblingData.log$gamble <- log(teenGamblingData$gamble + 0.01)
# Для income min > 0, поэтому проблем с log() не будет
teenGamblingData.log$income <- log(teenGamblingData$income)
summary(teenGamblingData.log)

linear.log <- lm(gamble ~., data = teenGamblingData.log)
# Заметим, что информативность status по вопросом
summary(linear.log)
# Кроме того, verbal и status сильно коррелируют, поэтому возникает желание убрать status
cor(subset(teenGamblingData, select=-sex))

# Удостоверимся, что гипотеза linear.log значительно лучше исходной гипотезы
AIC(linear, linear.log)

# Посмотим, что предложит stepAIC (особенно, нас интересует случай -status)
#
# Исключение предиктора status не дало желаемого результата. 
# Это можно объяснить тем, что данных мало, поэтому неразумно что-то исключать.
linear.log_2 <- stepAIC(linear.log)

# Проверим полученные результаты 
# Cross validation с leave-one-out, так как dataset небольшой
linear.log_minus_status <- update(linear.log, gamble ~ . - status)

tune(lm, linear.log_minus_status$call$formula, data = teenGamblingData.log, tunecontrol = tune.control(sampling = "cross", cross = nrow(teenGamblingData) - 1))
tune(lm, linear.log$call$formula, data = teenGamblingData.log, tunecontrol = tune.control(sampling = "cross", cross = nrow(teenGamblingData) - 1))

# Выводы:
# 1) Девушки значительно меньше тратят деньги на азартные игры
# 2) income и gamble разумно прологарифмирогвать, так как это денежные признаки (логнормальны)
# 3) Предикторы информативны (status рассматривался как кандидат на исключение, но так как dataset небольшой,  
#    то его исключение не приносило ощутимого результата)

