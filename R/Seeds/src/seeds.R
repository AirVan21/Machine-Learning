#' ---
#' title: "Seeds Data Set"
#' author: "Ivan Abramov"
#' ---

library(lattice)
library(latticeExtra)
library(gridExtra)
library(ggplot2)
library(e1071)
library(nnet)
library(MASS)
library(scales)
library(klaR)

seedsData <- read.csv("~/GitHub/StatLearning/HW4.Seeds/data/seeds_dataset.txt", sep = " ", 
                      comment.char = "#")
seedsData$type <- factor(seedsData$type, levels = c("1", "2", "3"), labels = c("Kama", "Rosa", "Canadian")) 

# Зерна пшеницы; 3 возможных класса (Kama, Rosa, Canadian); по 70 экземпляров в классе; 
# 7 quantitative-признаков (геометрические параметры зерен)
summary(seedsData)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

seedsData[1:7] <- as.data.frame(lapply(seedsData[1:7], normalize))

# Перемешиваем данные
set.seed(42)
seedsData <- seedsData[sample(nrow(seedsData)), ]

# Работаем с нормальизованными данными
summary(seedsData)

# Сильно ли пересекаются классы?
#
# Кажется, что в данной ситуации мы имеем случай, схожий с данными iris, поэтому зерна, 
# принадлежашие различным классам, можно будет успешно разделить

splom(subset(seedsData, select = c(area, perimeter, compactness, type)), 
      col = seedsData$type)
splom(subset(seedsData, select = c(length, width, assymmetry, groove, type)), 
      col = seedsData$type)

ggplot(seedsData, aes (x = width, y = length, colour = type)) + stat_density2d ()
ggplot(seedsData, aes (x = area, y = assymmetry, colour = type)) + stat_density2d ()

p1 <- densityplot(~area, groups = type,  data = seedsData)
p2 <- densityplot(~compactness, groups = type, data = seedsData)
p3 <- densityplot(~assymmetry, groups = type, data = seedsData)
p4 <- densityplot(~groove, groups = type, data = seedsData)

grid.arrange(p1, p2, p3, p4)


# Для вычисления значений некоторых предикторов используются другие предикторв
# (например, compactness вычисляется через area и perimeter), поэтому смотрим корреляцию предикторов
#
# Кажется, что можно спокойно исключить что-то из 5-ки (length, width, area, perimeter, compactness)
cor(subset(seedsData, select=-type))

# Так как количество классов пшеницы > 2, то для анализа воспользуемся lda и qda
ld_full  <- lda(type ~ ., data = seedsData)
ld_full

plot(ld_full, panel = function(x, y, ...) {points(x, y, ...)}, 
     col = as.integer(seedsData$type), pch = 20)

# Оставим теперь только lenght и width из 5-ки (length, width, area, perimeter, compactness)
# так как через них пересчитываются отсальные предикторы
ld_small <- lda(type ~ width + length + assymmetry + groove, data = seedsData)
ld_small

plot(ld_small, panel = function(x, y, ...) { points(x, y, ...)}, 
     col = as.integer(seedsData$type), pch = 20,  ylim=c(-4,4), xlim=c(-5,5))

partimat(type ~ width + length + assymmetry + groove, data=seedsData, method="lda")

summary(ld_small)

qda_small <- qda(type ~ width + length + assymmetry + groove, data = seedsData)

partimat(type ~ width + length + assymmetry + groove, data=seedsData, method="qda")

summary(qda_small)

simple.predict.da <- function(...) predict(...)$class
# Предположение о том, что можно исключить зависимые предикторы: area, perimeter, compactness
# оказалось разумным
#
# У нас 210 записей, оставляем примерно 50 событий для testset в 4-fold cross-validation
#
# Учитывая, что 1 ошибка дает 2%, то результат полученный на lda и qda сопоставим
tune(lda, type ~ ., data = seedsData, predict.func = simple.predict.da, 
     tunecontrol = tune.control(sampling = "cross", cross = 4))

tune(lda, type ~ width + length + assymmetry + groove, data = seedsData, predict.func = simple.predict.da, 
           tunecontrol = tune.control(sampling = "cross", cross = 4))

tune(qda, type ~ area + length + assymmetry + groove, data = seedsData, predict.func = simple.predict.da, 
     tunecontrol = tune.control(sampling = "cross", cross = 4))


# Попробуем использовать multinomial regression
mln <- multinom(type ~ ., data = seedsData, trace = FALSE)
summary(mln)
# Результат чуть хуже, чем тот, кторый мы получили используя lda
tune(multinom, type ~ ., data = seedsData, 
     tunecontrol = tune.control(sampling = "cross", cross = 4), trace = FALSE)

# Смотрим, какой набор предикторов предложит оставить stepAIC
#
# Оставляем (area, length, assymmetry, groove)
stepAIC(mln)

# Получаем сопоставимый с lda результат
mln_short <- multinom(type ~ width + length + assymmetry + groove, data = seedsData, trace = FALSE)
tune(multinom, type ~ width + length + assymmetry + groove, data = seedsData, 
     tunecontrol = tune.control(sampling = "cross", cross = 4), trace = FALSE)

# Результаты Naive Bayes незначительно хуже (выборка небольшая, поэтому каждый misclassification значительно портит error)
nb <- naiveBayes(type ~ ., data = seedsData)
tune(naiveBayes, type ~ ., data = seedsData, tunecontrol = tune.control(sampling = "cross", cross = 4))
tune(naiveBayes, type ~ width + length + assymmetry + groove, data = seedsData, 
     tunecontrol = tune.control(sampling = "cross", cross = 4))

# Итоговая проверка для lda
tune(lda, type ~ width + length + assymmetry + groove, data = seedsData, 
     predict.func = function(...) predict(...)$class, tunecontrol = tune.control(sampling = "fix", fix = 2/3))

# Итоговая проверка для qda
tune(qda, type ~ width + length + assymmetry + groove, data = seedsData, 
     predict.func = function(...) predict(...)$class, tunecontrol = tune.control(sampling = "fix", fix = 2/3))

# Кратко:
#
# 1. Dataset seeds содержит информацию о зернах пшеницы 3-х видов
# 2. По каждому наблюдению предоставлены 7 quantitative-признаков (геометрические параметры зерен)
# 3. Необходимо классифицировать (задача классификации) зерна по 3-м видам
#
# Выводы:
# 1. По построенным графикам выявили, что классы зерен разделимы
# 2. Так как предикторами являлись геометрические параметры зерен, то между предикторами наблюдались явные зависимости
# 3. В качестве основных были выбраны предикторы: width, length, assymmetry, groove (проверили разумность такого выбора)
# 4. Лучшие результаты показали модели на основе lda и qda
# 5. Для решения задачи классификации предлагается использовать qda гипотезу






