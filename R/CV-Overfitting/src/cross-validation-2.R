#' ---
#' title: "Cross Validation-2"
#' author: "Ivan Abramov"
#' ---

library(e1071)

generateData <- function(numOfPredictors, numOfPoints) {
  xData <- matrix(runif(numOfPredictors * numOfPoints, 0, 100), ncol = numOfPredictors)
  yData <- runif(numOfPoints)
  dataSet <- cbind(xData, yData)
  dataSet <- as.data.frame(dataSet)
  dataSet
}

getMostCorrPreicitorIndexes <- function(dataset, numOfPredictors) {
  xData <- subset(dataset, select = -yData)
  corrResults <- sort(sapply(xData, function (x) cor(x, dataset$yData)), decreasing = TRUE)
  topResults <- head(corrResults, numOfPredictors)
  names(topResults)
}

# 50 наблюдений, 10 000 предикторов
numOfPoints <- 50
numOfPredictors <- 10000

# Создаем набор данных для обучения lm
train <- generateData(numOfPredictors, numOfPoints)
# Создаем набор данных для validation
test  <- generateData(numOfPredictors, numOfPoints)
# Получаем 20-ку наиболее коррелирующих предикторов
corrIndexes <- getMostCorrPreicitorIndexes(train, 20)

# Выбираем train набор данных от 20-ки фиксированных предикторов для tune
train.corr <- subset(train, select = c(corrIndexes, "yData"))
# Выбираем test набор данных от 20-ки фиксированных предикторов для валидации
test.corr  <- subset(test, select = corrIndexes)
test.y     <- subset(test, select = yData)

linear <- lm(formula = yData ~ ., data = train.corr)
summary(linear)

# Замечательный результат CV
tune(lm, yData ~ ., data = train.corr, tunecontrol = tune.control(sampling = "cross", cross = nrow(train.corr)))
# Разочарование
sum((predict(linear, test.corr) - test.y) ** 2)


