#' ---
#' title: "MNIST Data Set"
#' author: "Ivan Abramov"
#' ---

library("e1071")
library("MASS")
library("class")
library("caret")

deskew <- function(frame) {
  start <- 1
  end <- ncol(frame)
  
  for (i in start:end) {
    minimum <- min(frame[[i]])
    frame[[i]] <- log(1 + frame[[i]] - minimum)
  }
  
  frame
}

load("~/GitHub/Machine-Learning/R/MNIST/data/mnist.rda")

# Делаем deskewing, используя log()  
mnist.train.data <- deskew(subset(mnist.train, select = -y))
mnist.test.data  <- deskew(subset(mnist.test, select  = -y))

# Убираем признаки с малой дисперсией
low_var_col <- nearZeroVar(mnist.train.data, saveMetrics = T)
mnist.train.data <- mnist.train.data[ ,!low_var_col$nzv]
mnist.test.data  <- mnist.test.data[  ,!low_var_col$nzv]

# Считаем pca
pca <- prcomp(mnist.train.data, center = TRUE, scale = TRUE)

# Считаем, сколько информации сохраним, если возьмем 85 предикторов
variance <- pca$sdev^2 / sum(pca$sdev ^ 2)
variance <- data.frame(PC = 1:249, Var = variance * 100)
cat("Variance = ", sum(variance$Var[1:85]))

mnist.train.pca <- predict(pca, newdata = mnist.train.data)[, 1:85]
mnist.test.pca  <- predict(pca, newdata = mnist.test.data) [, 1:85]

# Запускаем knn, где k = 3
knn_predict <- knn(train = mnist.train.pca, cl = mnist.train$y, test = mnist.test.pca, k = 3)
cat("Score = ", mean(knn_predict == mnist.test$y))





