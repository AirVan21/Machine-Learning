#' ---
#' title: "Cross Validation"
#' author: "Ivan Abramov"
#' ---
displayResult = function(data)
{
  cat("Mean = ", mean(data))
  cat("\tStandard Deviation = ", sd(data))
  cat("\n")
}

crossValidation = function(parameters)
{
  output <- numeric(NUM_OF_CHUNKS)
  
  for (i in 1: NUM_OF_CHUNKS)
  {
    # Select indices for chunk which will be testing one
    chunkIndexRange <- ((i - 1) * SIZE_OF_CHUNK + 1) : (i * SIZE_OF_CHUNK) 
    
    trainDataSet <- advertDataSet[-chunkIndexRange, ]
    testDataSet  <- advertDataSet[chunkIndexRange, ]
    
    # Fitting linear model
    linearModel <- lm(formula = parameters, data = trainDataSet)
    
    correctInfo <- testDataSet$Sales
    predictInfo <- predict(linearModel, testDataSet)
    
    paramsCount <- length(coefficients(linearModel)) - 1
    output[i] <- sum((correctInfo - predictInfo) ^ 2) / (paramsCount / 3)
  }
  
  output
}

advertDataSet <- read.csv("~/GitHub/StatLearning/HW1.CrossValidation/Advertising.csv")

NUM_OF_CHUNKS <- 20
SIZE_OF_CHUNK <- dim(advertDataSet)[1] / NUM_OF_CHUNKS
advertDataSet$X <- NULL

# Shuffle data row-wise
set.seed(42)
advertDataSet <- advertDataSet[sample(nrow(advertDataSet)), ]

displayResult(crossValidation(Sales ~ TV))
displayResult(crossValidation(Sales ~ Radio))
displayResult(crossValidation(Sales ~ Newspaper))

displayResult(crossValidation(Sales ~ TV + Radio))
displayResult(crossValidation(Sales ~ TV + Newspaper))
displayResult(crossValidation(Sales ~ Radio + Newspaper))

displayResult(crossValidation(Sales ~ TV + Radio + Newspaper))

# Excluded "Newspaper" predictor

displayResult(crossValidation(Sales ~ TV + Radio * TV))
displayResult(crossValidation(Sales ~ TV + Radio + Radio * TV))

displayResult(crossValidation(Sales ~ log(TV) + Radio))
displayResult(crossValidation(Sales ~ log(TV) + Radio * TV))

displayResult(crossValidation(Sales ~ sqrt(TV) + Radio * TV))
displayResult(crossValidation(Sales ~ log(TV) + sqrt(Radio * TV)))
displayResult(crossValidation(Sales ~ log(TV) + sqrt(Radio) + Radio * TV))

displayResult(crossValidation(Sales ~ poly(log(TV), Radio * TV, degree = 2)))
displayResult(crossValidation(Sales ~ poly(sqrt(TV), sqrt(Radio * TV), degree = 2)))
displayResult(crossValidation(Sales ~ poly(log(TV), sqrt(Radio * TV), degree = 2)))
displayResult(crossValidation(Sales ~ poly(sqrt(TV), sqrt(Radio), Radio * TV, degree = 2)))

# Added "Newspaper" predictor

displayResult(crossValidation(Sales ~ poly(log(TV), Radio * TV, Newspaper, degree = 2)))
displayResult(crossValidation(Sales ~ poly(log(TV), sqrt(Radio * TV), Newspaper, degree = 2)))

displayResult(crossValidation(Sales ~ log(TV) + poly(sqrt(Radio * TV), Newspaper, degree = 2)))
displayResult(crossValidation(Sales ~ log(TV) + poly(Radio * TV, degree = 2)))

# Box plot examples

boxplot(crossValidation(Sales ~ TV + Radio + Newspaper))
boxplot(crossValidation(Sales ~ log(TV) + sqrt(Radio) + Radio * TV))
boxplot(crossValidation(Sales ~ poly(log(TV), sqrt(Radio * TV), Newspaper, degree = 2)))
