#INSTALL NEEDED PACKAGES
install.packages("caret")
install.packages("MASS")
install.packages("ggplot2")

#IMPORT DATA FROM FILE
#WILL NEET TO EDIT FILE PATH NAME
data <-
  read.csv("C:/Users/parke/Downloads/Spam Data.csv",
           header = FALSE,
           sep = ";")
cols <-
  read.csv("C:/Users/parke/Downloads/Spam Names.csv",
           header = FALSE,
           sep = ";")
names(data) <-
  sapply((1:nrow(cols)), function(i)
    toString(cols[i, 1]))

#VIEW DATA
str(data)
dim(data)
summary(data)

#UPDATE CLASSIFICATION DATA TYPE TO FACTOR
data$y <- as.factor(data$y)

#SPLIT OUT TEST AND TRAIN DATASETS
sepIndex <- sample(nrow(data), 4000)
train <- data[sepIndex, ]
test <- data[-sepIndex, ]

#CHECK FOR CLASS CONSISTENCY BETWEEN TEST AND TRAIN SETS
nrow(subset(data, y == 0)) / nrow(data)
nrow(subset(train, y == 0)) / nrow(train)
nrow(subset(test, y == 0)) / nrow(test)

#LOGISTIC REGRESSION MODEL, PREDICTION, ACCURACY
library(caret)

set.seed(1)
logFit <- glm(y ~ ., data = train, family = binomial())
test$logProbs <- predict(logFit, newdata = test, type = "response")
test$logPred <- ifelse(test$logProbs >= 0.50, 1, 0)
table(test$logPred, test$y)
logAccuracy = sum(test$logPred == test$y) / nrow(test)
logPseudR2 = (logFit$null.deviance - logFit$deviance) / logFit$null.deviance

#TEST OTHER THRESHOLD VALUES FOR LOGISTIC REGRESSION

test$logPred <- ifelse(test$logProbs >= 0.60, 1, 0)
table(test$logPred, test$y)
test$logPred <- ifelse(test$logProbs >= 0.70, 1, 0)
table(test$logPred, test$y)
test$logPred <- ifelse(test$logProbs >= 0.97, 1, 0)
table(test$logPred, test$y)

#LDA AND QDA

library(MASS)
set.seed(2)
ldaFit <- lda(y ~ ., data = train)
test$ldaPred <-
  predict(ldaFit, newdata = test, type = 'response')$class
table(test$ldaPred, test$y)
ldaAccuracy = sum(as.factor(test$ldaPred) == test$y) / nrow(test)

set.seed(3)
qdaFit <- qda(y ~ ., data = train[-nearZeroVar(train)])
test$qdaPred <-
  predict(qdaFit, newdata = test[-nearZeroVar(test)], type = 'response')$class
table(test$qdaPred, test$y)
qdaAccuracy = sum(as.factor(test$qdaPred) == test$y) / nrow(test)

#KNN

set.seed(4)
control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(k = c(2:40))
knnFit <-
  train(
    y ~ .,
    data = train,
    method = "knn",
    tuneGrid = grid,
    trControl = control
  )
test$knnPred <- predict(knnFit, newdata = test, type = "raw")
table(test$knnPred, test$y)
knnAccuracy = sum(as.factor(test$knnPred) == test$y) / nrow(test)

##### GRAPHICS #####
plot(knnFit)

plot(
  as.numeric(train$y) - 1 ~ train$word_freq_internet ,
  data = train ,
  xlab = "Character Frequency !" ,
  ylab = "Probability of Spam Email",
  ylim = c(0:1),
  xlim = c(0, 1)
)
lines(as.numeric(test$y) - 1 ~ test$logProbs,
      type = "l",
      col = "blue")

library(ggplot2)

ggplot(test, aes(x = word_freq_internet, y = y - 1)) + labs(x = "Word Frequency Internet", y = "Class") + ylim(0, 1) + xlim(0, 1) +
  geom_point() +
  stat_smooth(data = test, aes(x = logProbs, y = y - 1)) +
  ggtitle("Logistic Fit on Spam Data")
