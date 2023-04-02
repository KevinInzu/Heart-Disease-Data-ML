install.packages("ROCR")
install.packages("dplyr")
install.packages("class")
install.packages("e1071")

library("ROCR")
library("dplyr")
library("class")
library("e1071")
#Using Processed Cleveland data, is classifcation
hdd <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"), header=FALSE)

#Switch Col's
hdd <- hdd[ , c("V1","V9","V3","V4","V5","V6","V7","V8","V2","V10","V11","V12","V13","V14")]

hdd$V9[hdd$V9== 0] <- "no"
hdd$V9[hdd$V9 == 1] <- "yes"

#Preprocess data:
#Normalize: We do not know if the data is within a distribution, and the algorithms we use do not make assumptions
#In order to make proper predictions, we must have similar levels across, that's why I removed V1, otherwise age will have to many levels
hdd <- subset(hdd,select = c(-V1))
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#get x by removing the diagnostic attribute(want predictors only), and binary values
#Binary values in this case, are as used true/false and indicating status, don't help in normalization as it is only intenger based
hdd.x <-subset(hdd,select = c(-V14,-V2,-V9,-V11,-V12,-V13))
#repeatedly apply the normalize function to all cols in x
hdd_x.normalized = as.data.frame(lapply(hdd.x,normalize))

#Split data into training sets 80% for training 20% testing
train.x <- hdd_x.normalized[1:242, ]
test.x <- hdd_x.normalized[243:303,]
train.y <- hdd[1:242, 1]
test.y <- hdd[243:303,1]

dat <- data.frame (
  x = train.x,
  y = as.factor(train.y)
)
set.seed(1)
#Tune out function, by default uses 10 fold Cross Validation using various cost
tune.out <- tune(svm, y ~., data = dat, kernel = "radial", range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10,100)))
#Choose out the most optimized model given the various cost parameters
out <- tune.out$best.model
summary(out)

dat.test <- data.frame(
  x = test.x,
  y = as.factor(test.y)
)

pred.test <- predict(out, dat.test)
table(pred.test,dat.test$y)


#ROC
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
#Choosing the first class which is "no" given by the negative value thats tuned
fitted <- attributes(
  predict(out,dat.test, decision.values = T)
)$decision.values
rocplot(-fitted, dat.test$y, main = "Training Data for class 1", col = "red")
#Choosing the first class which is "yes" given by the negative value thats tuned
rocplot(fitted, dat.test$y, main = "Training Data for class 2", col = "blue")

