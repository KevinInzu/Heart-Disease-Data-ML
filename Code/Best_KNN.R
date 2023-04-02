install.packages("ROCR")
install.packages("dplyr")
install.packages("class")

library("ROCR")
library("dplyr")
library("class")
#Using Processed Cleveland data, is classifcation
hdd <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"), header=FALSE)

hdd <- hdd[ , c("V1","V9","V3","V4","V5","V6","V7","V8","V2","V10","V11","V12","V13","V14")]

#Describe Data set clearly:

hdd$V9[hdd$V9== 0] <- "no"
hdd$V9[hdd$V9 == 1] <- "yes"

#Preprocess data:=
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
  
  #Using Cross Validation; 5 fold and KNN
  samplesize = nrow(hdd)
  chunksize = round(samplesize/5)
  cv.k = 5
  cv.error.k = rep(0, cv.k)
  all_errs = 0
  
    for (n in 1:cv.k) {
      #For the chunk taken out
      valid.start = (n-1)*chunksize + 1 
      #On the last iteration make sure the end is correct since it can get messed up during rounding
      if(n == cv.k)
      {
        valid.end = samplesize
      } else {
        valid.end = n* chunksize
        
      }
      valid_set.range <- valid.start:valid.end
      
      #split data into training and test sets
      #training the model on the data
      train.X = hdd_x.normalized[-(valid_set.range), ] #-(1:chunksize) is the other data
      test.X = hdd_x.normalized[valid_set.range , ] 
      train.Y=factor (hdd[-(valid_set.range ), 1])
      test.Y= factor (hdd[valid_set.range,1 ]) 
      set.seed(1)
      hdd.pred = knn(train.X, test.X, train.Y, 10)
      
    }
  
  table(hdd.pred,test.Y)



  