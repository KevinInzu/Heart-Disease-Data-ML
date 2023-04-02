install.packages("ROCR")
install.packages("dplyr")
install.packages("class")
install.packages("writexl")

library("ROCR")
library("dplyr")
library("class")
library("writexl")
#Using Processed Cleveland data, is classifcation
hdd <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"), header=FALSE)

hdd <- hdd[ , c("V1","V9","V3","V4","V5","V6","V7","V8","V2","V10","V11","V12","V13","V14")]

#Describe Data set clearly:

colnames(hdd) <- c('age','exang','cp','trestbps','chol','fbs','restecg','thalach','sex','oldpeak','slope','ca','thal','num')

hdd$sex[hdd$sex == 0] <- "female"
hdd$sex[hdd$sex == 1] <- "male"

hdd$cp[hdd$cp == 1] <- "typical angina"
hdd$cp[hdd$cp == 2] <- "atypical angina"
hdd$cp[hdd$cp == 3] <- "non-anginal pain"
hdd$cp[hdd$cp == 4] <- "asymptomatic"

hdd$fbs[hdd$fbs == 0] <- "false"
hdd$fbs[hdd$fbs == 1] <- "true"

hdd$restecg[hdd$restecg == 0] <- "Normal"
hdd$restecg[hdd$restecg == 1] <- "Abnormality"
hdd$restecg[hdd$restecg == 2] <- "probable or definite"

hdd$exang[hdd$exang== 0] <- "no"
hdd$exang[hdd$exang == 1] <- "yes"

hdd$slope[hdd$slope == 1] <- "upslopping"
hdd$slope[hdd$slope == 2] <- "flat"
hdd$slope[hdd$slope == 3] <- "downslopping"

hdd$thal[hdd$thal == 3] <- "normal"
hdd$thal[hdd$thal == 6] <- "fixed defect"
hdd$thal[hdd$thal == 7] <- "reversable defect"


hdd_named <-subset(hdd,select = c(-age,-num,-sex,-exang,-slope,-ca,-thal))

#export as excel
write_xlsx(hdd_named, "C:\\Users\\Kevin\\Code\\Course Project\\table_data.xlsx")