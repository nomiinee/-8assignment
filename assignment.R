url_train<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url_train, destfile = "train.csv")
download.file(url_test, destfile = "test.csv")
train<-read.csv("train.csv", header = TRUE, na.strings=c("NA","#DIV/0!",""))
testset<-read.csv("test.csv", header = TRUE, na.strings=c("NA","#DIV/0!",""))
library(caret)
library(randomForest)
library(rpart)
library(rattle)
library(ggplot2)
#Load and Clean data
train<-train[,-1]
train<-train[ , colSums(is.na(train))/dim(train)[1] < 0.95]
nearVar <- nearZeroVar(train)
train <- train[, -nearVar]

set.seed(5555)
inTrain  <- createDataPartition(train$classe, p=0.75, list=FALSE)
training <- train[inTrain, ]
testing  <- train[-inTrain, ]
dim(training)
dim(testing)

testset<-testset[, colnames(testset) %in% colnames(training)]

#Exploratory data 
summary(training)
summary(testing)
dim(training)
dim(testing)
#testset$classe<-NULL



#Decision Tree
Modfit1<-train(data = training, classe~. ,  method = 'rpart'  )
Modfit1$
pred1<-predict(Modfit1, newdata = testing)
fancyRpartPlot(Modfit1$finalModel)
result1<-confusionMatrix(testing$classe, pred1)
print(result1)
testing$check_pred1<-pred1 == testing$classe
table(pred1,testing$classe)
qplot(y = classe, colour = check_pred1, data = testing, xlab = "observation at ith")

#RandomForest
Modfit2<-randomForest(classe ~ ., data=training, method= "class")
pred2<-predict(Modfit2, newdata = testing, type = 'class')
result2<-confusionMatrix(testing$classe, pred2)
print(result2)
testing$check_pred2<-pred2 == testing$classe
table(pred2,testing$classe)
qplot(y = classe, colour = check_pred2, data = testing, xlab = "observation at ith")
#Model testing with Model 2 (random forest)
dum <- head(train,1)

dum <- dum[, -length(colnames(dum))]

validate_set <- rbind(dum, testset) #add first row of training set to validation set, it somehow make column class same as testing and training sets
validate_set <- validate_set[-1,] #remove first row we added previously
pred3<-predict(Modfit2, newdata = validate_set,type = "class")
print(pred3)




