# Practical Machine learning assigment 
# Overview
  Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here:
[See the section on the Weight Lifting Exercise Dataset](http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har)
# Load library and data
The training data used is available here
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The testing data used is available here
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har.
```{r, cache=TRUE}
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
```
# Clean and explore data
```{r, results='markup'}
train<-train[,-1]
dim(train)
```
First, we remove the first column of train data frame in order to delete row 'id', and check total no. of variables which is 159 now

```{r, results='markup'}
train<-train[ , colSums(is.na(train))/dim(train)[1] < 0.95]
dim(train)
```
Then, column with higher than 95% of NA is removed, now we have only 59 variables, 100 were removed.

```{r, cache= TRUE, results='markup'}
nearVar <- nearZeroVar(train)
train <- train[, -nearVar]
dim(train)
```
The Near Zero variance (NZV) variables are also removed and the ID variables as well. We have 58 varibles at the end

# Data partition
```{r, results='markup'}
set.seed(5555)
inTrain  <- createDataPartition(train$classe, p=0.75, list=FALSE)
training <- train[inTrain, ]
testing  <- train[-inTrain, ]
testset<-testset[, colnames(testset) %in% colnames(training)]
```
We create partition of training and testing with caret package. 

# Exploratory data 
```{r,results='markup'}
#str(training)
dim(training)
#summary(testing)
dim(testing)

```

Finally, we get training set and testing set as our ready-to-process data

# Decision Tree
First, we apply Decision Tree method to training set and predict result from testing set
```{r, cache= TRUE, results='markup', fig.width= 12}
Modfit1<-train(data = training, classe~. ,  method = 'rpart'  )
pred1<-predict(Modfit1, newdata = testing)
result1<-confusionMatrix(testing$classe, pred1)
print(result1)
fancyRpartPlot(Modfit1$finalModel)


```

The result of this prediction is shown as summary, only 0.6533 accuracy, and tree representation


```{r, cache= TRUE, results='markup'}
testing$check_pred1<-pred1 == testing$classe
table(pred1,testing$classe)
qplot(y = classe, colour = check_pred1, data = testing, xlab = "observation at ith")
```
Next, we create prediction table, and graph showing FALSE and TRUE prediction compared to actual class
# RandomForest
First, we apply RandomForest method to training set and predict result from testing set
```{r, cache= TRUE, results='markup'}
Modfit2<-randomForest(classe ~ ., data=training, method= "class")
pred2<-predict(Modfit2, newdata = testing, type = 'class')
result2<-confusionMatrix(testing$classe, pred2)
print(result2)
```
This method give around 99% acccuracy which is much higher than previous method.

```{r, cache= TRUE, results='markup'}
testing$check_pred2<-pred2 == testing$classe
table(pred2,testing$classe)
qplot(y = classe, colour = check_pred2, data = testing, xlab = "observation at ith")
```
Next, we create prediction table, and graph showing FALSE and TRUE prediction compared to actual class. We barely see FALSE value in the graph

# Model testing with Model 2 (random forest)
Two accuracies from two models are shown

Random Forest : 0.999
Decision Tree : 0.6553

Thus, we apply RandomForest method to testset as it has higher accuracy

```{r, cache= TRUE, results='markup'}
dum <- head(train,1)
dum <- dum[, -length(colnames(dum))]
validate_set <- rbind(dum, testset)
validate_set <- validate_set[-1,] 
pred3<-predict(Modfit2, newdata = validate_set,type = "class")
print(pred3)
```
Use second model, randomForest, to predict the actual est data.



