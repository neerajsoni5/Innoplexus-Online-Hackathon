library(data.table)
library(readr)

library(e1071)
path<-"C:/Users/SoniNe02/Downloads/innoplex"

################################################################################################################3

library(caret)
library(mlbench)
path<-"C:/Users/SoniNe02/Downloads/innoplex"

test<-read.csv(file.path(path,"test_nvPHrOx.csv"))
#train_html<-fread(file.path(path,"train/html_data.csv"))
#train_htm<-fread("C:/Users/SoniNe02/Downloads/innoplex/train/html_data.csv")
train<-read.csv("C:/Users/SoniNe02/Downloads/innoplex/train/train.csv")

#test$Tag<-""

#train$sourcce<-"train"
#test$sourcce<-"test"


x = train[,-4]
y = train$Tag

fit3 <- knn3(Tag ~ ., data=train)

model = train(x,y,'nb',trControl=trainControl(method='cv'))

predictions1 <- predict(model,test,type="raw")

test$Tag<-predictions1

Sample<-test[,c("Webpage_id","Tag")]
write.csv(Sample,file.path(path,"SampleP_v2.csv"),row.names = F)
############################################################################################33
##SVM##############
# load libraries
library(caret)
library(mlbench)
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.svmRadial <- train(Tag~., data=train, method="svmRadial", metric="Accuracy", trControl=control)
# summarize fit
print(fit.svmRadial)



########Convert Factors
combine<-Comb[,c(1:3)]
combineDummy <- dummyVars("~.",data=combine, fullRank=T)
combineDF <- as.data.frame(predict(combineDummy,combine))
print(names(combineDF))

Comb<-rbind(train,test)


########Convert Factors
combineDummy <- dummyVars("~.",data=Comb[,c(1:3)], fullRank=F)
combineDF <- as.data.frame(predict(combineDummy,Comb))
print(names(combineDF))

dim(train)

# list types for each attribute
sapply(train, class)
# list the levels for the class
levels(train$Tag)





# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
1
2
3
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

model = train(x,y,'nb',trControl=trainControl(method='cv'))

set.seed(7)
fit.knn <- train(x,y, method="knn",trControl=control)

set.seed(7)
fit.knn <- train(Tag~., data=train, method="knn", metric=metric, trControl=control)

# a) linear algorithms
set.seed(7)
fit.lda <- train(Tag~., data=train, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Tag~., data=train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Tag~., data=train, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Tag~., data=train, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Tag~., data=train, method="rf", metric=metric, trControl=control)





