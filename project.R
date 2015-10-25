library(caret)
library(randomForest)
library(rpart)
TrainingURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestingURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainData<-read.csv(url(TrainingURL),na.strings=c("NA",""),header=TRUE)
testData<-read.csv(url(TestingURL),na.strings=c("NA",""),header=TRUE)
dim(trainData)
dim(testData)
sum(complete.cases(trainData))

trainData<-trainData[,colSums(is.na(trainData))== 0]
testData<-testData[,colSums(is.na(testData))==0]
trainClean<- grepl("^X|timestamp|window", names(trainData))
trainData <- trainData[, !trainClean]
testClean<-grepl("^X|timestamp|window", names(testData))
testData <- testData[, !testClean]

set.seed(22500)
inTrain<-createDataPartition(trainData$classe,p=0.60,list=FALSE)
mytrain<-trainData[inTrain,]
mytest<-trainData[-inTrain,]
dim(mytrain)
dim(mytest)


control<-trainControl(method="cv",5)
Rfmodel<-train(classe ~.,data=mytrain,method="rf",trControl=control,ntree=250)
Rfmodel
prediction<-predict(Rfmodel,mytest)
confusionMatrix(mytest$classe,prediction)

accuracy<-postResample(prediction,mytest$classe)
accuracy

outofsamplerror<-1-confusionMatrix(mytest$classe, prediction)$overall[1]
outofsamplerror

finalresult<-predict(Rfmodel,testData)
finalresult

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(finalresult)
