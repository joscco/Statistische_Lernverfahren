############################################################################################
############################################################################################
############################################################################################
## Umgebung leeren ## 
rm(list=ls(all=TRUE))
## Pakete laden ## 
library(dplyr)
library(tidytext)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
## Daten laden ## 
#path_reviews_preprocessed  = "C:/Development_local/Uni-Projekt/Daten/reviews_preprocessed.csv"
reviews_preprocessed = read.csv('reviews_preprocessed.csv',colClasses=c("factor","character","character"))
#path_reviews  = "C:/Development_local/Uni-Projekt/Daten/reviews.csv"
reviews = read.csv('reviews.csv',colClasses=c("character","factor"))
############################################################################################
############################################################################################
############################################################################################

## create dictionary ###
dictionary1 = reviews_preprocessed %>% group_by(preprocessed_text) %>% count()
sum(dictionary1$n==1)
sum(dictionary1$n>10)

dEins = dictionary1$preprocessed_text[dictionary1$n==1]
dMehr = dictionary1$preprocessed_text[dictionary1$n>20]
countOfWords = sapply(strsplit(reviews$text, " "), length)
############################################################################################
############################################################################################
############################################################################################
## create document term matrix ## 
# add count to dataset
reviews_preprocessed$count = rep(1,nrow(reviews_preprocessed))
# transform dataset to document term matrix
dtmatrix = reviews_preprocessed %>%
  cast_dtm(doc_id, preprocessed_text, count)
dtmatrix = as.data.frame(as.matrix(dtmatrix))
dtmatrix = dtmatrix[order(as.numeric(rownames(dtmatrix))),]
dtmatrix = dtmatrix[dMehr]
dtmatrix$wordCount = countOfWords
dtmatrix$doc_id = as.factor(1:nrow(dtmatrix))
# add target variable to the document term matrix
reviews$doc_id = as.factor(1:nrow(reviews))
dtmatrix = inner_join(dtmatrix,reviews,by='doc_id')
############################################################################################
############################################################################################
############################################################################################
## split dataset in trainings- & testdataset ## 
#train_ind = createDataPartition(dtmatrix$type, p = 0.8, list = FALSE)
#sample_size <- ceiling(0.8*nrow(dtmatrix))
#set.seed(40211)
#train_ind <- sample(seq_len(nrow(dtmatrix)),size = sample_size)
#train_ind <- sort(train_ind)
train_ind <- read.csv("doc_id.csv")
train_ind <- train_ind$Resample1
train = dtmatrix[train_ind,]
test = dtmatrix[-train_ind,]
col_count = ncol(train)
x = train[,-c(col_count-2,col_count-1,col_count)]
y = train$type

model_nb = train (x,y,'nb',
  trControl=trainControl(method ='cv', number =10))
Predict_nb = predict(model_nb,newdata = test)
C = confusionMatrix(Predict_nb, test$type)

model_rpart = rpart(y~.,x,method = "class")
Predict_rpart = predict(model_rpart,newdata = test,type ="class")
C_rpart = confusionMatrix(Predict_rpart, test$type)

model_rf = randomForest(x,y,ntree = 2000)
Predict_rf = predict(model_rf,newdata = test,type ="class")
C_rf = confusionMatrix(Predict_rf, test$type)

C
C_rpart
C_rf

#write.csv(train_ind, file = "doc_id.csv")

############################################################################################
############################################################################################
############################################################################################
unsupervised = stats::kmeans(dtmatrix[,-c(col_count-2,col_count-1,col_count)], centers = 3, nstart = 100)#$cluster
undtmatrix1 = reviews_preprocessed %>%
  cast_dtm(doc_id, preprocessed_text, count)
dtmatrix1 = as.data.frame(as.matrix(dtmatrix))
dtmatrix1 = dtmatrix[order(as.numeric(rownames(dtmatrix))),]
dtmatrix1 = dtmatrix[dMehr]
dtmatrix1$wordCount = countOfWords
dtmatrix1$doc_id = as.factor(1:nrow(dtmatrix))
reviews$doc_id = as.factor(1:nrow(reviews))
dtmatrix1$type = unsupervised
train_ind = createDataPartition(dtmatrix1$type, p = 0.8, list = FALSE)
train1 = dtmatrix1[train_ind,]
test1 = dtmatrix1[-train_ind,]

col_count = ncol(train1)
x1 = train1[,-c(col_count-2,col_count-1,col_count)]
y1 = as.factor(train1$type)


# Model Creation
model_nb = train (x1,y1,'nb',
                  trControl=trainControl(method ='cv', number =10))
# Model Evaluation
Predict = predict(model_nb,newdata = test1)
#Get the confusion matrix and statistics
C1 = confusionMatrix(Predict, as.factor(test1$type))

C$table
C1$table
