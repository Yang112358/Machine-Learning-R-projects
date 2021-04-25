# import libraries
library(FNN)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
df<-read.csv("RPractice.csv")
train.index <- sample(rownames(df), dim(df)[1]*0.6) 
valid.index<-setdiff(rownames(df), train.index)
train.df <- df[train.index, ]
valid.df <- df[valid.index, ]
train.norm.df <- train.df
valid.norm.df <- valid.df
norm.values <- preProcess(train.df[, 2:6], method=c("center", "scale"))
train.norm.df[, 2:6] <- predict(norm.values, train.df[, 2:6])
valid.norm.df[, 2:6] <- predict(norm.values, valid.df[, 2:6])
accuracy.df <- data.frame(k = seq(10, 30, 1), accuracy = rep(0, 21))
for(i in 10:30) {
  knn.pred <- knn(train.norm.df[, 2:6], valid.norm.df[, 2:6], 
                  cl = train.norm.df[, 1], k = i)
  accuracy.df[i-9, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 1])$overall[1] 
}
knn.k21 <- knn(train.norm.df[, 2:6], valid.norm.df[, 2:6], 
                cl = train.norm.df[, 1], k = 21)
confusionMatrix(knn.k21, valid.norm.df[, 1])
train.factor.df <- train.df
valid.factor.df <- valid.df
for (i in 2:6){
  train.factor.df[,i] <- as.factor(train.factor.df[,i])
  valid.factor.df[,i] <- as.factor(valid.factor.df[,i])
}
nb <- naiveBayes(VALUE ~ ., data = train.factor.df)
confusionMatrix(predict(nb, valid.factor.df),valid.factor.df$VALUE)
ct <- rpart(VALUE ~ ., data = train.df, method = "class")
prp(ct)
pred<- predict(ct,newdata=valid.df,type = "class")
confusionMatrix(pred, valid.df$VALUE)
