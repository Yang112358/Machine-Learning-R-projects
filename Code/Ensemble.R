library(adabag)
library(rpart) 
library(caret)
library(FNN)

bank.df <- read.csv("UniversalBank_Ensemble.csv")
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)
train.index <- sample(rownames(bank.df), dim(bank.df)[1]*0.7)  
valid.index<-setdiff(rownames(bank.df),train.index)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]
tr <- rpart(Personal.Loan ~ ., data = train.df)
pred <- predict(tr, valid.df, type = "class")
confusionMatrix(pred, valid.df$Personal.Loan)
bag <- bagging(Personal.Loan ~ ., data = train.df)
pred_bag <- predict(bag, valid.df, type = "class")
confusionMatrix(as.factor(pred_bag$class), valid.df$Personal.Loan)
boost <- boosting(Personal.Loan ~ ., data = train.df)
pred_bst <- predict(boost, valid.df, type = "class")
confusionMatrix(as.factor(pred_bst$class), valid.df$Personal.Loan)
train1.index<-sample(train.index, dim(train.df)[1]*0.5) 
train2.index<-setdiff(train.index, train1.index) 
train1.df<-train.df[train1.index,]
train2.df<-train.df[train2.index,]
prim_tree<-train(Personal.Loan~., data=train1.df, method="rpart")
prim_knn<-train(Personal.Loan~., data=train1.df, method="knn")
prim_logit<-train(Personal.Loan~., data=train1.df, method="glm",family="binomial")
tree_prediction<-predict(prim_tree,newdata=train2.df)
knn_prediction<-predict(prim_knn,newdata=train2.df)
logit_prediction<-predict(prim_logit,newdata=train2.df)
meta_data<-data.frame(tree_prediction,knn_prediction,logit_prediction,Personal.Loan=train2.df$Personal.Loan)
meta.model<-train(Personal.Loan~., data=meta_data, method="glm",family="binomial")
tree_pred_v<-predict(prim_tree,newdata=valid.df)
knn_pred_v<-predict(prim_knn,newdata=valid.df)
logit_pred_v<-predict(prim_logit,newdata=valid.df)
meta_data_v<-data.frame(tree_prediction=tree_pred_v,
                        knn_prediction=knn_pred_v,
                        logit_prediction=logit_pred_v,
                        Personal.Loan=valid.df$Personal.Loan)
meta_pred<-predict(meta.model,newdata=meta_data_v)
confusionMatrix(tree_pred_v, valid.df$Personal.Loan)
confusionMatrix(knn_pred_v, valid.df$Personal.Loan)
confusionMatrix(logit_pred_v, valid.df$Personal.Loan)
confusionMatrix(meta_pred, valid.df$Personal.Loan)
bank.df <- read.csv("UniversalBank_Ensemble.csv")
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)
set.seed(1)  
train.index <- sample(rownames(bank.df), dim(bank.df)[1]*0.6)  
valid.index <- setdiff(rownames(bank.df), train.index)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]
reg <- glm(Personal.Loan ~., data = train.df, family = "binomial")
pred_class_logit<-as.factor(ifelse(predict(reg, valid.df, type = "response")>0.5, 1, 0))
confusionMatrix(pred_class_logit, valid.df$Personal.Loan)
knn <-knn(train = train.df[, -7], test = valid.df[,-7], 
          cl = train.df[, 7], k = 3, prob=TRUE)
confusionMatrix(knn, valid.df$Personal.Loan)
tr <- rpart(Personal.Loan ~., data = train.df)
pred_class_tree<-predict(tr, valid.df,type="class")
confusionMatrix(pred_class_tree, valid.df$Personal.Loan)
res<- data.frame(Actual=valid.df$Personal.Loan, 
                 LogisticProb = predict(reg, valid.df, type = "response"), 
                 LogisticPred = pred_class_logit, 
                 KNNProb = ifelse(as.numeric(knn)-1>0,attr(knn, "prob"),1-attr(knn, "prob")), 
                 KNNPred = as.numeric(knn)-1, 
                 TREEProb = predict(tr, valid.df,type="prob")[,2], 
                 TREEPred = pred_class_tree)
res$majority.1 <-rowSums(data.frame(as.numeric(res$LogisticPred)-1, res$KNNPred, as.numeric(res$TREEPred)-1))>1
res$avg <- rowMeans(data.frame(res$LogisticProb, res$KNNProb, res$TREEProb))
confusionMatrix(as.factor(ifelse(res$majority.1==TRUE,1,0)), valid.df$Personal.Loan)
confusionMatrix(as.factor(ifelse(res$avg > 0.5,1,0)), valid.df$Personal.Loan)


