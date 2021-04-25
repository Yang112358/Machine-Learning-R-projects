library(neuralnet)
library(dummies)
library(caret)
library(forecast)
library(FNN)
library(e1071)
library(adabag)
library(rpart) 
library(dplyr)

Tesla.df <- read.csv("~/Downloads/TSLA.csv")
#ann
return_df=Tesla.df[2:252,c(1,2,5)]
# Day change
day_change_df = data.frame(Tesla.df$Close-return_df$Open)
return_df=data.frame(return_df)
return_df$day_change = day_change_df[1:251,1]
# Return 
return_df$Open=diff(Tesla.df$Open)
return_df$Close=diff(Tesla.df$Close)
return_df$Adj.Close=diff(Tesla.df$Adj.Close)
return_df$Volume=Tesla.df[2:252,7]
### Close return change to binary. 1 = Gain, 0 = lose 
return_df$Gain <- ifelse(return_df$Close > 0 ,1,0)
## Split data set
train.index=sample(rownames(return_df), dim(return_df)[1]*0.6)
valid.index=setdiff(rownames(return_df), train.index)
train.df<-return_df[train.index,]
valid.df<-return_df[valid.index,]
# normalize
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)
# ANN
nn1 <- neuralnet(Close ~ Open+day_change+Volume, data = train.norm.df, linear.output = TRUE, hidden =4)
plot(nn1)
# Predict the close of that day based on the open
valid.prediction1= neuralnet::compute(nn1, valid.norm.df)
# rmse and other measures
accuracy(valid.prediction1$net.result[,1], valid.norm.df$Close)
## Predict close price 
r_o=600.55-626.06
d_c=597.95	-626.06	
v=89300300
test.df =data.frame("Open"={r_o},"day_change"={d_c},"Volume"=v)
test.value = neuralnet::compute(nn1,test.df)
predict_close_Mar.3=as.numeric(test.value[2][1]) + 597.95		
predict_close_Mar.3

#Logit
logit.reg <- glm(Gain ~ Open+Volume+day_change, data = train.df, family = "binomial") 
summary(logit.reg)

# Predict
pred.prob<-predict(logit.reg,type="response")
train.df$predicted_prob<-pred.prob
confusionMatrix(relevel(as.factor(ifelse(train.df$predicted_prob > 0.5, "1", "0")),ref="1"), relevel(as.factor(train.df$Gain),ref="1"))

## Precict
model = 1/(1+exp(0.1324*d_c))

#plot(Gain ~ day_change, data = valid.df, 
#     col=ifelse(valid.df$Gain=="1", "red","blue"))
#legend("topright",legend=c("1","0"),pch=1,col=c("red","blue"))


#lm
lm.reg = lm(Close ~ Open+Volume+day_change, data = train.df)
summary(lm.reg)

# Predict
pred.prob<-predict(lm.reg,type="response")
train.df$predicted_prob<-pred.prob
confusionMatrix(relevel(as.factor(ifelse(train.df$predicted_prob > 0, "1", "0")),ref="1"), relevel(as.factor(train.df$Gain),ref="1"))



#knn
# Initial the dataset
# initialize normalized training, validation data, complete data frames to originals
train.stand.df <- train.df
valid.stand.df <- valid.df
return.stand.df <- return_df
# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.df[, 2:6], method=c("center", "scale"))
train.stand.df[, 2:6] <- predict(norm.values, train.df[, 2:6])
valid.stand.df[, 2:6] <- predict(norm.values, valid.df[, 2:6])
return.stand.df[, 2:6] <- predict(norm.values, return_df[, 2:6])

# To avoid the Error: `data` and `reference` should be factors with the same levels.
valid.stand.df$Gain=as.factor(valid.stand.df$Gain)  

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))


# KNN
for(i in 1:15) {
  knn.pred <- knn(train.stand.df[,2:6], valid.stand.df[, 2:6], 
                  cl = train.stand.df[, 7], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.stand.df[, 7])$overall[1] 
}
accuracy.df

## From the result k=4 gives the best.
knn.pred <- knn(train.stand.df[,2:6], valid.stand.df[, 2:6], 
                cl = train.stand.df[, 7], k = 4)
confusionMatrix(knn.pred, valid.stand.df[, 7])

#Naive bayes
### Change the dataset into category
### Close return to 1 = Gain, 0 = lose 
return_Cat_df = return_df
return_Cat_df$Close <- ifelse(return_Cat_df$Close > 0 ,1,0)
return_Cat_df$Open <- ifelse(return_Cat_df$Close > 0 ,1,0)
return_Cat_df$Adj.Close <- ifelse(return_Cat_df$Adj.Close > 0 ,1,0)
return_Cat_df$day_change <- ifelse(return_Cat_df$day_change > 0 ,1,0)
## Split data set
train.cat.index <- sample(row.names(return_Cat_df), 0.6*dim(return_Cat_df)[1])  
valid.cat.index <- setdiff(row.names(return_Cat_df), train.cat.index)  
train.cat.df <- return_Cat_df[train.cat.index, ]
valid.cat.df <- return_Cat_df[valid.cat.index, ]
# As factor
valid.cat.df$Gain=as.factor(valid.cat.df$Gain) 

# Naive Bayes
gain.nb <- naiveBayes(Gain ~Open+day_change+Volume, data = train.cat.df)


## predict probabilities
pred.prob <- predict(gain.nb, newdata = valid.cat.df, type = "raw")
pred_valid.df<-cbind(valid.cat.df,pred.prob)

## Construct a prediction dataset
colnames(pred_valid.df)[which(names(pred_valid.df)=="0")] = "l"
colnames(pred_valid.df)[which(names(pred_valid.df)=="1")] = "g"
pred_valid.df$Pre_Gain = 0
pred_valid.df$Pre_Gain <- ifelse(pred_valid.df$g > pred_valid.df$l ,1,0)
pred_valid.df$Pre_Gain=as.factor(pred_valid.df$Pre_Gain) 

## Confusion matrix
confusionMatrix(pred_valid.df$Pre_Gain, valid.cat.df$Gain)
#EM
# partition the data
## Bootstrapping
return_df1=return_df%>% slice(rep(1:n(), each = 100))


train.index <- sample(rownames(return_df), dim(return_df)[1]*0.7)  
valid.index<-setdiff(rownames(return_df),train.index)
train.df <- return_df[train.index, ]
valid.df <- return_df[valid.index, ]

train.df$Gain=as.factor(train.df$Gain) 
valid.df$Gain=as.factor(valid.df$Gain) 

# partition the training data again into 2 training sets, 1 for primary models, 1 for meta model
train1.index<-sample(train.index, dim(train.df)[1]*0.5) 
train2.index<-setdiff(train.index, train1.index) 
train1.df<-train.df[train1.index,]
train2.df<-train.df[train2.index,]

# use train() function to get the primary models (tree, knn, and logit) first
prim_tree<-train(Gain~Open+day_change+Volume, data=train1.df, method="rpart")
prim_knn<-train(Gain~Open+day_change+Volume, data=train1.df, method="knn")
prim_logit<-train(Gain~Open+day_change+Volume, data=train1.df, method="glm",family="binomial")

# then make predictions based on train2.df
tree_prediction<-predict(prim_tree,newdata=train2.df)
knn_prediction<-predict(prim_knn,newdata=train2.df)
logit_prediction<-predict(prim_logit,newdata=train2.df)

# compile a new data frame for the 3 models' predictions together with the actual response
meta_data<-data.frame(tree_prediction,knn_prediction,logit_prediction,Pre_Gain=train2.df$Gain)
# train the meta model based on the newly compiled 'meta_data' (here I use a logit model for the meta model)
meta.model<-train(Pre_Gain~., data=meta_data, method="glm",family="binomial")

### Predict
tree_pred_v<-predict(prim_tree,newdata=valid.df)
knn_pred_v<-predict(prim_knn,newdata=valid.df)
logit_pred_v<-predict(prim_logit,newdata=valid.df)

# make a data frame
meta_data_v<-data.frame(tree_prediction=tree_pred_v,
                        knn_prediction=knn_pred_v,
                        logit_prediction=logit_pred_v,
                        Pre_Gain=valid.df$Gain)

# predict based on the newly made data frame for validation
meta_pred<-predict(meta.model,newdata=meta_data_v)

# compare the model performance
confusionMatrix(tree_pred_v, valid.df$Gain)
confusionMatrix(knn_pred_v, valid.df$Gain)
confusionMatrix(logit_pred_v, valid.df$Gain)
confusionMatrix(meta_pred, valid.df$Gain)

## Predict
### Predict
tree_pred_v<-predict(prim_tree,newdata=test.df)
knn_pred_v<-predict(prim_knn,newdata=test.df)
logit_pred_v<-predict(prim_logit,newdata=test.df)
