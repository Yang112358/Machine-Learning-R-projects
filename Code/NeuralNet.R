library(neuralnet)
library(dummies)
library(caret)
library(forecast)

df <- read.csv("TinyData.csv")
df$Like <- df$Acceptance=="like"
df$Dislike <- df$Acceptance=="dislike"
set.seed(1)
nn <- neuralnet(Like + Dislike ~ Fat + Salt, data = df, linear.output=FALSE, hidden = 3)
nn$weights
plot(nn)
predict <- compute(nn, df[,2:3])
predicted.class=ifelse(apply(predict$net.result,1,which.max)==1,"like","dislike")
confusionMatrix(relevel(as.factor(predicted.class),ref="like"), 
                relevel(as.factor(df$Acceptance),ref="like"))
nn1 <- neuralnet(Like ~ Fat + Salt, data = df, linear.output=FALSE,hidden = 3)
plot(nn1)
predict <- compute(nn1, df[,2:3])
confusionMatrix(relevel(as.factor(ifelse(predict$net.result>0.5, "like", "dislike")),ref="like"), 
                relevel(as.factor(df$Acceptance),ref="like"))
accidents.df <- read.csv("accidentsnn.csv")
ir_dummies<-dummy(accidents.df$MAX_SEV_IR)
accidents.df<-cbind(accidents.df,ir_dummies)
train.index=sample(rownames(accidents.df), dim(accidents.df)[1]*0.6)
valid.index=setdiff(rownames(accidents.df), train.index)
train.df<-accidents.df[train.index,]
valid.df<-accidents.df[valid.index,]
nn <- neuralnet(MAX_SEV_IR0 + MAX_SEV_IR1 + MAX_SEV_IR2 ~ ALCHL_I + PROFIL_I_R + VEH_INVL, 
                data = train.df, linear.output=FALSE,hidden = 2)
train.prediction <- compute(nn, train.df)
train.class <- ifelse(apply(train.prediction$net.result,1,which.max)==1,"0",
                      ifelse(apply(train.prediction$net.result,1,which.max)==2,"1","2"))
confusionMatrix(as.factor(train.class), as.factor(accidents.df[train.index,]$MAX_SEV_IR))
valid.prediction <- compute(nn, valid.df)
valid.class <-ifelse(apply(valid.prediction$net.result,1,which.max)==1,"0",
                     ifelse(apply(valid.prediction$net.result,1,which.max)==2,"1","2"))
confusionMatrix(as.factor(valid.class), factor(accidents.df[valid.index,]$MAX_SEV_IR))
toyota.df <- read.csv("ToyotaCorolla.csv")
fuel_dummy<-dummy(toyota.df$Fuel_Type)
toyota.df <-cbind(toyota.df,fuel_dummy)
t(t(names(toyota.df)))
varlist<-c(3,4,9,12,14,17,19,21,25,26,28,30,34,39,40,41)
train.index <- sample(rownames(toyota.df), 0.6*dim(toyota.df)[1])  
valid.index <- setdiff(rownames(toyota.df), train.index)  
train.df <- toyota.df[train.index, varlist]
valid.df <- toyota.df[valid.index, varlist]
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)
nn1 <- neuralnet(Price ~ ., data = train.norm.df, linear.output = TRUE, hidden = 2)
plot(nn1)
train.prediction1=compute(nn1, train.norm.df)
valid.prediction1=compute(nn1, valid.norm.df)
accuracy(train.prediction1$net.result[,1], train.norm.df$Price)
accuracy(valid.prediction1$net.result[,1], valid.norm.df$Price)
nn2 <- neuralnet(Price ~ ., data = train.norm.df, linear.output = TRUE, hidden =5)
plot(nn2)
train.prediction2=compute(nn2, train.norm.df)
valid.prediction2=compute(nn2, valid.norm.df)
accuracy(train.prediction2$net.result[,1], train.norm.df$Price)
accuracy(valid.prediction2$net.result[,1], valid.norm.df$Price)
nn3 <- neuralnet(Price ~ ., data = train.norm.df, linear.output = TRUE, hidden = c(5,5))
plot(nn3)
train.prediction3=compute(nn3, train.norm.df)
valid.prediction3=compute(nn3, valid.norm.df)
accuracy(train.prediction3$net.result[,1], train.norm.df$Price)
accuracy(valid.prediction3$net.result[,1], valid.norm.df$Price)

