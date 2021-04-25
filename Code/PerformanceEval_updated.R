library(forecast)
toyota.df <- read.csv("ToyotaCorolla.csv")
training <- sample(rownames(toyota.df), 600)
validation <- sample(setdiff(rownames(toyota.df),training), 400)
train.data<-toyota.df[training,]
valid.data<-toyota.df[validation,]
reg <- lm(Price~., data = train.data[,-c(1,2,8,11)])
pred_v <- predict(reg, newdata = valid.data[,-c(1,2,8,11)])
accuracy(reg$fitted.values,train.data$Price)
accuracy(pred_v,valid.data$Price)
library(caret)
owner.df <- read.csv("ownerExample.csv")
confusionMatrix(relevel(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')),ref="owner"), 
                relevel(as.factor(owner.df$Class),ref="owner"))
confusionMatrix(relevel(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')),ref="owner"), 
                relevel(as.factor(owner.df$Class),ref="owner"))
confusionMatrix(relevel(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')),ref="owner"), 
                relevel(as.factor(owner.df$Class),ref="owner"))
df<-read.csv("5_7.csv") ## when you download the cmpractice csv file from Blackboard, the name of the downloaded file would be "5_7.csv"
confusionMatrix(relevel(as.factor(ifelse(df$Propensity>0.5, 1, 0)),ref="1"), relevel(as.factor(df$Actual),ref="1"))
df1_train<-read.csv("prac1_train.csv")
reg<-lm(target~.,data=df1_train)
df1_valid<-read.csv("prac1_valid.csv")
library(forecast)
pred_values<-predict(reg,newdata=df1_valid)
accuracy(pred_values,df1_valid$target)
df2<-read.csv("prac2.csv")
library(caret)
confusionMatrix(relevel(as.factor(ifelse(df2$Propensity>0.5,"Yes","No")),ref="Yes"),
                relevel(as.factor(df2$Purchase),ref="Yes"))