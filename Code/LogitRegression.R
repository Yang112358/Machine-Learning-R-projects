library(caret)

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

set.seed(1)
train.index <- sample(rownames(bank.df), dim(bank.df)[1]*0.6)
valid.index<-setdiff(rownames(bank.df),train.index)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]
logit.reg <- glm(Personal.Loan ~Income, data = train.df, family = "binomial") 
summary(logit.reg)
plot(logit.reg$fitted.values~train.df$Income,ylim=c(0,1))
eq=function(x){1/(1+exp(6.217 - 0.038*x))} 
lines(eq(0:300),col="red")
points(train.df$Personal.Loan,col=28)
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
summary(logit.reg)
pred <- predict(logit.reg, valid.df, type = "response")
valid.predict<-data.frame(actual = valid.df$Personal.Loan, predicted = pred)
mowers.df <-read.csv("RidingMowers.csv")
mowers.df$Owner<-ifelse(mowers.df$Ownership=="Owner",1,0)
plot(Income ~ Lot_Size, data = mowers.df, 
     col=ifelse(mowers.df$Ownership=="Owner", "red","blue"))
legend("topright",legend=c("Owner","Nonowner"),pch=1,col=c("red","blue"))
reg<-glm(Owner ~ Income + Lot_Size, data = mowers.df, family = "binomial") 
summary(reg)
pred.prob<-predict(reg,type="response")
mowers.df$predicted_prob<-pred.prob
confusionMatrix(relevel(as.factor(ifelse(mowers.df$predicted_prob > 0.5, "Owner", "Nonowner")),ref="Owner"), relevel(as.factor(mowers.df$Ownership),ref="Owner"))
cutoffs<-data.frame(cutoff=seq(0.1,0.9,0.1),specificity=rep(0,9))
for(i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
  cm<-confusionMatrix(relevel(as.factor(ifelse(mowers.df$predicted_prob > i, "Owner", "Nonowner")),ref="Owner"), relevel(as.factor(mowers.df$Ownership),ref="Owner"))
  cutoffs[cutoffs$cutoff>=i,2]<-cm$byClass[2]
}
new<-data.frame(Income=60,Lot_Size=20)
new$predicted_prob<-predict(reg,newdata=new, type="response")
new$predicted_odds<-new$predicted_prob/(1-new$predicted_prob)
check_income<-data.frame(Income=seq(60,200,10),prob=rep(0,15))
for(i in seq(60,200,10)){
  tempdata=data.frame(Income=i,Lot_Size=16)
  pred<-predict(reg,newdata=tempdata,type="response")
  check_income[check_income$Income==i,2]<-pred
}
check_income<-data.frame(Income=seq(90,100,1),prob=rep(0,11))
for(i in seq(90,100,1)){
  tempdata=data.frame(Income=i,Lot_Size=16)
  pred<-predict(reg,newdata=tempdata,type="response")
  check_income[check_income$Income==i,2]<-pred
}
