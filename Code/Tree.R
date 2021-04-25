library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
bank.df$Personal.Loan<-as.factor(bank.df$Personal.Loan)
train.index <- sample(rownames(bank.df), dim(bank.df)[1]*0.6)
valid.index<-setdiff(rownames(bank.df),train.index)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
prp(default.ct)
pred_default <- predict(default.ct,newdata=valid.df,type = "class")
confusionMatrix(pred_default, valid.df$Personal.Loan)
rf <- randomForest(Personal.Loan ~ ., data = train.df, ntree = 500)  
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$Personal.Loan)
delays.df <- read.csv("FlightDelays.csv")
t(t(names(delays.df)))  # Look at your selected column names.
delays.df <- delays.df[, -c(1, 3, 6, 7, 11, 12)]  # Select your variables.
delays.df$DAY_WEEK <- as.factor(delays.df$DAY_WEEK)
train.index <- sample(rownames(delays.df), dim(delays.df)[1]*0.6)  
valid.index <- setdiff(rownames(delays.df), train.index)  
train.df <- delays.df[train.index, ]
valid.df <- delays.df[valid.index, ]
delays.ct <- rpart(Flight.Status ~ ., data = train.df, method = "class")
prp(delays.ct)
pred<-predict(delays.ct,newdata=valid.df,type = "class")
confusionMatrix(pred,valid.df$Flight.Status)
rf2 <- randomForest(Flight.Status ~ ., data = train.df, ntree = 500) 
rf2.pred <- predict(rf2, newdata=valid.df)
confusionMatrix(rf2.pred, valid.df$Flight.Status)
