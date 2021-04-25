toyota.df <- read.csv("ToyotaCorolla.csv")
toyotaCAT <- model.matrix(~ 0 + Fuel_Type + Color, data = toyota.df)
toyotaCAT.df <- as.data.frame(toyotaCAT)
t(t(names(toyotaCAT.df)))
toyota.df <- cbind(toyota.df,toyotaCAT.df)
head(toyota.df)
train.rows <- sample(rownames(toyota.df),dim(toyota.df)[1]*0.5)
valid.rows <-sample(setdiff(rownames(toyota.df), train.rows),dim(toyota.df)[1]*0.3)
test.rows <- setdiff(rownames(toyota.df), union(train.rows, valid.rows))
train.data <- toyota.df[train.rows,]
valid.data <- toyota.df[valid.rows,]
test.data <- toyota.df[test.rows,]
library(forecast)
toyota.df<-na.omit(toyota.df)
training <- sample(rownames(toyota.df), 600)
validation <- sample(rownames(toyota.df), 400)
train.data<-toyota.df[training,]
valid.data<-toyota.df[validation,]
reg <- lm(Price~., data = train.data[,-c(1,2,8,11)])
pred_v <- predict(reg, newdata = valid.data[,-c(1,2,8,11)])
accuracy(reg$fitted.values,train.data$Price)
accuracy(pred_v,valid.data$Price)