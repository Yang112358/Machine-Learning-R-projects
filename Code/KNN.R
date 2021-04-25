library(caret)

#### Riding mower example
mower.df <- read.csv("RidingMowers.csv")
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])  
valid.index <- setdiff(row.names(mower.df), train.index)  
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]
new.df <- data.frame(Income = 60, Lot_Size = 20)
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2])
new.norm.df <- predict(norm.values, new.df)
library(FNN)
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 3)
row.names(train.df)[attr(nn, "nn.index")]
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                  cl = train.norm.df[, 3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1] 
}
accuracy.df
knn.pred.new <- knn(train.norm.df[, 1:2], new.norm.df, 
                    cl = train.norm.df[, 3], k = 4)
row.names(train.df)[attr(knn.pred.new, "nn.index")]
library(caret)
library(FNN)

cust.df <- data.frame(custid = c(1, 2, 3), 
                      Stat = c(1, 0, 0), 
                      IT = c(0, 0, 1),
                      Other = c(0, 1, 0), 
                      year = c(1, 1.1, 1), 
                      course = c(0, 1, NA))

cust.df.3dummies <- cust.df[,-c(1, 6)]
cust.df.2dummies <- cust.df.3dummies[,-3]
dist(cust.df.3dummies) # prospect is closer to cust 1 --> not taking the course
dist(cust.df.2dummies) # prospect is closer to cust 2 --> taking the course
knn(cust.df[1:2,2:5], cust.df[3, 2:5], cl = cust.df[1:2, 6], k = 1)
knn(cust.df[1:2,c(2,3,5)], cust.df[3, c(2,3,5)], cl = cust.df[1:2, 6], k = 1)
housing.df<-read.csv("WestRoxburyKNN.csv")
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])  
valid.index <- setdiff(row.names(housing.df), train.index)  
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]
train.norm.df <- train.df
valid.norm.df <- valid.df
norm.values <- preProcess(train.df[, 1:3], method=c("center", "scale"))
train.norm.df[, 1:3] <- predict(norm.values, train.df[, 1:3])
valid.norm.df[, 1:3] <- predict(norm.values, valid.df[, 1:3])
knn(train = train.norm.df[, 1:3], test = valid.norm.df[,1:3], 
    cl = train.norm.df[, 4], k = 3)
accuracy.df <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))
for(i in 1:15) {
  knn.pred <- knn(train.norm.df[, 1:3], valid.norm.df[, 1:3], 
                  cl = train.norm.df[, 4], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 4])$overall[1] 
}

