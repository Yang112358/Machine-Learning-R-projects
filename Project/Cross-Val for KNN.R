library(FNN)
library(caret)
library(dplyr)
Tesla.df <- read.csv("~/Downloads/TSLA.csv")
ave.accuracy.df <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))

## Return set create
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

# Split the data set
return_sample.index=sample(rownames(return_df), dim(return_df)[1]*0.8)
return.test.index=setdiff(rownames(return_df), return_sample.index)
return.train.df<-return_df[return_sample.index,]
return.test.df<-return_df[return.test.index,]
row.names(return.train.df) <- NULL

# bootstrapping
return.space.df = return.train.df %>% slice(rep(1:n(), each = 100))




for (k in 1:5){
   return.sample.index=sample(rownames(return.space.df), 400)
   return.sample.df<-return.space.df[return.sample.index,]
   row.names(return.sample.df) <- NULL

   ## Split data set
   all.train.index=sample(rownames(return.sample.df), dim(return.sample.df)[1]*0.8)
   test.index=setdiff(rownames(return.sample.df), all.train.index)
   all.train.df<-return.sample.df[all.train.index,]
   test.df<-return.sample.df[test.index,]

   all.train.stand.df=all.train.df
   test.stand.df = test.df
   # use preProcess() from the caret package to normalize Income and Lot_Size.
   norm.values <- preProcess(all.train.df[, 2:6], method=c("center", "scale"))
   all.train.stand.df[, 2:6] <- predict(norm.values, all.train.df[, 2:6])
   test.stand.df[, 2:6] <- predict(norm.values, test.df[, 2:6])


   # To avoid the Error: `data` and `reference` should be factors with the same levels.
   test.stand.df$Gain=as.factor(test.stand.df$Gain) 
   
   #Re-set the index of the row
   row.names(test.stand.df) <- NULL
   
   for (j in 1:100){ 
  
         train.index=sample(rownames(all.train.stand.df), dim(all.train.stand.df)[1]*0.8)
         valid.index=setdiff(rownames(all.train.stand.df), train.index)
         train.stand.df<-all.train.stand.df[train.index,]
         valid.stand.df<-all.train.stand.df[valid.index,]
 

 
       # To avoid the Error: `data` and `reference` should be factors with the same levels.
       valid.stand.df$Gain=as.factor(valid.stand.df$Gain)  


        # initialize a data frame with two columns: k, and accuracy.
        accuracy.df <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))


       # KNN
       for(i in 1:15) {
               knn.pred <- knn(train.stand.df[,2:5], valid.stand.df[, 2:5], 
                            cl = train.stand.df[, 7], k = i)
               accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.stand.df[, 7])$overall[1] 
               }
       ave.accuracy.df$accuracy=(ave.accuracy.df$accuracy+accuracy.df$accuracy)/2
   }
}
ave.accuracy.df




## From the result choose the k which max the accuracy.
k = which.max(ave.accuracy.df$accuracy)
print(k)

return.test.df$Gain=as.factor(return.test.df$Gain)  

knn.pred <- knn(return.train.df[,2:5], return.test.df[, 2:5], 
                cl = return.train.df[, 7], k)
confusionMatrix(knn.pred, return.test.df[, 7])
