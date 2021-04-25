library(e1071)
library(caret)

delays.df <- read.csv("FlightDelays.csv")
delays.df$DAY_WEEK <- as.factor(delays.df$DAY_WEEK)
delays.df$Flight.Status<-as.factor(delays.df$Flight.Status)
selected.var <- c(2, 4, 8, 10, 13)
train.index <- sample(rownames(delays.df), dim(delays.df)[1]*0.6) 
valid.index<-setdiff(rownames(delays.df), train.index)
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[valid.index, selected.var]
delays.nb <- naiveBayes(Flight.Status ~ ., data = train.df)
delays.nb
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
pred_valid.df<-cbind(valid.df,pred.prob)
pred.class <- predict(delays.nb, newdata = valid.df)
pred_valid.df<-cbind(pred_valid.df,pred.class)
pred.class <- predict(delays.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$Flight.Status)
pred.class <- predict(delays.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)
accidents.df <- read.csv("accidents.csv")
accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")
for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}
train.index <- sample(rownames(accidents.df), dim(accidents.df)[1]*0.6)  
valid.index <- setdiff(rownames(accidents.df), train.index)  
train.df <- accidents.df[train.index, ]
valid.df <- accidents.df[valid.index, ]
vars <- c("INJURY", "HOUR_I_R",	"ALIGN_I"	,"WRK_ZONE",	"WKDY_I_R",
          "INT_HWY",	"LGTCON_I_R",	"PROFIL_I_R",	"SPD_LIM",	"SUR_COND",
          "TRAF_CON_R",	"TRAF_WAY",	"WEATHER_R")
nb <- naiveBayes(INJURY ~ ., data = train.df[, vars])
confusionMatrix(predict(nb, train.df[, vars]),train.df$INJURY)
confusionMatrix(predict(nb, valid.df[, vars]),valid.df$INJURY)

