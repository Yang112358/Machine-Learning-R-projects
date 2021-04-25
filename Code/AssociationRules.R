library(arules)
library(recommenderlab)
## Association rules
all.books.df <- read.csv("CharlesBookClub.csv")
count.books.df <- all.books.df[, 8:18]
incid.books.mat <- ifelse(count.books.df > 0, 1, 0)
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)
itemFrequencyPlot(books.trans)
rules <- apriori(books.trans, parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))
inspect(sort(rules, by = "lift"))
course.df <- read.csv("Coursetopics.csv")
course.mat <- as.matrix(course.df)
course.trans <- as(course.mat, "transactions")
inspect(course.trans)
itemFrequencyPlot(course.trans)
rules <- apriori(course.trans, parameter = list(supp= 0.01, conf = 0.5, target = "rules"))
inspect(head(sort(rules, by = "lift")))
rating.df <- read.csv("courserating.csv")
row.names(rating.df) <- rating.df[,1]
rating.df <- rating.df[,-1]
m <- as.matrix(rating.df)
r <- as(m, "realRatingMatrix")
UB.Rec <- Recommender(r, "UBCF")
pred_U <- predict(UB.Rec, r)
as(pred_U, "matrix")[1:10,]
IB.Rec <- Recommender(r, "IBCF")
pred_I <- predict(IB.Rec, r)
pred_I@items[1:10]
as(pred_I, "matrix")[1:10,]
