library(tm)
library(lsa)
library(caret)
text <- c("this is the first sentence", 
          "this is a second sentence", 
          "the third sentence is here")
corp <- Corpus(VectorSource(text))
tdm <- TermDocumentMatrix(corp)
inspect(tdm)
text <- c("this is the first sentence!!", 
          "this is a second Sentence :)", 
          "the third sentence, is here", 
          "forth of all sentences")
corp <- Corpus(VectorSource(text))
tdm <- TermDocumentMatrix(corp)
inspect(tdm)
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, removePunctuation) 
tdm <- TermDocumentMatrix(corp)
inspect(tdm)
stopwords("english")
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, stemDocument) 
tdm <- TermDocumentMatrix(corp)
inspect(tdm)
tfidf <- weightTfIdf(tdm)
inspect(tfidf)
corp <- Corpus(ZipSource("AutoAndElectronics.zip", recursive = T))
label <- c(rep(1, 1000), rep(0, 1000))
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, removePunctuation) 
corp <- tm_map(corp, removeNumbers) 
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, stemDocument) 
tdm <- TermDocumentMatrix(corp)
inspect(tdm)
tfidf <- weightTfIdf(tdm)
lsa.tfidf <- lsa(tfidf, dim = 20)
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk)) 
training <- sample(c(1:2000), 0.6*2000)
trainData = cbind(label = label[training], words.df[training,])
reg <- glm(label ~ ., data = trainData, family = 'binomial')
validData = cbind(label = label[-training], words.df[-training,])
pred <- predict(reg, newdata = validData, type = "response")
confusionMatrix(as.factor(ifelse(pred>0.5, 1, 0)), as.factor(label[-training]))
library(wordcloud)
termcount <-apply(tdm,1,sum)
wordcloud(names(termcount),termcount, min.freq=100,colors=brewer.pal(8, "Dark2"))
