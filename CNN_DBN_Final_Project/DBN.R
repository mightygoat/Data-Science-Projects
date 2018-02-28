
install.packages("deepnet")
library(deepnet)

data.dbn.sparse<-read.csv("sparse_representation.csv")
data.dbn.sentiment<-read.csv("label_reviews.csv")
#data.dbn.sparse<-data.frame(data.dbn.sparse)
#data.dbn.sentiment<-data.frame(data.dbn.sentiment)

samplizing<-sample(1:nrow(data.dbn.sparse),round(0.9*nrow(data.dbn.sparse)))

#samplizing<-sample(1,nrow(data.dbn.sentiment),replace = TRUE, prob = 0.8)
sparse_train<-data.dbn.sparse[samplizing,]
sparse_test<-data.dbn.sparse[-samplizing,]
sentiment_train<-data.dbn.sentiment[samplizing,]
sentiment_test<-data.dbn.sentiment[-samplizing,]
#nrow(sparse_train)
#nrow(sparse_test)
#nrow(sentiment_train)
#nrow(sentiment_test)

sparse_train_data<-data.matrix(sparse_train)
sparse_test_data<-data.matrix(sparse_test)
sparse_train_x<-sparse_train_data[,-1]
sparse_test_x<-sparse_test_data[,-1]
#nrow(sparse_train_x)
#nrow(sparse_test_x)
#sentiment_train_data<-data.matrix(sentiment_train)
#sentiment_test_data<-data.matrix(sentiment_test)
#nrow(sentiment_test_data)
#nrow(sentiment_train_data)
sentiment_train_y<-sentiment_train$final_decision
sentiment_train_y.y<-as.numeric(sentiment_train_y)-1

sentiment_test_y<-sentiment_test$final_decision
sentiment_test_y.y<-as.numeric(sentiment_test_y)-1

dnn <- dbn.dnn.train(sparse_train_x, sentiment_train_y.y, hidden = c(300,300,300), numepochs = 2, cd=2)

err.dnn.small<-nn.test(dnn,sparse_test_x,sentiment_test_y.y)
yy.dnn.small<-nn.predict(dnn, sparse_test_x)
yy.dnn.small


