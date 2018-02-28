library(XLConnect)
library(tm)
library(dplyr)
reviews <- read.csv("userreview.csv")
#reviews <- reviews[2,]
reviews
reviews <- data.frame(reviews)
all.reviews <- reviews[4]
all.reviews
length(all.reviews)
all.reviews <- unlist(all.reviews)
length(all.reviews)
#words <- unlist(strsplit(all.reviews, " "))
#length(words)
myCorpus <- Corpus(VectorSource(all.reviews))
length(myCorpus)
myCorpus <- tm_map(myCorpus, tolower)
inspect(myCorpus)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
inspect(myCorpus)

#myCorpus <- tm_map(mycorpus, stripWhitespace)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
inspect(myCorpus)
# remove stopwords
# keep "r" by removing it from stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
inspect(myCorpus)
dictCorpus <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus)

myDtm <- DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))
inspect(myDtm)
DtmFrame <- as.matrix(myDtm)
DtmFrame
write.csv(DtmFrame, "sparse_representation.csv",row.names = TRUE)
dim(DtmFrame)
