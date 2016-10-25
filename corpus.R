# Loading tm library
library(tm)
# opening a corpus of the sampled docs
text_dir = "/home/smeths/Documents/development/rProgramming/Coursera/Capstone Project/Week 1/sample_data/"
docs <- Corpus(DirSource(text_dir))   
summary(docs)
# applying a few filters
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)   

# creates a term dictionary starts with terms starting with 0,1,2,3,4,5,6,7,8,9,a,b
dtm <- DocumentTermMatrix(docs)
# Note one row for each document in the corpus
inspect(dtm[1:3,1000:1010])

# finding the most common words
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)
freq[tail(ord)]

# plotting results
barplot(freq[tail(ord)])
