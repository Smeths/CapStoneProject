# opening data frames with text
twitter_data <- read.csv("data/sample_data/twitter_l100_s200.csv")
news_data <- read.csv("data/sample_data/news_l100_s200.csv")
blogs_data <- read.csv("data/sample_data/blogs_l100_s200.csv")


# Creating a corpus for each data set using tm
library(tm)
twitter_corp <- Corpus(VectorSource(twitter_data$twitter_lines))
twitter_corp <- tm_map(twitter_corp, removePunctuation)
twitter_corp <- tm_map(twitter_corp, removeNumbers)
twitter_corp <- tm_map(twitter_corp, tolower)
twitter_corp <- tm_map(twitter_corp, PlainTextDocument) 
twitter_dtm <- DocumentTermMatrix(twitter_corp)
inspect(twitter_dtm[1,])
nDocs(twitter_dtm)
findAssocs(twitter_dtm,c("you"),0.1)
twitter_dtm[1,]
twitter_freq <- colSums(as.matrix(twitter_dtm))
twitter_ord <- order(twitter_freq)
twitter_freq[tail(twitter_ord)]/sum(twitter_freq)

news_corp <- Corpus(VectorSource(news_data$news_lines))
news_corp <- tm_map(news_corp, removePunctuation)
news_corp <- tm_map(news_corp, removeNumbers)
news_corp <- tm_map(news_corp, tolower)
news_corp <- tm_map(news_corp, PlainTextDocument) 
news_dtm <- DocumentTermMatrix(news_corp)
news_freq <- colSums(as.matrix(news_dtm))
news_ord <- order(news_freq)
news_freq[tail(news_ord)]/sum(news_freq)

blogs_corp <- Corpus(VectorSource(blogs_data$blogs_lines))
blogs_corp <- tm_map(blogs_corp, removePunctuation)
blogs_corp <- tm_map(blogs_corp, removeNumbers)
blogs_corp <- tm_map(blogs_corp, tolower)
blogs_corp <- tm_map(blogs_corp, PlainTextDocument) 
blogs_dtm <- DocumentTermMatrix(blogs_corp)
blogs_freq <- colSums(as.matrix(blogs_dtm))
blogs_ord <- order(blogs_freq)
blogs_freq[tail(blogs_ord)]/sum(blogs_freq)


