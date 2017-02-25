# opening data frames with text
twitter_data <- read.csv("data/sample_data/twitter_l1000_s100.csv")
news_data <- read.csv("data/sample_data/news_l1000_s100.csv")
blogs_data <- read.csv("data/sample_data/blogs_l1000_s100.csv")

# Creating a corpus for each data set using tm
library(tm)
twitter_corp <- Corpus(VectorSource(twitter_data$twitter_lines))
news_corp <- Corpus(VectorSource(news_data$news_lines))
blogs_corp <- Corpus(VectorSource(blogs_data$blogs_lines))

# Combining corpora and clearning

corp <- c(twitter_corp,news_corp,blogs_corp)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)

# Creating a bigram text document matrix

BigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))

# Finding the frequency of each bigram

bigram_freq <- as.vector(rowSums(as.matrix(tdm)))
wordA <- gsub("\\s.+", "", rownames(as.matrix(tdm)))
wordB <- gsub(".+\\s", "", rownames(as.matrix(tdm)))
df <- data.frame(wordA,wordB,bigram_freq)
dfa <- aggregate(bigram_freq ~ wordA, data=df, sum)
df <- merge(df,dfa, by.x="wordA", by.y="wordA")
df$con_probs <- df$bigram_freq.x/df$bigram_freq.y
df <- data.frame(df$wordA,df$wordB,df$con_probs)
ord <- order(df$df.wordA,df$df.con_probs, decreasing = TRUE)
df <- df[ord,]



