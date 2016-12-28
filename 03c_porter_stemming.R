suppressMessages(library(tm))
suppressMessages(library(ggplot2))

# opening data frames with text
twitter_data <- read.csv("data/sample_data/twitter_l500_s150.csv")
news_data <- read.csv("data/sample_data/news_l500_s150.csv")
blogs_data <- read.csv("data/sample_data/blogs_l500_s150.csv")

# Creating a corpus for each data set using tm
twitter_corp <- Corpus(VectorSource(twitter_data$twitter_lines))
news_corp <- Corpus(VectorSource(news_data$news_lines))
blogs_corp <- Corpus(VectorSource(blogs_data$blogs_lines))

# Combining corpora and clearning
corp <- c(twitter_corp,news_corp,blogs_corp)
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
corp <- tm_map(corp, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)
corp <- tm_map(corp, stemDocument, language = "porter")
tdm_uni <- TermDocumentMatrix(corp)

print("########################################################################")
print("NUMBER OF UNIQUE UNIGRAMS:")
nTerms(tdm_uni)
