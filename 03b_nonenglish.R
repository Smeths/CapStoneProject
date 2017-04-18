suppressMessages(library(tm))
suppressMessages(library(ggplot2))

# opening data frames with text
twitter_data <- read.csv("data/sample_data/twitter_l1000_s100.csv")
news_data <- read.csv("data/sample_data/news_l1000_s100.csv")
blogs_data <- read.csv("data/sample_data/blogs_l1000_s100.csv")

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
corp <- tm_map(corp, PlainTextDocument)

tdm_uni <- TermDocumentMatrix(corp)
terms <- Terms(tdm_uni)

# american-english file used below kept at the following area /usr/share/dict/american-english
# on my linux operating system.

aeng_con <- file('data/american-english', 'r')
aeng_content <- readLines(aeng_con)
aeng_content <- gsub("'s","",aeng_content)
close(aeng_con)

nonenlish <- terms[!(terms %in% aeng_content)]
nonenglish_con <- file('data/nonenglish.txt', 'w')
writeLines(nonenlish,con=nonenglish_con)
close(nonenglish_con)
