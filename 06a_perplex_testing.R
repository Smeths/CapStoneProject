# opening data frames with text

twitter_data <- read.csv("data/sample_data/twitter_l1000_s125.csv")
news_data <- read.csv("data/sample_data/news_l1000_s125.csv")
blogs_data <- read.csv("data/sample_data/blogs_l1000_s125.csv")
bi_data_train <- file("data/sample_data/bi_perplexity_data_train.txt", "w")
tri_data_train <- file("data/sample_data/tri_perplexity_data_train.txt", "w")
model_data <- file("data/sample_data/model_data.txt","w")
bi_data_test <- file("data/sample_data/bi_perplexity_data_test.txt", "w")
tri_data_test <- file("data/sample_data/tri_perplexity_data_test.txt", "w")

# Creating a corpus for each data set using tm

library(tm)
twitter_corp <- Corpus(VectorSource(twitter_data$twitter_lines))
news_corp <- Corpus(VectorSource(news_data$news_lines))
blogs_corp <- Corpus(VectorSource(blogs_data$blogs_lines))

# Combining corpora and clearning

corp <- c(twitter_corp,news_corp,blogs_corp)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, PlainTextDocument)

# Creating a unigram matrix

v1w_tdm <- TermDocumentMatrix(corp)
v1w_freq <- as.vector(rowSums(as.matrix(v1w_tdm)))
unkindex <- v1w_freq == 1
sum(unkindex)
v1w <- rownames(as.matrix(v1w_tdm))
v1w_unk <- v1w[unkindex]
v1w[unkindex] <- "UNK"

# Ammending corpus with start end and unk

ndocs <- nDocs(v1w_tdm)
# Use 95% to train and 5% to test
breakp <- as.numeric(round(0.95*ndocs, digits = 0))

for (i in 1:ndocs) {
  if ( i <= breakp) {
    content <- corp[[i]]$content
    for (j in 1:length(v1w_unk)){
      v1w_unk_str <- paste("\\s", v1w_unk[j], "\\s",sep="")
      content <- gsub(v1w_unk_str," UNK ",content)
    }
    writeLines(content, con = model_data)
    content <- paste("START",content,"END",sep=" ")
    writeLines(content, con = bi_data_train)
    writeLines(paste("START",content,sep=" "), con = tri_data_train)
  }
  else{
    writeLines(content, con = bi_data_test)
    writeLines(paste("START",content,sep=" "), con = tri_data_test)
  }
}

close(bi_data_train)
close(tri_data_train)
close(bi_data_test)
close(tri_data_test)
close(model_data)