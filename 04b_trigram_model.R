library(tm)

# opening data frames with text
twitter_data <- read.csv("data/sample_data/train_twitter_l1000_s100.csv")
news_data <- read.csv("data/sample_data/train_news_l1000_s100.csv")
blogs_data <- read.csv("data/sample_data/train_blogs_l1000_s100.csv")
trigram_csv <- "ShinyApp/data/trigram_l1000_s100.csv"

# Creating a corpus for each data set using tm
twitter_corp <- Corpus(VectorSource(twitter_data$train_twitter_lines))
news_corp <- Corpus(VectorSource(news_data$train_news_lines))
blogs_corp <- Corpus(VectorSource(blogs_data$train_blogs_lines))

# Combining corpora and clearning
corp <- c(twitter_corp,news_corp,blogs_corp)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)

TrigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))

trigram_freq <- as.vector(rowSums(as.matrix(tdm)))
wordAB <-gsub("(\\s[a-z]+)$", "", rownames(as.matrix(tdm)))
wordC <- gsub(".+\\s", "", rownames(as.matrix(tdm)))
df <- data.frame(wordAB,wordC,trigram_freq)
df2 <- aggregate(trigram_freq ~ wordAB, data=df, sum)
df <- merge(df,df2, by.x="wordAB", by.y="wordAB")
df$con_probs <- df$trigram_freq.x/df$trigram_freq.y
df <- data.frame(df$wordAB,df$wordC,df$con_probs)
ord <- order(df$df.wordAB,df$df.con_probs, decreasing = TRUE)
df_tri <- df[ord,]
names(df_tri) <- c("wordAB","wordC","con_probs")
write.csv(df_tri,file=trigram_csv,row.names=FALSE)

