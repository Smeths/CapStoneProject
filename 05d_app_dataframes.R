# opening data frames with text
twitter_data <- read.csv("data/sample_data/train_twitter_l2000_s200.csv")
news_data <- read.csv("data/sample_data/train_news_l2000_s200.csv")
blogs_data <- read.csv("data/sample_data/train_blogs_l2000_s200.csv")

# Creating a corpus for each data set using tm
library(tm)
twitter_corp <- Corpus(VectorSource(twitter_data$train_twitter_lines))
news_corp <- Corpus(VectorSource(news_data$train_news_lines))
blogs_corp <- Corpus(VectorSource(blogs_data$train_blogs_lines))
trigram_csv <- "ShinyApp/data/trigram_ml.csv"
bigram_csv <- "ShinyApp/data/bigram_ml.csv"

# Combining corpora and clearning

corp <- c(twitter_corp,news_corp,blogs_corp)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, PlainTextDocument)

# finding words with frequency 1

tdm_uni <- TermDocumentMatrix(corp)
uni_freq <- as.vector(rowSums(as.matrix(tdm_uni)))
unkindex <- uni_freq == 1
words <- rownames(as.matrix(tdm_uni))
unkwords <- words[unkindex]

# Bigram Model

BigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm_bi <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))

bigram_freq <- as.vector(rowSums(as.matrix(tdm_bi)))
wordA <- gsub("\\s.+", "", rownames(as.matrix(tdm_bi)))
unkindex <- wordA %in% unkwords
wordA[unkindex] = "UNK"
wordB <- gsub(".+\\s", "", rownames(as.matrix(tdm_bi)))
unkindex <- wordB %in% unkwords
wordB[unkindex] = "UNK"
df <- data.frame(wordA,wordB,bigram_freq)
dfa <- aggregate(bigram_freq ~ wordA, data=df, sum)
df <- merge(df,dfa, by.x="wordA", by.y="wordA")
df$con_probs <- df$bigram_freq.x/df$bigram_freq.y
df_bi <- data.frame(df$wordA,df$wordB,df$con_probs)
names(df_bi) <- c("wordA","wordB","pBconA")
ord <- order(df_bi$wordA,df_bi$pBconA, decreasing = TRUE)
df_bi <- df_bi[ord,]
write.csv(df_bi,file=bigram_csv,row.names=FALSE)

# Trigram Model

TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))

trigram_freq <- as.vector(rowSums(as.matrix(tdm)))
wordAB <-gsub("(\\s[a-z]+)$", "", rownames(as.matrix(tdm)))
wordA <- gsub("\\s.+", "", wordAB)
unkindex <- wordA %in% unkwords
wordA[unkindex] = "UNK"
wordB <- gsub(".+\\s", "", wordAB)
unkindex <- wordB %in% unkwords
wordB[unkindex] = "UNK"
wordC <- gsub(".+\\s", "", rownames(as.matrix(tdm)))
unkindex <- wordC %in% unkwords
wordC[unkindex] = "UNK"
wordAB <- paste(wordA,wordB)

df <- data.frame(wordAB,wordC,trigram_freq)
df2 <- aggregate(trigram_freq ~ wordAB, data=df, sum)
df <- merge(df,df2, by.x="wordAB", by.y="wordAB")
df$con_probs <- df$trigram_freq.x/df$trigram_freq.y
df <- data.frame(df$wordAB,df$wordC,df$con_probs)
ord <- order(df$df.wordAB,df$df.con_probs, decreasing = TRUE)
df_tri <- df[ord,]
names(df_tri) <- c("wordAB","wordC","pCconAB")
write.csv(df_tri,file=trigram_csv,row.names=FALSE)



