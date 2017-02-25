# opening data frames with text
twitter_data <- read.csv("data/sample_data/train_twitter_l1000_s100.csv")
news_data <- read.csv("data/sample_data/train_news_l1000_s100.csv")
blogs_data <- read.csv("data/sample_data/train_blogs_l1000_s100.csv")

# Creating a corpus for each data set using tm
library(tm)
twitter_corp <- Corpus(VectorSource(twitter_data$train_twitter_lines[1:1]))
news_corp <- Corpus(VectorSource(news_data$train_news_lines[1:1]))
blogs_corp <- Corpus(VectorSource(blogs_data$train_blogs_lines[1:1]))

# Combining corpora and clearning

# corp <- c(twitter_corp,news_corp,blogs_corp)
corp <- c(news_corp)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)

# Forming maximum likelihood for unigrams
# wordLengths=c(1,Inf) added as one and two letter
# words seem to be ignored by default

tdm_uni <- TermDocumentMatrix(corp,control=list(wordLengths=c(1,Inf)))
unigram_freq <- as.vector(rowSums(as.matrix(tdm_uni)))
unigram_total <- sum(unigram_freq)
unigram_probs <- unigram_freq/unigram_total
unigram_words <- rownames(as.matrix(tdm_uni))
unigram_df <- data.frame(wordA=unigram_words,unigram_probs=unigram_probs)

# discounting factor for each frequency
dis = 0.5

# Forming likelihoods for bigrams
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm_bi <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
bigram_freq <- as.vector(rowSums(as.matrix(tdm_bi)))
wordA <- gsub("\\s.+", "", rownames(as.matrix(tdm_bi)))
wordB <- gsub(".+\\s", "", rownames(as.matrix(tdm_bi)))
df <- data.frame(wordA,wordB,bigram_freq)
dfa <- aggregate(bigram_freq ~ wordA, data=df, sum)
df <- merge(df,dfa, by.x="wordA", by.y="wordA")
# Forming maximum likelihood for bigrams
df$con_probs <- df$bigram_freq.x/df$bigram_freq.y
bigram_df_ml <- data.frame(wordA=df$wordA,wordB=df$wordB,bigram_probs=df$con_probs)
ord <- order(bigram_df_ml$wordA,bigram_df_ml$bigram_probs, decreasing = TRUE)
bigram_df_ml <- bigram_df_ml[ord,]
# Forming discounted likelihood for bigrams
df$con_probs <- (df$bigram_freq.x-dis)/df$bigram_freq.y
bigram_df_dis <- data.frame(wordA=df$wordA,wordB=df$wordB,bigram_probs=df$con_probs)
ord <- order(bigram_df_dis$wordA,bigram_df_dis$bigram_probs, decreasing = TRUE)
bigram_df_dis <- bigram_df_dis[ord,]

# Forming likelihoods for trigrams
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))
trigram_freq <- as.vector(rowSums(as.matrix(tdm)))
wordAB <-gsub("(\\s[a-z]+)$", "", rownames(as.matrix(tdm)))
wordC <- gsub(".+\\s", "", rownames(as.matrix(tdm)))
df <- data.frame(wordAB,wordC,trigram_freq)
dfa <- aggregate(trigram_freq ~ wordAB, data=df, sum)
df <- merge(df,dfa, by.x="wordAB", by.y="wordAB")
# Forming maximum likelihood for trigrams
df$con_probs <- df$trigram_freq.x/df$trigram_freq.y
trigram_df_ml <- data.frame(wordAB = df$wordAB,wordC = df$wordC,trigram_probs=df$con_probs)
ord <- order(trigram_df_ml$wordAB,trigram_df_ml$trigram_probs, decreasing = TRUE)
trigram_df_ml <- trigram_df_ml[ord,]
# Forming discounted likelihood for trigrams
df$con_probs <- (df$trigram_freq.x-dis)/df$trigram_freq.y
trigram_df_dis <- data.frame(wordAB=df$wordAB,wordC=df$wordC,trigram_probs=df$con_probs)
ord <- order(trigram_df_dis$wordAB,trigram_df_dis$trigram_probs, decreasing = TRUE)
trigram_df_dis <- trigram_df_dis[ord,]

write.csv(unigram_df,file="data/likelihood_dataframes/unigram_df_1.csv",row.names=FALSE)
write.csv(bigram_df_ml,file="data/likelihood_dataframes/bigram_df_ml_1.csv",row.names=FALSE)
write.csv(bigram_df_dis,file="data/likelihood_dataframes/bigram_df_dis_1.csv",row.names=FALSE)
write.csv(trigram_df_ml,file="data/likelihood_dataframes/trigram_df_ml_1.csv",row.names=FALSE)
write.csv(trigram_df_dis,file="data/likelihood_dataframes/trigram_df_dis_1.csv",row.names=FALSE)




