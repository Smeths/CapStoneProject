library(tm)
con_corp <- file("data/sample_data/bi_perplexity_data_train.txt", "r")
file <- readLines(con=con_corp)
close(con_corp)
corp <- Corpus(VectorSource(file))

uni_tdm <- TermDocumentMatrix(corp, control = list(tolower=FALSE, wordLengths=c(1,Inf)))
uni_freq <- as.vector(rowSums(as.matrix(uni_tdm)))
sum_uni_freq <- sum(uni_freq)
prob_uni_freq <- uni_freq/sum_uni_freq
uni_words <-rownames(as.matrix(uni_tdm))
df <- data.frame(uni_words,prob_uni_freq)
names(df) <- c("word","pword")
write.csv(df,file="data/sample_data/uni_model_data.csv",row.names=FALSE)

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

bi_tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer,tolower=FALSE))
bi_freq <- as.vector(rowSums(as.matrix(bi_tdm)))

# constructing discounted data frame for bigram data

wordA <- gsub("\\s.+", "", rownames(as.matrix(bi_tdm)))
wordB <- gsub(".+\\s", "", rownames(as.matrix(bi_tdm)))
df <- data.frame(wordA,wordB,bi_freq)
dfa <- aggregate(bi_freq ~ wordA, data=df, sum)
df <- merge(df,dfa, by.x="wordA", by.y="wordA")
df$con_probs <- (df$bi_freq.x-0.5)/df$bi_freq.y
df <- data.frame(df$wordA,df$wordB,df$con_probs)
ord <- order(df$df.wordA,df$df.con_probs, decreasing = TRUE)
df <- df[ord,]
names(df) <- c("wordA","wordB","pBgivenA")
write.csv(df,file="data/sample_data/bi_model_data.csv",row.names=FALSE)
