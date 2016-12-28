library(tm)

# Demonstrating how the findAssocs function works
# dummy data for bigram tests
# data <- c("word1 word2", 
#           "word1 word2",
#           "word1 word3",
#           "word2 word4",
#           "word2 word4",
#           "word2 word1")
# dummy data for trigram tests

data <- c("worda wordb wordc", 
          "worda wordb wordc",
          "worda wordb wordd",
          "wordb wordc worda")

frame <-  data.frame(data)
test <-  Corpus(DataframeSource(frame))

BigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm2 <- TermDocumentMatrix(test, control = list(tokenize = BigramTokenizer))
inspect(tdm2)

# Calculating conditional probabilities for bigram model

bigram_freq <- as.vector(rowSums(as.matrix(tdm2)))
wordA <- gsub("\\s.+", "", rownames(as.matrix(tdm2)))
wordB <- gsub(".+\\s", "", rownames(as.matrix(tdm2)))
df2 <- data.frame(wordA,wordB,bigram_freq)
df3 <- aggregate(bigram_freq ~ wordA, data=df2, sum)
df4 <- merge(df2,df3, by.x="wordA", by.y="wordA")
df4$con_probs <- df4$bigram_freq.x/df4$bigram_freq.y
df5 <- data.frame(df4$wordA,df4$wordB,df4$con_probs)
ord <- order(df5$df4.wordA,df5$df4.con_probs, decreasing = TRUE)
df6 <- df5[ord,]
df6[df6$df4.wordA=="word2",]
