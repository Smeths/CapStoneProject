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

# producing trigram information

TrigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm2 <- TermDocumentMatrix(test, control = list(tokenize = TrigramTokenizer))
inspect(tdm2)
trigram_freq <- as.vector(rowSums(as.matrix(tdm2)))
wordAB <-gsub("(\\s[a-z]+)$", "", rownames(as.matrix(tdm2)))
wordC <- gsub(".+\\s", "", rownames(as.matrix(tdm2)))
df2 <- data.frame(wordAB,wordC,trigram_freq)
df3 <- aggregate(trigram_freq ~ wordAB, data=df2, sum)
df4 <- merge(df2,df3, by.x="wordAB", by.y="wordAB")
df4$con_probs <- df4$trigram_freq.x/df4$trigram_freq.y
df5 <- data.frame(df4$wordAB,df4$wordC,df4$con_probs)
ord <- order(df5$df4.wordAB,df5$df4.con_probs, decreasing = TRUE)
df6 <- df5[ord,]