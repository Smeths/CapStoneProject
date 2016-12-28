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
dtm <-  DocumentTermMatrix(test)
as.matrix(dtm) 
findAssocs(dtm, "word2", 0.1)
# Correlation between word2 and word3
cor(c(0,1,1,1,1),c(0,0,1,1,1))
# Correlation between word2 and word4
cor(c(0,1,1,1,1),c(0,0,0,1,1))

# Demo of other useful functions


# Generating a corpus and inspecting it
test$content
test[[1]]$content
test$meta

inspect(dtm)
inspect(dtm[,c("word1","word2")])
inspect(dtm[c(1,2),])
nDocs(dtm)
nTerms(dtm)
Terms(dtm)

# Plotting term frequencies
freq <- colSums(as.matrix(dtm))
barplot(freq)

findFreqTerms(dtm, lowfreq = 1, highfreq = Inf)
findFreqTerms(dtm, lowfreq = 2, highfreq = Inf)

# producing bigram information

tdm1 <- TermDocumentMatrix(test)
inspect(tdm1)
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

data <- c("going to the CINEMA!!!", 
          "hey I'm a boring cooool hipster",
          "see me looking totally fab at http://web.com. going",
          "being to cool for school")
frame <-  data.frame(data)
corp <-  Corpus(DataframeSource(frame))
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
corp <- tm_map(corp, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)
corp <- tm_map(corp, stemDocument, language = "porter")
tdm <- TermDocumentMatrix(corp)

as.matrix(tdm) 



