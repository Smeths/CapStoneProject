library(tm)

# Demonstrating how the findAssocs function works
data <- c("word1", "word1 word2","word1 word2 word3","word1 word2 word3 word4","word1 word2 word3 word4 word5") 
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




