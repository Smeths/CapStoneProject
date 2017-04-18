library(tm)

# opening data frames with text
con_corp <- file("data/sample_data/model_data.txt", "r")
file <- readLines(con=con_corp)
close(con_corp)
corp <- Corpus(VectorSource(file))
bigram_csv <- "ShinyApp/data/bigram.csv"

BigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer,tolower=FALSE))

# Finding the frequency of each bigram

bigram_freq <- as.vector(rowSums(as.matrix(tdm)))
wordA <- gsub("\\s.+", "", rownames(as.matrix(tdm)))
wordB <- gsub(".+\\s", "", rownames(as.matrix(tdm)))
df <- data.frame(wordA,wordB,bigram_freq)
dfa <- aggregate(bigram_freq ~ wordA, data=df, sum)
df <- merge(df,dfa, by.x="wordA", by.y="wordA")
df$con_probs <- df$bigram_freq.x/df$bigram_freq.y
df <- data.frame(df$wordA,df$wordB,df$con_probs)
ord <- order(df$df.wordA,df$df.con_probs, decreasing = TRUE)
df <- df[ord,]
names(df) <- c("wordA","wordB","pBgivenA")
write.csv(df,file=bigram_csv,row.names=FALSE)



