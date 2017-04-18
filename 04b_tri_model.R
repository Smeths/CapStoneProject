library(tm)

# opening data frames with text
con_corp <- file("data/sample_data/model_data.txt", "r")
file <- readLines(con=con_corp)
close(con_corp)
corp <- Corpus(VectorSource(file))
trigram_csv <- "ShinyApp/data/trigram.csv"

TrigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer,tolower=FALSE))

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
names(df_tri) <- c("wordAB","wordC","pCgivenAB")
write.csv(df_tri,file=trigram_csv,row.names=FALSE)

