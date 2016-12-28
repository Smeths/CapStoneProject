suppressMessages(library(tm))
suppressMessages(library(knitr))

# opening data frames with text
twitter_data <- read.csv("data/sample_data/twitter_l500_s150.csv")
news_data <- read.csv("data/sample_data/news_l500_s150.csv")
blogs_data <- read.csv("data/sample_data/blogs_l500_s150.csv")

# Creating a corpus for each data set using tm
twitter_corp <- Corpus(VectorSource(twitter_data$twitter_lines))
news_corp <- Corpus(VectorSource(news_data$news_lines))
blogs_corp <- Corpus(VectorSource(blogs_data$blogs_lines))

# Combining corpora and clearning
corp <- c(twitter_corp,news_corp,blogs_corp)
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
corp <- tm_map(corp, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)
if(stemming){
     corp <- tm_map(corp, stemDocument, language = "porter")
}
tdm_uni <- TermDocumentMatrix(corp)

uni_num_unique <- nTerms(tdm_uni)
unigram_freq <- rowSums(as.matrix(tdm_uni))
uni_ord <- order(as.vector(unigram_freq),decreasing=TRUE)
unigram_ord_freq <- unigram_freq[uni_ord]
uni_50_percent <- 100*prop_number(prop_test=0.5,freq_ngrams = as.vector(unigram_ord_freq))/uni_num_unique
uni_90_percent <- 100*prop_number(prop_test=0.9,freq_ngrams = as.vector(unigram_ord_freq))/uni_num_unique
uni_non_sparse <- sum(as.matrix(tdm_uni) > 0)
uni_sparse <- tdm_uni$nrow * tdm_uni$ncol - uni_non_sparse

BigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
tdm_bi <- TermDocumentMatrix(corp,control=list(tokenize = BigramTokenizer))

bi_num_unique <- nTerms(tdm_bi)
bigram_freq <- rowSums(as.matrix(tdm_bi))
bi_ord <- order(as.vector(bigram_freq),decreasing=TRUE)
bigram_ord_freq <- bigram_freq[bi_ord]
bi_50_percent <- 100*prop_number(prop_test=0.5,freq_ngrams = as.vector(bigram_ord_freq))/bi_num_unique
bi_90_percent <- 100*prop_number(prop_test=0.9,freq_ngrams = as.vector(bigram_ord_freq))/bi_num_unique
bi_non_sparse <- sum(as.matrix(tdm_bi) > 0)
bi_sparse <- tdm_bi$nrow * tdm_bi$ncol - bi_non_sparse

TrigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm_tri <- TermDocumentMatrix(corp,control=list(tokenize = TrigramTokenizer))

tri_num_unique <- nTerms(tdm_tri)
trigram_freq <- rowSums(as.matrix(tdm_tri))
tri_ord <- order(as.vector(trigram_freq),decreasing=TRUE)
trigram_ord_freq <- trigram_freq[tri_ord]
tri_50_percent <- 100*prop_number(prop_test=0.5,freq_ngrams = as.vector(trigram_ord_freq))/tri_num_unique
tri_90_percent <- 100*prop_number(prop_test=0.9,freq_ngrams = as.vector(trigram_ord_freq))/tri_num_unique
tri_non_sparse <- sum(as.matrix(tdm_tri) > 0)
tri_sparse <- tdm_tri$nrow * tdm_tri$ncol - tri_non_sparse

QuadgramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

tdm_quad <- TermDocumentMatrix(corp,control=list(tokenize = QuadgramTokenizer))

quad_num_unique <- nTerms(tdm_quad)
quadgram_freq <- rowSums(as.matrix(tdm_quad))
quad_ord <- order(as.vector(quadgram_freq),decreasing=TRUE)
quadgram_ord_freq <- quadgram_freq[quad_ord]
quad_50_percent <- 100*prop_number(prop_test=0.5,freq_ngrams = as.vector(quadgram_ord_freq))/quad_num_unique
quad_90_percent <- 100*prop_number(prop_test=0.9,freq_ngrams = as.vector(quadgram_ord_freq))/quad_num_unique
quad_non_sparse <- sum(as.matrix(tdm_quad) > 0)
quad_sparse <- tdm_quad$nrow * tdm_quad$ncol - quad_non_sparse

df_freq_info <- data.frame(Number.Unique = c(uni_num_unique,bi_num_unique,tri_num_unique,quad_num_unique),
                           Per.For50.Per = c(uni_50_percent,bi_50_percent,tri_50_percent,quad_50_percent),
                           Per.For90.Per = c(uni_90_percent,bi_90_percent,tri_90_percent,quad_90_percent),
                           Num.Non.Sparse = c(uni_non_sparse,bi_non_sparse,tri_non_sparse,quad_non_sparse),
                           Num.Sparse = c(uni_sparse,bi_sparse,tri_sparse,quad_sparse))

colnames(df_freq_info) <- c("Number Unique","Percent require for 50% Coverage","Percent require for 90% Coverage","Not Blank TDM","Blank TDM")
rownames(df_freq_info) <- c("Unigrams","Bigrams","Trigrams","Quadgrams")











