test_data <- file("data/sample_data/bi_perplexity_data_test.txt", "r")
file <- readLines(test_data)
close(test_data)
bigram_df <- read.csv("data/sample_data/bi_model_data.csv")
unigram_df <- read.csv("data/sample_data/uni_model_data.csv")

# Checking vocab size

V = dim(unigram_df)[1]

# calculating perplexity

num_sens <- length(file)
M <- 0
logprob <- 0

# Looping over lines in the file

for (i in 1:num_sens){
  sen <- file[i]
  words <- strsplit(sen, " ")[[1]]
  M <- M + length(words)
  num_words <- length(words) - 1

# Looping over words in the line
  
  logprobsen <- 0
  
  for (j in 1:num_words) {
    
    wordA <- words[j]
    wordB <- words[j+1]
    matchA <- unigram_df$word == wordA
    matchB <- unigram_df$word == wordB
    
    if (sum(matchA) == 0) {
      wordA <- "UNK"
    }
    
    if (sum(matchB) == 0) {
      wordB <- "UNK"
    }
    
    # finding bigram probability using Katz backoff method
    
    matchA <- bigram_df$wordA == wordA
    matchB <- bigram_df$wordB == wordB
    matchAB <- matchA & matchB
    
    if (sum(matchAB) > 0 ){
      pBgivenA <- as.numeric(bigram_df[matchAB,][1,3])
    } else {
      wordAresidprob <- 1 - sum(bigram_df[matchA,][,3])
      wordBs <- bigram_df[matchA,][,2]
      matchBs <- unigram_df$word %in% wordBs
      matchB <- unigram_df$word == wordB
      pB <- as.numeric(unigram_df[matchB,][1,2])
      pBgivenA <- (pB/sum(unigram_df[matchBs,]$pword)) * wordAresidprob
    }
    logprobsen <- logprobsen + logb(pBgivenA,base=2)
  }
  logprob <- logprob + logprobsen
}

perp <- 2^(-(1/M)*logprob)



  

  



