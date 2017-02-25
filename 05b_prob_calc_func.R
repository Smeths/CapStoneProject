##########################################################
# unigram maximum likelihood probabilities              #
##########################################################
uni_prob_calc_ml <- function(wordA = "in"){
  unigram_df <- read.csv("data/likelihood_dataframes/unigram_df_1.csv")
  # matching to the data frame
  match = unigram_df$wordA == wordA
  if(sum(match)>0){
    prob_ml <- as.numeric(unigram_df[match,][2])    
  }
  else
  {
    prob_ml = 0
  }
  return(prob_ml)
}

##########################################################
# bigram maximum likelihood probabilities                #
##########################################################
bi_prob_calc_ml <- function(wordA = "in",wordB = "the"){
  bigram_df <- read.csv("data/likelihood_dataframes/bigram_df_ml_1.csv")
  # matching to the data frame
  matchA = bigram_df$wordA == wordA
  matchB = bigram_df$wordB == wordB
  match = matchA & matchB
  if(sum(match)>0){
    prob_ml <- as.numeric(bigram_df[match,][3])    
  }
  else
  {
    prob_ml = 0
  }
  return(prob_ml)
}

##########################################################
# bigram discounted probabilities                        #
##########################################################
bi_prob_calc_dis <- function(wordA = "in",wordB = "the"){
  unigram_df <- read.csv("data/likelihood_dataframes/unigram_df_1.csv")
  bigram_df <- read.csv("data/likelihood_dataframes/bigram_df_dis_1.csv")
  # matching to the data frame
  matchA <- bigram_df$wordA == wordA
  matchB <- bigram_df$wordB == wordB
  match <- matchA & matchB
  if(sum(match)>0){
    prob_dis <- as.numeric(bigram_df[match,][3])
  }
  else
  {
    resid_prob <- 1-sum(bigram_df[matchA,][3])
    wordnon0 <- as.vector(bigram_df[matchA,]$wordB)
    word0 <- as.vector(unigram_df[!(unigram_df$wordA %in% wordnon0),]$wordA)
    prob_word0 <- sum(as.vector(unigram_df[!(unigram_df$wordA %in% wordnon0),]$unigram_probs))
    prob_wordB <- as.vector(unigram_df[unigram_df$wordA == wordB,]$unigram_probs)
    prob_dis <- (prob_wordB/prob_word0)*resid_prob
  }
  # return(c(resid_prob,wordnon0,word0,prob_word0,prob_wordB,prob_dis))
  return(prob_dis)
}

##########################################################
# trigram maximum likelihood probabilities               #
##########################################################
tri_prob_calc_ml <- function(wordAB = "in the",wordC = "next"){
  trigram_df <- read.csv("data/likelihood_dataframes/trigram_df_ml_1.csv")
  # matching to the data frame
  matchAB = trigram_df$wordAB == wordAB
  matchC = trigram_df$wordC == wordC
  match = matchAB & matchC
  if(sum(match)>0){
    prob_ml <- as.numeric(trigram_df[match,][3])    
  }
  else
  {
    prob_ml = 0
  }
  return(prob_ml)
}

##########################################################
# trigram discounted likelihood probabilities            #
##########################################################
tri_prob_calc_dis <- function(wordAB = "in the",wordC = "next"){
  trigram_df <- read.csv("data/likelihood_dataframes/trigram_df_dis_1.csv")
  unigram_df <- read.csv("data/likelihood_dataframes/unigram_df_1.csv")
  # matching to the data frame
  matchAB = trigram_df$wordAB == wordAB
  matchC = trigram_df$wordC == wordC
  match = matchAB & matchC
  if(matchAB=0)
  {
    # Model not used in this situation
    prob_dis = -1
  }
  else if(sum(match)>0)
  {
    prob_dis <- as.numeric(trigram_df[match,][3])
  }
  else
  {
    wordA <- strsplit(wordAB, " ")[[1]][1]
    wordB <- strsplit(wordAB, " ")[[1]][2]
    resid_prob <- 1-sum(trigram_df[matchAB,][3])
    wordnon0 <- as.vector(trigram_df[matchAB,]$wordC)
    word0 <- as.vector(unigram_df[!(unigram_df$wordA %in% wordnon0),]$wordA)
    prob_wordBC <- bi_prob_calc_dis(wordB, wordC)
    prob_sumBC <- sum(sapply(word0,bi_prob_calc_dis,wordA=wordB))
    prob_dis <- prob_wordBC/prob_sumBC
  }
  return(resid_prob)
}


