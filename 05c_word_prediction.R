##########################################################
# trigram prediciton and probabilities                   #
##########################################################
tri_prediction <- function(wordAB = "in the"){
  trigram_df <- read.csv("data/likelihood_dataframes/trigram_df_dis_1.csv")
  # matching to the data frame
  matchAB = trigram_df$wordAB == wordAB
  if(sum(matchAB)==0)
  {
    # Model not used in this situation
    prob = -1
    prediction = ""
    df=""
  }
  else
  {
    prediction <- as.character(trigram_df[matchAB,][1,2])
    prob <- as.character(trigram_df[matchAB,][1,3])
    df <- head(as.character(bigram_df[matchAB,]))
  }
  return(c(prediction,prob,df))
}

##########################################################
# bigram prediction and probabilities                    #
##########################################################
bi_prediction <- function(wordA = "in"){
  bigram_df <- read.csv("data/likelihood_dataframes/bigram_df_dis_1.csv")
  # matching to the data frame
  match <- bigram_df$wordA == wordA
  prediction <- as.character(bigram_df[match,][1,2])
  prob <- as.character(bigram_df[match,][1,3])
  df <- head(as.character(bigram_df[match,]))
  return(c(prediction,prob,df))
}

comb_prediction <- function(wordAB = "in the"){
  pred_type <- "trigram"
  predprob <- tri_prediction(wordAB)
  if (as.numeric(predprob[2]) < 0)
  {
    wordB <- strsplit(wordAB, " ")[[1]][2]
    predprob <- bi_prediction(wordB)
    pred_type <- "bigram"
  }
  return(c(predprob,pred_type))
}
