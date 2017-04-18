library(shiny)

bigram_df <- read.csv("data/bigram.csv")
trigram_df <- read.csv("data/trigram.csv")

##########################################################
# preprocessing user input                               #
##########################################################

clean <- function(wordAB = "THE last",count){
  # converting to lower case
  wordAB <- tolower(wordAB)
  # removing punctuation
  wordAB <- gsub("[[:punct:]]", "", wordAB)

  # initialising warning to "none"
  warning <- "none"
  # picking last two words
  if(count > 1)
  {
    wordAB <- tail(strsplit(wordAB, " ")[[1]],2)
    wordAB <- paste(wordAB[1],wordAB[2],sep=" ")
    wordB <- strsplit(wordAB, " ")[[1]][2]
    matchB = bigram_df$wordA == wordB
    if(sum(matchB) == 0)
    {
      warning = paste("word",wordB,"not recognised!",sep=" ")
      wordB = "UNK"
      wordAB = paste(strsplit(wordAB, " ")[[1]][1],wordB,sep=" ")
    }
  }
  else
  {
    matchA = bigram_df$wordA == wordAB
    if(sum(matchA) == 0)
    {
      warning = paste("word",wordAB,"not recognised!",sep=" ")
      wordAB = "UNK"  
    }
  }
  return(c(wordAB,warning))
}

##########################################################
# trigram prediciton and probabilities                   #
##########################################################
tri_prediction <- function(wordAB = "in the"){
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
    df <- head(trigram_df[matchAB,])
    # picking next element or using bigram if "UNK" if predicted
    if (prediction=="UNK")
    {
      if(dim(df)[1]==1){
        prob = -1
        prediction = ""
        df=""
      }
      else
      {
        prediction <- as.character(trigram_df[matchAB,][2,2])
        prob <- as.character(trigram_df[matchAB,][2,3])
      }
    }
  }
  return(list(prediction,prob,df))
}

##########################################################
# bigram prediction and probabilities                    #
##########################################################
bi_prediction <- function(wordA = "in"){
  # matching to the data frame
  match <- bigram_df$wordA == wordA
  prediction <- as.character(bigram_df[match,][1,2])
  prob <- as.character(bigram_df[match,][1,3])
  df <- head(bigram_df[match,])
  # picking next element or using bigram if "UNK" if predicted
  if (prediction=="UNK")
  {
    if(dim(df)[1]==1){
      prob = -1
      prediction = ""
      df=""
    }
    else
    {
      prediction <- as.character(bigram_df[match,][2,2])
      prob <- as.character(bigram_df[match,][2,3])
    }
  }
  return(list(prediction,prob,df))
}

##########################################################
# combining models                                       #
##########################################################

comb_prediction <- function(wordAB = "in the",count){
  if(count > 1){
    predprob <- tri_prediction(wordAB)
    pred_type <- "Trigram"
    if (as.numeric(predprob[2]) < 0)
    {
      wordB <- strsplit(wordAB, " ")[[1]][2]
      predprob <- bi_prediction(wordB)
      pred_type <- "Bigram"
      if (as.numeric(predprob[2])<0)
      {
        predprob <- list("the",0.035,"")
        pred_type = "Most Common Word"
      }
    }
  }
  else
  {
    predprob <- bi_prediction(wordAB)
    pred_type <- "Bigram"
    if (as.numeric(predprob[2])<0)
    {
      predprob <- list("the",0.035,"")
      pred_type = "Most Common Word"
    }
  }
  
  if(pred_type=="Trigram")
  {
    predprob[2] <- paste("P(C|A,B) = ",predprob[2],sep="")
  }
  else if (pred_type=="Bigram")
  {
    predprob[2] <- paste("P(C|B) = ",predprob[2],sep="")    
  }
  else
  {
    predprob[2] <- paste("Most Common Word = ",predprob[2],sep="")
  }
  return(list(predprob,pred_type))
}

##########################################################
# shiny server application                               #
##########################################################

shinyServer(function(input, output) {
  # Opening trigram file and checking input has two words
  countstr <- reactive({
    sapply(strsplit(input$wordAB, " "), length)
  })
  wordAB_warning <- reactive({clean(input$wordAB,countstr())})
  # Getting word prediction and most likely word
  get_prediction <- reactive({comb_prediction(wordAB_warning()[1],countstr())})
  # Outputting probabilties, words and models
    output$wordC <- renderPrint({
      if (countstr() >= 1){
        get_prediction()[[1]][[1]]
      }
    })
    output$conprobs <- renderPrint({
      if (countstr() >= 1){
        get_prediction()[[1]][[2]]
      }
    })
    output$predtype <- renderPrint({
      if (countstr() >= 1){
        get_prediction()[[2]]
      }
    })
    output$df <- renderPrint({
      if (countstr() >= 1){
        get_prediction()[[1]][[3]]
      }
    })
    output$warning <- renderPrint({
      if (countstr() >= 1){
        wordAB_warning()[2]
      }
    })
})