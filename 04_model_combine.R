model_combine <- function(phrase="test"){
     ngram <- length(strsplit(phrase, " ")[[1]])
     if(ngram==4){
          return(df_quin[df_quin$df.wordABCD==phrase,])     
     }
     else if(ngram==3){
          return(df_quad[df_quad$df.wordABC==phrase,])
     }
     else if(ngram==2){
          return(df_tri[df_tri$df.wordAB==phrase,])
     }
     else{
          return(df_bi[df_bi$df.wordA==phrase,])
     }
}