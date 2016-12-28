prop_number <- function(prop_test=0.5,freq_ngrams= c(3,2,1)){
     total_ngrams <- sum(freq_ngrams)
     prop_ngrams <- as.vector(freq_ngrams/total_ngrams)
     i <- 1
     prop <- prop_ngrams[1]
     while (prop < prop_test) {
          i <- i + 1
          prop = prop + prop_ngrams[i]
          if (prop > prop_test) {
               return(i)
          }
     }
}