gen_sample <- function(seed=100,nl=1000){
     
     #################################################
     # Extracting the number of lines from each file #
     #################################################
     
     system("wc -l data/final/en_US/en_US.twitter.txt > data/lines.txt")
     system("wc -l data/final/en_US/en_US.news.txt >> data/lines.txt")
     system("wc -l data/final/en_US/en_US.blogs.txt >> data/lines.txt")
     
     # loading number of lines for each file
     
     nl_con <- file('data/lines.txt','r')
     nl_content <- readLines(nl_con)
     nl_twitter <- as.numeric(strsplit(nl_content," ")[[1]][1])
     nl_news <- as.numeric(strsplit(nl_content," ")[[2]][1])
     nl_blogs <- as.numeric(strsplit(nl_content," ")[[3]][1])
     close(nl_con)

     #######################################################
     # Setting random seeds and sample index for each file #
     #######################################################
     
     # Number of lines to read
     set.seed(seed)
     twitter_index <- sample(1:nl_twitter,nl,replace=FALSE)
     news_index <- sample(1:nl_news,nl,replace=FALSE)
     blogs_index <- sample(1:nl_blogs,nl,replace=FALSE)
     #####################################
     # Extracting twitter data           #
     #####################################
     
     twitter_con <- file('data/final/en_US/en_US.twitter.txt', 'r')
     twitter_lines <- rep("",nl)
     twitter_line_num <- rep(0,nl)
     
     j=0
     for (i in 1:nl_twitter) {
          if (i %in% twitter_index){
               j = j + 1
               twitter_lines[j] <- readLines(twitter_con,n=1)
               twitter_line_num[j] <- i
          } 
          else
          {
               temp <- readLines(twitter_con,n=1)
          }
     }
     
     close(twitter_con)
     
     #####################################
     # Extracting news data              #
     #####################################
     
     news_con <- file('data/final/en_US/en_US.news.txt', 'r')
     news_lines <- rep("",nl)
     news_line_num <- rep(0,nl)
     
     j=0
     for (i in 1:nl_news) {
          if (i %in% news_index){
               j = j + 1
               news_lines[j] <- readLines(news_con,n=1)
               news_line_num[j] <- i
          } 
          else
          {
               temp <- readLines(news_con,n=1)
          }
     }
     
     close(news_con)
     
     #####################################
     # Extracting blogs data             #
     #####################################
     
     blogs_con <- file('data/final/en_US/en_US.blogs.txt', 'r')
     blogs_lines <- rep("",nl)
     blogs_line_num <- rep(0,nl)
     
     j=0
     for (i in 1:nl_blogs) {
          if (i %in% blogs_index){
               j = j + 1
               blogs_lines[j] <- readLines(blogs_con,n=1)
               blogs_line_num[j] <- i
          } 
          else
          {
               temp <- readLines(blogs_con,n=1)
          }
     }
     
     close(blogs_con)
     
     #######################################
     # Removing bad words from each vector #
     #######################################
     
     badwords_con <- file('data/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt', 'r')
     badwords <- readLines(badwords_con)
     close(badwords_con)
     badwords <- append(badwords,"fuck")
     
     # Converting to ASCII
     badwords <- iconv(badwords, from = "", to="ASCII", sub="")
     # ordering badwords
     dec_ord <- order(nchar(badwords),decreasing=TRUE)
     badwords <- badwords[dec_ord]
     
     # Removing all bad words
     
     for (word in badwords) {
          twitter_lines <- gsub(word,"",twitter_lines,ignore.case=TRUE)
          news_lines <- gsub(word,"",news_lines,ignore.case=TRUE)
          blogs_lines <- gsub(word,"",blogs_lines,ignore.case=TRUE)
     }
     
     ####################################################
     # Creating data frames and writting data to a file #
     ####################################################
     
     twitter_df <- data.frame(twitter_line_num,twitter_lines)
     news_df <- data.frame(news_line_num,news_lines)
     blogs_df <- data.frame(blogs_line_num,blogs_lines)
     
     twitter_fn <- paste("data/sample_data/twitter_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     news_fn <- paste("data/sample_data/news_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     blogs_fn <- paste("data/sample_data/blogs_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     
     write.csv(twitter_df,twitter_fn)
     write.csv(news_df,news_fn)
     write.csv(blogs_df,blogs_fn)     
}
     

