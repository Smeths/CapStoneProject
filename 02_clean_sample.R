gen_sample <- function(seed=100,nl=1000){
     
     #################################################
     # Extracting the number of lines from each file #
     #################################################
     
     system("wc -l data/final/en_US/en_US.twitter.txt > data/lines.txt")
     system("wc -l data/final/en_US/en_US.news.txt >> data/lines.txt")
     system("wc -l data/final/en_US/en_US.blogs.txt >> data/lines.txt")
     system("wc -w data/final/en_US/en_US.twitter.txt >> data/lines.txt")
     system("wc -w data/final/en_US/en_US.news.txt >> data/lines.txt")
     system("wc -w data/final/en_US/en_US.blogs.txt >> data/lines.txt")
     system("wc -c data/final/en_US/en_US.twitter.txt >> data/lines.txt")
     system("wc -c data/final/en_US/en_US.news.txt >> data/lines.txt")
     system("wc -c data/final/en_US/en_US.blogs.txt >> data/lines.txt")
     
     # loading number of lines for each file
     
     nl_con <- file('data/lines.txt','r')
     nl_content <- readLines(nl_con)
     nl_twitter <- as.numeric(strsplit(nl_content," ")[[1]][1])
     nl_news <- as.numeric(strsplit(nl_content," ")[[2]][1])
     nl_blogs <- as.numeric(strsplit(nl_content," ")[[3]][1])
     nw_twitter <- as.numeric(strsplit(nl_content," ")[[4]][1])
     nw_news <- as.numeric(strsplit(nl_content," ")[[5]][1])
     nw_blogs <- as.numeric(strsplit(nl_content," ")[[6]][1])
     nb_twitter <- as.numeric(strsplit(nl_content," ")[[7]][1])
     nb_news <- as.numeric(strsplit(nl_content," ")[[8]][1])
     nb_blogs <- as.numeric(strsplit(nl_content," ")[[9]][1])
     close(nl_con)
     
     #######################################################
     # Setting random seeds and sample index for each file #
     #######################################################
     
     # Number of lines to read
     set.seed(seed)
     twitter_index <- sample(1:nl_twitter,2*nl,replace=FALSE)
     news_index <- sample(1:nl_news,2*nl,replace=FALSE)
     blogs_index <- sample(1:nl_blogs,2*nl,replace=FALSE)
     #########################################################
     # Extracting twitter data for both training and testing #
     #########################################################
     
     twitter_con <- file('data/final/en_US/en_US.twitter_clean.txt', 'r')
     train_twitter_lines <- rep("",nl)
     test_twitter_lines <- rep("",nl)
     train_twitter_line_num <- rep(0,nl)
     test_twitter_line_num <- rep(0,nl)
     
     jtrain=0
     jtest=0
     train = TRUE
     
     for (i in 1:nl_twitter) {
       if (i %in% twitter_index){
         if(train){
           jtrain = jtrain + 1
           train_twitter_lines[jtrain] <- readLines(twitter_con,n=1)
           train_twitter_line_num[jtrain] <- i
           train = FALSE
         }
         else 
         {
           jtest = jtest + 1
           test_twitter_lines[jtest] <- readLines(twitter_con,n=1)
           test_twitter_line_num[jtest] <- i
           train = TRUE                   
         }
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
     
     news_con <- file('data/final/en_US/en_US.news_clean.txt', 'r')
     train_news_lines <- rep("",nl)
     test_news_lines <- rep("",nl)
     train_news_line_num <- rep(0,nl)
     test_news_line_num <- rep(0,nl)
     
     jtrain=0
     jtest=0
     train = TRUE
     
     for (i in 1:nl_news) {
       if (i %in% news_index){
         if(train){
           jtrain = jtrain + 1
           train_news_lines[jtrain] <- readLines(news_con,n=1)
           train_news_line_num[jtrain] <- i
           train = FALSE
         }
         else
         {
           jtest = jtest + 1
           test_news_lines[jtest] <- readLines(news_con,n=1)
           test_news_line_num[jtest] <- i
           train = TRUE                   
         }
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
     
     blogs_con <- file('data/final/en_US/en_US.blogs_clean.txt', 'r')
     train_blogs_lines <- rep("",nl)
     test_blogs_lines <- rep("",nl)
     train_blogs_line_num <- rep(0,nl)
     test_blogs_line_num <- rep(0,nl)
     
     jtrain=0
     jtest=0
     train = TRUE
     
     j=0
     for (i in 1:nl_blogs) {
       if (i %in% blogs_index){
         if(train){
           jtrain = jtrain + 1
           train_blogs_lines[jtrain] <- readLines(blogs_con,n=1)
           train_blogs_line_num[jtrain] <- i
           train = FALSE
         }
         else
         {
           jtest = jtest + 1
           test_blogs_lines[jtest] <- readLines(blogs_con,n=1)
           test_blogs_line_num[jtest] <- i
           train = TRUE                   
         }
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
          train_twitter_lines <- gsub(word,"",train_twitter_lines,ignore.case=TRUE)
          train_news_lines <- gsub(word,"",train_news_lines,ignore.case=TRUE)
          train_blogs_lines <- gsub(word,"",train_blogs_lines,ignore.case=TRUE)
          test_twitter_lines <- gsub(word,"",test_twitter_lines,ignore.case=TRUE)
          test_news_lines <- gsub(word,"",test_news_lines,ignore.case=TRUE)
          test_blogs_lines <- gsub(word,"",test_blogs_lines,ignore.case=TRUE)
     }
     
     ####################################################
     # Creating data frames and writting data to a file #
     ####################################################
     
     train_twitter_df <- data.frame(train_twitter_line_num,train_twitter_lines)
     train_news_df <- data.frame(train_news_line_num,train_news_lines)
     train_blogs_df <- data.frame(train_blogs_line_num,train_blogs_lines)
     test_twitter_df <- data.frame(test_twitter_line_num,test_twitter_lines)
     test_news_df <- data.frame(test_news_line_num,test_news_lines)
     test_blogs_df <- data.frame(test_blogs_line_num,test_blogs_lines)
     
     if(!file.exists("data/sample_data")){
        system("mkdir data/sample_data")
     }
     
     train_twitter_fn <- paste("data/sample_data/train_twitter_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     train_news_fn <- paste("data/sample_data/train_news_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     train_blogs_fn <- paste("data/sample_data/train_blogs_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     test_twitter_fn <- paste("data/sample_data/test_twitter_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     test_news_fn <- paste("data/sample_data/test_news_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     test_blogs_fn <- paste("data/sample_data/test_blogs_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     
     write.csv(train_twitter_df,train_twitter_fn)
     write.csv(train_news_df,train_news_fn)
     write.csv(train_blogs_df,train_blogs_fn)
     write.csv(test_twitter_df,test_twitter_fn)
     write.csv(test_news_df,test_news_fn)
     write.csv(test_blogs_df,test_blogs_fn)
     
     df_data_files <- data.frame(Blog.file=c(nl_blogs,nw_blogs,nb_blogs),
                                 News.file=c(nl_news,nw_news,nb_news),
                                 Twitter.file=c(nl_blogs,nw_blogs,nb_blogs))
     colnames(df_data_files)<-c("Blog File","News File","Twitter File")
     rownames(df_data_files)<-c("Number of Lines","Number of Words","Number of Bytes")
     return(df_data_files)
}
     

