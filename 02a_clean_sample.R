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
     twitter_index <- sample(1:nl_twitter,nl,replace=FALSE)
     news_index <- sample(1:nl_news,nl,replace=FALSE)
     blogs_index <- sample(1:nl_blogs,nl,replace=FALSE)
     #########################################################
     # Extracting twitter data for both training and testing #
     #########################################################
     
     twitter_con <- file('data/final/en_US/en_US.twitter_clean.txt', 'r')
     twitter_lines <- rep("",nl)
     twitter_line_num <- rep(0,nl)
     
     j=0
     
     for (i in 1:nl_twitter) {
       if (i %in% twitter_index){
         j = j + 1
         twitter_lines[j] <- tolower(readLines(twitter_con,n=1))
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
     
     news_con <- file('data/final/en_US/en_US.news_clean.txt', 'r')
     news_lines <- rep("",nl)
     news_line_num <- rep(0,nl)
     
     j=0
     
     for (i in 1:nl_news) {
       if (i %in% news_index){
         j = j + 1
         news_lines[j] <- tolower(readLines(news_con,n=1))
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
     
     blogs_con <- file('data/final/en_US/en_US.blogs_clean.txt', 'r')
     blogs_lines <- rep("",nl)
     blogs_line_num <- rep(0,nl)
     
     j=0
     
     for (i in 1:nl_blogs) {
       if (i %in% blogs_index){
         j = j + 1
         blogs_lines[j] <- tolower(readLines(blogs_con,n=1))
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
     
     # Appending sentences with start and end for model testing
     
     ####################################################
     # Creating data frames and writting data to a file #
     ####################################################
     
     twitter_df <- data.frame(twitter_line_num,twitter_lines)
     news_df <- data.frame(news_line_num,news_lines)
     blogs_df <- data.frame(blogs_line_num,blogs_lines)
     
     if(!file.exists("data/sample_data")){
        system("mkdir data/sample_data")
     }
     
     twitter_fn <- paste("data/sample_data/twitter_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     news_fn <- paste("data/sample_data/news_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     blogs_fn <- paste("data/sample_data/blogs_l",as.character(nl),"_s",as.character(seed),".csv",sep="")
     
     write.csv(twitter_df,twitter_fn)
     write.csv(news_df,news_fn)
     write.csv(blogs_df,blogs_fn)
     
     df_data_files <- data.frame(Blog.file=c(nl_blogs,nw_blogs,nb_blogs),
                                 News.file=c(nl_news,nw_news,nb_news),
                                 Twitter.file=c(nl_blogs,nw_blogs,nb_blogs))
     colnames(df_data_files)<-c("Blog File","News File","Twitter File")
     rownames(df_data_files)<-c("Number of Lines","Number of Words","Number of Bytes")
     return(df_data_files)
}
     

