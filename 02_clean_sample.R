# Extracting the number of lines from each file

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

# Parameters

# Number of lines read
nl = 1000

# reading data
twitter_con <- file('data/final/en_US/en_US.twitter.txt', 'r',fileEncoding=getOption("latin1"))
news_con <- file('data/final/en_US/en_US.news.txt', 'r',fileEncoding=getOption("latin1"))
blogs_con <- file('data/final/en_US/en_US.blogs.txt', 'r',fileEncoding=getOption("latin1"))

twitter_lines <-readLines(twitter_con,n=nl)
news_lines <-readLines(news_con,n=nl)
blogs_lines <-readLines(blogs_con,n=nl)

close(twitter_con)
close(news_con)
close(blogs_con)

# Reading google "bad words" data

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

# outputing data

twitter_con <- file('data/sample_data/en_US.twitter.txt', 'w')
news_con <- file('data/sample_data/en_US.news.txt', 'w')
blogs_con <- file('data/sample_data/en_US.blogs.txt', 'w')

writeLines(twitter_lines, con = twitter_con)
writeLines(news_lines, con = news_con)
writeLines(blogs_lines, con = blogs_con)

close(twitter_con)
close(news_con)
close(blogs_con)
