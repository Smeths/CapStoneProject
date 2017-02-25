suppressMessages(library(knitr))

# Creating directory for the data

destfile <- "data"

if(!file.exists(destfile))
{
     system("mkdir data")     
}

# Downloading Swift Key Data

destfile <- "data/Coursera-SwiftKey.zip"

if(!file.exists(destfile))
{
     url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
     download.file(url, destfile, method="wget")
     unzip(data,exdir="data")
}

# Downloading google bad words list

destfile <- "data/full-list-of-bad-words-banned-by-google-txt-file.zip"

if(!file.exists(destfile))
{
     url <- "http://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip"
     download.file(url, destfile, method="wget")
     unzip(data,exdir="data")
}

# Extracting the data and removing null characters

zipfile <- "data/Coursera-SwiftKey.zip"
if(!file.exists("data/final"))
{
     unzip("data/Coursera-SwiftKey.zip",exdir="data")
# Removing null and non ascii characters
     file <- 'data/final/en_US/en_US.blogs.txt'
     newfile <- 'data/final/en_US/en_US.blogs_clean.txt'
     writeLines(iconv(readLines(file,skipNul = TRUE),from="UTF-8",to="ASCII",sub=""),newfile)
     file <- 'data/final/en_US/en_US.news.txt'
     newfile <- 'data/final/en_US/en_US.news_clean.txt'
     writeLines(iconv(readLines(file,skipNul = TRUE),from="UTF-8",to="ASCII",sub=""),newfile)
     file <- 'data/final/en_US/en_US.twitter.txt'
     newfile <- 'data/final/en_US/en_US.twitter_clean.txt'
     writeLines(iconv(readLines(file,skipNul = TRUE),from="UTF-8",to="ASCII",sub=""),newfile)
}
     
txtfile <- "data/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
zipfile <- "data/full-list-of-bad-words-banned-by-google-txt-file.zip"
if(!file.exists(txtfile))
{
     unzip(zipfile,exdir="data")
}

zip_files <- list.files("data",pattern="zip")
data_files <- list.files("data/final/en_US")
txt_files <- list.files("data",pattern = "txt")

df_data_files <- data.frame(Blog.file=data_files[1],News.file=data_files[2],Twitter.file=data_files[3])
colnames(df_data_files)<-c("Blog File","News File","Twitter File")




