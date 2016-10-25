# Creating directory for the data

system("mkdir data")

# Downloading Swift Key Data

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
data <- "data/Coursera-SwiftKey.zip"
download.file(url, data, method="wget")
unzip(data,exdir="data")

url <- "http://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip"
data <- "data/full-list-of-bad-words-banned-by-google-txt-file.zip"
download.file(url, data, method="wget")
unzip(data,exdir="data")




