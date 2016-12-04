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

# Listing files in the data directory

print("zip files downloaded:")
print("")
list.files("data",pattern="zip")

# Extracting the data

zipfile <- "data/Coursera-SwiftKey.zip"
if(!file.exists("data/final"))
{
     unzip("data/Coursera-SwiftKey.zip",exdir="data")
}
     
txtfile <- "data/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
zipfile <- "data/full-list-of-bad-words-banned-by-google-txt-file.zip"
if(!file.exists(txtfile))
{
     unzip(zipfile,exdir="data")
}

print("")
print("extracted data file")
print("")
list.files("data/final/en_US")
list.files("data",pattern = "txt")




