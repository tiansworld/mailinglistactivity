library(XML)
library(RCurl)
library(htmltools)
library(zoo)
library(ggplot2)


## Get the mailing list archieve page url
url <- c("https://lists.fedoraproject.org/pipermail/trans/")

## Store the page html to transhtml
transhtml <- getURL(url)
#transtable <- getNodeSet(htmlParse(transhtml),"//table")

## Convert html code to R list
finaltrans <- xpathApply(htmlTreeParse(transhtml,useInternalNodes = T),"//td",
                         function(x) xmlValue(x))

## Set the list index that the content will be extracted. Here extract the 
## archieve month and year, and the size of the archieve.
filtermonth <- seq(4,423,3)
filtersize <- seq(6,423,3)

## Extract the month, year and archieve size, then change the extracted lists to 
## vectors
month <- sapply(finaltrans[filtermonth], function(i) i[1])
size <- sapply(finaltrans[filtersize], function(i) i[1])

## Remove semicolons in month column
month <- as.character(sapply(month, function(i) gsub(":","",i)))
## Extract size number from size column
size <- as.numeric(sapply(size,function(i) gsub("[^0-9]+","",i)))

## Built a data frame contains archieve year, month and size
archieveyear <- as.factor(year(as.yearmon(trans$month)))
archievemonth <- as.factor(month(as.yearmon(trans$month)))
trans <- data.frame(year=archieveyear,month=archievemonth,size=trans$size)

## Plot
fig <- ggplot(trans) + aes(x=month,y=size,group=year,color=year) +
        facet_wrap(~ year) + geom_point()
ggsave("trans_archieve.png",fig)