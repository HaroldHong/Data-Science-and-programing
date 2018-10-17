library(XML)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)

  collecturl1 <- function(i){
  srcURL <- paste("http://search.sina.com.cn/?c=news&q=%BB%C6%BD%F0&range=all&time=custom&stime=2018-09-0",i,"&etime=2018-09-0",i,"&num=20&col=1_7&source=&from=&country=&size=&a=&sort=rel",sep = "")
  srcURL
  u1 <- htmlParse(srcURL,encoding = 'UTF-8')
  url <- xpathApply(u1,'//div[@class="box-result clearfix"]/*/*/a',xmlAttrs)
  URL <- unlist(lapply(url,str_extract_all,'http://finance\\S+'))
  goldNews <- data.frame(URL)
  csvpath <- paste("C://Users//Harold//Documents//GitHub//Data-Science-and-programing//week5//news_09",i,".csv",sep = "")
  write.csv(goldNews,file = csvpath,row.names = F)
}
  mapply(collecturl1,5)
  
  collecturl2 <- function(i){
  srcURL <- paste("http://search.sina.com.cn/?c=news&q=%BB%C6%BD%F0&range=all&time=custom&stime=2018-09-",i,"&etime=2018-09-",i,"&num=20&col=1_7&source=&from=&country=&size=&a=&sort=rel",sep = "")
  print(srcURL)
  u1 <- htmlParse(srcURL,encoding = 'UTF-8')
  url <- xpathApply(u1,'//div[@class="box-result clearfix"]/*/*/a',xmlAttrs)
  URL <- unlist(lapply(url,str_extract_all,'http://finance\\S+'))
  goldNews <- data.frame(URL)
  csvpath <- paste("C://Users//Harold//Documents//GitHub//Data-Science-and-programing//week5//news_09",i,".csv",sep = "")
  write.csv(goldNews,file = csvpath,row.names = F)
  }
  mapply(collecturl2,27)

  
  
  
  
  