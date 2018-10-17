library('xml2')
library('rvest')
library('dplyr')

collectTEXT <- function(url,i){
  url_char <- as.character(url)
  print(url_char)
  
  web <- read_html(url_char,encoding = 'UTF-8')
  text_node <- html_nodes(web,"#artibody p")
  text_info <- html_text(text_node)
  filepath <- paste("C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week5\\text09",i,".txt",sep = "")
  cat(text_info, file = filepath,append = TRUE)
  
}
traverseTEXT <- function(i){
  urlSetPath = "C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week5\\news_09"
    urlSetPath <- paste(urlSetPath,i,".csv",sep = "")
  urlframe <- read.csv(urlSetPath)
  mapply(collectTEXT,urlframe$URL,i)
}
mapply(traverseTEXT,27:30)
