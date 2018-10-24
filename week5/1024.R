library(NLP)
library(tm)
library(stats)
library(proxy)
library(dplyr)
library(readtext)
library(jiebaRD)
library(jiebaR)
library(slam)
library(Matrix)
library(tidytext)

topsort <- function(x,num = 0)
{
  top <- count(x);
  top <- top[order(top$freq,decreasing = T),]
  if(num > 0){
    return(top[1:num,])
  }
  else return(top)
}



docs <- Corpus(DirSource("C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week5\\text\\decline"),readerControl = list(language = "zho"))
#two methods to construct the corpus
# rawdata<- readtext("*.txt")
# summary(rawdata)
# text <- Corpus(VectorSource(rawdata$text))
# summary(text)
# writeCorpus(docs,path = "D:\\Corpus")

writeLines(as.character(docs[[1]]))

docs <- tm_map(docs,toSpace, "黄金")
docs <- tm_map(docs,stripWhitespace)
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,stripWhitespace)
docs <- tm_map(docs,stripWhitespace)


mixseg = worker(stop_word = "C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week5\\mystopwords.txt")

jieba_tokenizer = function(d){
  unlist(segment(d[[1]], jiebar = mixseg))
}
seg = lapply(docs, jieba_tokenizer)

freq_fram <- as.data.frame(table(unlist(seg)))
freq_fram <- freq_fram[order(freq_fram$Freq,decreasing = T),]
#剔除单个字符
freq_fram <- freq_fram[-which(nchar(as.character(freq_fram$Var1))==1),]


summary(seg)
d.corpus <- Corpus(VectorSource(seg))
s.dtm <- TermDocumentMatrix(d.corpus)
inspect(s.dtm)

tf <- apply(s.dtm,2,sum)

library(RColorBrewer)
library(wordcloud2)
m1 <- as.matrix(s.dtm)
v <- sort(rowSums(m1),decreasing = T)
d <- data.frame(word = names(v),freq = v)
#wordcloud(d$word,d$freq,min.freq = 20 , random.order = F,ordered.colors = F,
#          clolors = rainbow(length(row.names(m1))))
seg.unlist <- unlist(seg)
#cloudword <- topsort(seg.unlist)
wordcloud2(freq_fram,size = 0.9,fontFamily = '微软雅黑')


# # tf-idf computation
# tf <- apply(tdm, 2, sum) # term frequency
# idf <- function(word_doc){ log2( (length(word_doc)+1) / nnzero(word_doc) ) }
# idf <- apply(tdm, 1, idf)
# doc.tfidf <- as.matrix(tdm)
# for(i in 1:nrow(tdm)){
#   for(j in 1:ncol(tdm)){
#     doc.tfidf[i,j] <- (doc.tfidf[i,j] / tf[j]) * idf[i]
#   }
# }