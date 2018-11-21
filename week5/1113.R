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



docs.corpus <- Corpus(DirSource("C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week5\\text\\total"),readerControl = list(language = "zho"))

docs.corpus <- tm_map(docs.corpus,toSpace, "黄金")
docs.corpus <- tm_map(docs.corpus,stripWhitespace)
docs.corpus <- tm_map(docs.corpus,removeNumbers)
docs.corpus <- tm_map(docs.corpus,toSpace,"[a-zA-Z]")
#import dictionary
keywords <- read.csv("FinanceCorpusCH.csv",header = FALSE, encoding = "utf-8")
mixseg = worker()
keys <- as.matrix(keywords)
new_user_word(mixseg,keys)

jieba_tokenizer = function(d){
  unlist(segment(d[[1]], jiebar = mixseg))
}
seg = lapply(docs.corpus, jieba_tokenizer)
freq_fram <- as.data.frame(table(unlist(seg)))

n = length(seg)
colnames <- names(seg)
colnames <- gsub(".txt","",colnames)
freq_fram <- freq_fram[order(freq_fram$Freq,decreasing = T),]
#剔除单个字符
freq_fram <- freq_fram[-which(nchar(as.character(freq_fram$Var1))==1),]

d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
print(tf <- as.matrix(tdm))
DF <- tidy(tf)

#tf-idf computation
N = tdm$ncol
tf <- apply(tdm,2,sum)
idfCal <- function(word_doc)
{
  log2(N/nnzero(word_doc))
}
idf <- apply(tdm,1,idfCal)
doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y]/tf[y]) * idf[x]
  }
}
findZeroId  = as.matrix(apply(doc.tfidf,1,sum))
tfidfnn = doc.tfidf[-which(findZeroId ==0),]
write.csv(tfidfnn,"show.csv")


