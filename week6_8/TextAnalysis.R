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
library(ggplot2)
library(wordcloud2)
library(knitr)
library(devtools)
library(scales)
library(grid)
library(factoextra)
library(ggbiplot)

#define function toSpace
toSpace <- content_transformer(
  function(x, pattern) gsub(pattern, " ", x))

#apply to tokenizer
jieba_tokenizer = function(d){
  unlist(segment(d[[1]],jiebar = mixseg))
}

#convert Finance Corpus into matrix
dict.csv <- read.csv("FinanceCorpusCH.csv",header = FALSE,encoding = "utf-8")
dict.matrix <- as.matrix(dict.csv)

#import finance term dictionary
mixseg = worker()
new_user_word(mixseg,dict.matrix)
new_user_word(mixseg,"以太坊")
new_user_word(mixseg,"避险天堂")
#construct the Frequency-DataFrame
RawAnalysis <- function(filepath){
  #construct raw corpus
  docs.corpus <- Corpus(DirSource(filepath),
                        readerControl = list(language = "zho"))
  
  # first cleaning process
  docs.corpus <- tm_map(docs.corpus,stripWhitespace)
  docs.corpus <- tm_map(docs.corpus,removeNumbers)
  docs.corpus <- tm_map(docs.corpus,toSpace,"[a-zA-Z]")
  

  
  #split the docs into terms with jiebaR worker
  seg = lapply(docs.corpus,jieba_tokenizer)
  
  #build the TermDocumentMatrix
  freq_fram <- as.data.frame(table(unlist(seg)))
  freq_fram <- freq_fram[-which(nchar(as.character(freq_fram$Var1))<=1),]
  freq_fram <- freq_fram[order(freq_fram$Freq,decreasing = T),]
  n = length(seg)
  
  return(freq_fram)  
}

#get the freq_fram of decline-data
DeclinePath <- "C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week6_8\\text\\decline"
freq_fram_D <- RawAnalysis(DeclinePath)
kable(head(freq_fram_D,10))


#plot the histgram
plot <- freq_fram_D[1:10,1:2]
plot$Var1 <- factor(plot$Var1,levels = plot$Var1[order(-plot$Freq)])
theme_set(theme_bw())
ggplot(plot,aes(x = Var1,y = Freq))+
  geom_bar(stat = "identity",width = 0.5,fill = "tomato3")+
  labs(title = "News Of Declining Gold Price")

#plot the wordcloud
wordcloud2(freq_fram_D)

#get the freq_fram of rise-data
RisePath <- "C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week6_8\\text\\rise"
freq_fram_R <- RawAnalysis(RisePath)
kable(head(freq_fram_R,10))

#plot the histgram
plot <- freq_fram_R[1:10,1:2]
plot$Var1 <- factor(plot$Var1,levels = plot$Var1[order(-plot$Freq)])
theme_set(theme_bw())
ggplot(plot,aes(x = Var1,y = Freq))+
  geom_bar(stat = "identity",width = 0.5,fill = "tomato3")+
  labs(title = "News Of Rising Gold Price")

#plot the wordcloud
wordcloud2(freq_fram_R)


#construct raw corpus
filepath <- "C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week6_8\\text\\whole"
docs.corpus <- Corpus(DirSource(filepath),
                      readerControl = list(language = "zho"))

# first cleaning process
docs.corpus <- tm_map(docs.corpus,stripWhitespace)
docs.corpus <- tm_map(docs.corpus,removeNumbers)
docs.corpus <- tm_map(docs.corpus,toSpace,"[a-zA-Z]")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周五")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周四")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周三")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周二")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周一")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周六")
docs.corpus <- tm_map(docs.corpus,toSpace,"上周日")
docs.corpus <- tm_map(docs.corpus,toSpace,"一月")
docs.corpus <- tm_map(docs.corpus,toSpace,"二月")
docs.corpus <- tm_map(docs.corpus,toSpace,"三月")
docs.corpus <- tm_map(docs.corpus,toSpace,"四月")
docs.corpus <- tm_map(docs.corpus,toSpace,"五月")
docs.corpus <- tm_map(docs.corpus,toSpace,"六月")
docs.corpus <- tm_map(docs.corpus,toSpace,"七月")
docs.corpus <- tm_map(docs.corpus,toSpace,"八月")
docs.corpus <- tm_map(docs.corpus,toSpace,"九月")
docs.corpus <- tm_map(docs.corpus,toSpace,"十月")
docs.corpus <- tm_map(docs.corpus,toSpace,"十一月")
docs.corpus <- tm_map(docs.corpus,toSpace,"十二月")
docs.corpus <- tm_map(docs.corpus,toSpace,"该行")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期一")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期二")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期三")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期四")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期五")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期六")
docs.corpus <- tm_map(docs.corpus,toSpace,"星期天")
docs.corpus <- tm_map(docs.corpus,toSpace,"今晚")


#split the docs into terms with jiebaR worker
seg = lapply(docs.corpus,jieba_tokenizer)

#build the TermDocumentMatrix
count_token = function(d)
{
  as.data.frame(table(d))
}
freq_fram <- lapply(seg,count_token)
for(id in 1:length(freq_fram))
{
  freq_fram[[id]] <- freq_fram[[id]][-which(nchar(as.character(freq_fram[[id]]$d))<=1),]
}
#freq_fram <- freq_fram[order(freq_fram$Freq,decreasing = T),]
n = length(seg)
TDM <- freq_fram[[1]]
colNames <- names(seg)
colNames <- gsub(".txt","",colNames)
kable(head(TDM))
for(id in c(2:n))
{
  TDM = merge(TDM,freq_fram[[id]],by = "d",all = TRUE)
  names(TDM) = c('d',colNames[1:id])
}
TDM[is.na(TDM)] <- 0
names(TDM) = c('d',colNames[2:n])

kable(head(TDM))

#function to calculate idf
idfCal <- function(word_doc)
{
  log2(n / nnzero(word_doc))
}
idf <- apply(as.matrix(TDM[,2:(n+1)]),1,idfCal)
tf <- apply(as.matrix(TDM[,2:(n+1)]),2,sum)

doc.tfidf <- TDM

tempY <- matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX <- matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX
stopLine <- rowSums(doc.tfidf[,2:(n+1)])
delID <- which(stopLine == 0)

TDM <- TDM[-delID,]
doc.tfidf <- doc.tfidf[-delID,]
TopWords = data.frame()
for( id in c(1:n) ){
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:5],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colNames[1:n]
TopWords = droplevels(TopWords)
View(TopWords)


TopWords_R <- TopWords[a<-c(3,4,6,7,8,10,14,15,16,21,22,24,25,26),]
kable(TopWords_R)

TopWords_D <- TopWords[-a,]
kable(TopWords_D)

pca.tfidf <- subset(doc.tfidf,select = -d)



 
 library(ggfortify)
 
 word.doc.tfidf <- subset(doc.tfidf,select = -d)
 row.names(word.doc.tfidf) <- doc.tfidf$d
 pcs.date <- prcomp(t(word.doc.tfidf), center = T, scale = T)

 fviz_eig(pcs.date)
 fviz_pca_var(pcs.date, col.var = "contrib")
 str(pcs.date)
 k.date <- 2
 princomp.date <- data.frame(pcs.date$x[,1:5])
 View(princomp.date)
 km.date <- kmeans(princomp.date,centers = k.date,nstart=26, iter.max=1000)
 autoplot(km.date, data = pcs.date, label = TRUE, label.size = 3)
 