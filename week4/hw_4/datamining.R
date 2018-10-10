library(readxl)
library(jiebaRD)
library(jiebaR)
library(zoo)
library(plyr)
library(wordcloud2)
library(tm)
library(NLP)
#get top num
topsort <- function(x,num = 0)
{
  top <- count(x)
  top <- top[order(top$freq,decreasing = T),]
  if(num > 0){
    return(top[1:num,])
  }
  else return(top)
}

jobdf <- read_excel("C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week4\\hw_4\\jobdata.xlsx",sheet = 1)
jobdf <- jobdf[,c("scale","scale2","salary","title","experience",
                  "education","campany","description","phase","city" )]
mydata <- jobdf
mydata$description <- tolower(mydata$description)

mydata$description <- gsub('以上学历|优先|岗位职责|年','',mydata$description)
mydata$description <- gsub('数据|分析|挖掘|中|强|使用|负责|相关','',mydata$description)
mydata$description <- gsub('能力|工作|进行|具有|日常|完成|具备|业务','',mydata$description)

# mycorpus <- Corpus(mydata,readerControl = list(reader = mydata,language = "en"))
# mymap <- tm_map(mydata$description,stemDocument)

keys <- worker(type = "keywords",
               user ="jieba.dict",
               topn = 30,
               encoding = 'UTF-8',
               stop_word ="C:/Users/Harold/Documents/R/win-library/3.5/jiebaRD/dict/stop_words.utf8")
keyword.list <- lapply(mydata$description,function(x){
              temp <- gsub("[0-9|\\.|\\-]","",keywords(x,keys))
              temp <- temp[temp!=""]
              return(temp)
})
keyword.list <- unlist(keyword.list)
finalkeyword <- topsort(keyword.list)
str(finalkeyword)

wordcloud2(finalkeyword,size = 0.9,fontFamily = '微软雅黑')