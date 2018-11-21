library(shiny)
library(rvest)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(NLP)
library(ggplot2)
library(htm2txt)
library(stats)
library(proxy)
library(dplyr)
library(readtext)
library(jiebaRD)
library(jiebaR)
library(slam)
library(Matrix)
library(tidytext)
library(wordcloud2)
library(knitr)
library(devtools)
library(scales)
library(grid)
library(factoextra)
library(ggbiplot)
library(DT)

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
kable(TopWords)

pca.tfidf <- subset(doc.tfidf,select = -d)




library(ggfortify)

word.doc.tfidf <- subset(doc.tfidf,select = -d)
row.names(word.doc.tfidf) <- doc.tfidf$d
pcs.date <- prcomp(t(word.doc.tfidf), center = T, scale = T)

fviz_eig(pcs.date)
#ggbiplot(pcs.date, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
fviz_pca_var(pcs.date, col.var = "contrib")
str(pcs.date)
k.date <- 3
# princomp.date <- data.frame(pcs.date$x[,1:5])
# View(princomp.date)
# km.date <- kmeans(princomp.date,centers = k.date,nstart=26, iter.max=1000)
# autoplot(km.date, data = pcs.date, label = TRUE, label.size = 3)
# 

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
ui <- navbarPage(
  
   
   # Application title
   "Gold News Analysis",
   
   #set tab panel
   tabPanel(
     "简介",
     tags$h2("金价新闻与走势分析报告"),br(),
     tags$h3("报告人：洪世豪"),br(),
     tags$h3("因之前受贸易战影响，素有避险光环的黄金市场波动不小。"),br(),
     tags$h3("想法有二，一是想看看能否透过前一天的新闻热词看出第二天黄金涨跌的情况。"),br(),
     tags$h3("二是检验提取的关键字能否有效聚类。本次作业爬取了新浪新闻财经板块的九月份有关金价的新闻内容，以日期为单位收录。")
   ),
   tabPanel(
     "直方图",
     tags$h2("Bar graph for two sets of news words"),br(),
     sidebarPanel(
       selectInput("Corpus_B","text:",choices = c("涨价","跌价")),
       hr(),
       helpText("跌金日和涨金日前一天的热词有哪些")
     ),
     mainPanel(
       plotOutput("Plot_Bar")
     )
   ),
   tabPanel(
     "词云",
     tags$h2("WordCloud for two set of news words"),br(),
     sidebarPanel(
       selectInput("Corpus_W","text:",choices = c("涨价","跌价"))
     ),
     # sidebarPanel(
     #  
     # ),
     mainPanel(
       plotOutput("WordCloud")
     )
   ),
   tabPanel(
     "关键字",
     tags$h2("经过TF-IDF后抓出每个文本的最有代表性的n个词"),br(),
     sidebarPanel(
       selectInput("Corpus_T","text:",choices = c("涨价","跌价")),
       sliderInput("topN","number of the representitive words",
                   min = 1,max = 10,value = 5)
     ),
     mainPanel(dataTableOutput("table"))
   ),
   tabPanel(
     "PCA降维",
     tags$h2("PCA降维后的各个维度的贡献图"),br(),
     mainPanel(
       plotOutput("Plot_PCA")
     )
   ),
   tabPanel(
     "K-Means Clustering",
     tags$h2("Plot K-Means"),br(),
     tags$h3("取PCA降维结果的前5项将text聚两类"),
     mainPanel(
       img(src="kmeans.png", height = 400, width = 400)
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot_Bar <- renderPlot({
    select <- input$Corpus_B
    if(select == "涨价"){
      plot <- freq_fram_R[1:10,1:2]
      title <- "Rising"
    }else{
      plot <- freq_fram_D[1:10,1:2]
      title <-"Decling"
    }
    plot$Var1 <- factor(plot$Var1,levels = plot$Var1[order(-plot$Freq)])
    theme_set(theme_bw())
    print(
      ggplot(plot,aes(x = Var1,y = Freq))+
      geom_bar(stat = "identity",width = 0.5,fill = "tomato3")+
      labs(title = paste("News Of",title,"Gold Price",sep = " "))
    )
    
  })
  output$WordCloud <- renderPlot({
    select <- input$Corpus_W
    if(select == "涨价"){
      img(src="wordcloud_R.png", height = 400, width = 400)
    }else{
      img(src="wordcloud_D.png", height = 400, width = 400)
    }
  })
  
  output$table <- DT::renderDataTable({
    topN <- as.numeric(input$topN)
    TopWords = data.frame()
    for( id in c(1:n) ){
      dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
      showResult = t(as.data.frame(doc.tfidf[dayMax[1:topN],1]))
      TopWords = rbind(TopWords, showResult)
    }
    rownames(TopWords) = colNames[1:n]
    select <- input$Corpus_T
    TopWords_R <- TopWords[a<-c(3,4,6,7,8,10,14,15,16,21,22,24,25,26),]
    TopWords_D <- TopWords[-a,]
    if(select == "涨价"){
      table <- TopWords_R
    }else{
      table <- TopWords_D
    }
    table = droplevels(table)
    DT::datatable(table)
  })
  output$Plot_PCA <- renderPlot({
    pcs.date <- prcomp(t(word.doc.tfidf), center = T, scale = T)
    
    fviz_eig(pcs.date)
  })
  output$kmeans <- renderImage({
    
      src <- "image//kmeans.png"
    
    return(list(
      src,
      height = 300,
      contentType = "image/png",
      alt = "icon"
    ))    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

