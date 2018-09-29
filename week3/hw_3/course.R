library("ggplot2")
mydata = read.csv("house_info.csv")
#my.plot <- ggplot(data = mydata,aes(x = 面积,y = 月租,colour=房屋结构))+geom_point()
var <- mydata$朝向 #the categorical data--orientation of the room

nrows <- 15
df <- expand.grid(y = 1:nrows,x = 1:nrows)
categ_table <- round(table(var)*((nrows^2)/(length(var))))
df$category <- factor(rep(names(categ_table),categ_table))
df$category
## ggplot
ggplot(df, aes(x = x ,y = y, fill = category)) + 
        geom_tile(color = "black", size = 0.5) + 
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_brewer(palette = "Set3") +
        labs(title ="Waffle Chart",subtitle = "'Class'of 朝向",caption = "Source: mydata" )
        theme(panel.border = element_rect(size = 2),
              plot.title = element_text(size = rel(1.2)),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.position = "right")

#Boxplot
library(ggExtra)
theme_set(theme_bw())
data_select <- mydata[mydata$面积.平方米. < 250, ]
g <- ggplot(data_select, aes(x = 面积.平方米.,y =月租)) + 
      geom_count() + 
      geom_smooth(method = 'lm', se = F)

ggMarginal(g, type = "histogram", fill = "transparent")

        
#Histogram on a continuous variable
theme_set(theme_classic())

g <- ggplot(data_select,aes(平米月租)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill = 房屋结构),
                 bindwidth = .1,
                 col = "black",
                 size = .1) 
plot(g)


##ggpairs
library(ggplot2)
library('GGally')
library('scales')
library('MASS')
library('plyr')
library('reshape')
set.seed(2022012)
house_sample <- data.frame(c(mydata$房屋结构),c(mydata$朝向),c(mydata$面积.平方米.),
                       c(mydata$月租),c(mydata$平米月租))
ggpairs(house_sample,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))
        