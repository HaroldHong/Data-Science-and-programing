library(ggmap)
library(ggplot2)
library(mapproj)
library(dplyr)
library(forcats)
library(devtools)
register_google(key = "AIzaSyCYIbzoIJnDaWbTjYg2do0cJvnKvQcfdos",day_limit = 1000)
ggmap_credentials()
map=get_map(location='San Fransico',maptype='roadmap',zoom=12)


mydata <- read.csv("C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week3\\hw_3\\train.csv")
# head(mydata[c('Category','DayOfWeek','PdDistrict','X','Y','Year')])
ggmap(map)
plot_crime <- ggmap(map)+geom_jitter(aes(X,Y,colour=Category),
              data<-mydata[mydata$Category,],alpha=0.2)+
              geom_jitter(aes(X,Y,colour=Category),data=data0[data0$Category,],alpha=1)+
               labs(x='Longitude',y='Latitude')
plot_crime

