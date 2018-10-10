library(ggmap)
library(ggplot2)
library(mapproj)
library(dplyr)
library(forcats)
library(devtools)
library(leaflet)
library(maps)

 mydata <- read.csv("C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week4\\hw_4\\train.csv")

 leaflet(mydata)%>%addTiles()%>% addMarkers(popup=~Category)
 