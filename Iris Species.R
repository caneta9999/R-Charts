install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

#https://www.kaggle.com/datasets/uciml/iris

iris <- read.csv("Iris.csv")
head(iris)

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8

jitter <- ggplot(iris, aes(x=PetalLengthCm,y=PetalWidthCm,color=Species)) +
geom_jitter()+ 
labs(x="Petal Length (cm)",y="Petal Width (cm)", title="Petal Length vs Petal Width against Species ")+ 
theme_linedraw()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

histogram <- ggplot(iris, aes(x=SepalLengthCm)) + 
geom_histogram(aes(fill=Species))+
facet_grid(Species ~ .)+
labs(x="Sepal Length Cm",y="Count", title="Distribution of Sepal Length")+  
theme_linedraw()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

density <- ggplot(iris, aes(x=SepalWidthCm))+
geom_density(aes(fill=Species),alpha=0.55)+
labs(x="Sepal Width (cm)",y="Count", title="Distribution of Sepal Width")+  
theme_linedraw()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

ggsave("Jitter1_IrisSpecies.png", 
plot = jitter, 
height=h, width=w, units="in") 

ggsave("Histogram1_IrisSpecies.png", 
plot = histogram, 
height=h, width=w, units="in")

ggsave("Density1_IrisSpecies.png", 
plot = density, 
height=h, width=w, units="in")
