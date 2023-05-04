install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggdendro")
library(ggplot2)
library(ggdendro)
library(tidyverse)

#https://www.kaggle.com/datasets/iamsouravbanerjee/planet-dataset
#I was so confused while making this chart so I dont know if this is right

planets <- read.csv("planets.csv")
planets <- planets[,-2]
rownames(planets) <- planets$Planet
planets$Planet <- NULL
#head(planets, n=2)

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8

distance <- dist(planets)
hc <- hclust(distance)
head(hc)

dendrogram <- ggdendrogram(hc, size = 2) + 
labs(x="Planets",y="", title="Dendrogram of Planets") + 
theme_dark() +
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

ggsave("Dendrogram1_Planets.png", plot=dendrogram, height=h, width=w, units="in")
