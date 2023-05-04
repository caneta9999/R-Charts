install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

#https://www.kaggle.com/datasets/parulpandey/palmer-archipelago-antarctica-penguin-data

penguins <- read.csv("penguins_size.csv")
head(penguins)
penguins$sex <- ifelse(penguins$sex == ".", NA, penguins$sex)

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8
a <- 0
hjust <- 0.5

barchart <- ggplot(penguins, aes(species,fill=sex))+
geom_bar(stat="count",position='stack', width = 0.6)+
geom_text(stat="count", aes(label=after_stat(count)), position = position_stack(vjust = 0.5))+
labs(x="Species",y="Count", title="Distribution of Penguin Species ")+ 
theme_classic()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title,angle=a ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

pie <- ggplot(penguins, aes(x="",fill = island)) + 
geom_bar(width = 1) +
scale_fill_brewer(palette = "PuBu")+
theme_classic()+
theme(axis.text.x = element_blank(),plot.title = element_text(hjust=hjust,size=t)) + 
labs(fill="island", x=NULL, y=NULL, title="Pie Chart of Penguin Islands") +
coord_polar(theta = "y", start=0) +
geom_text(stat="count",aes(label = after_stat(count)),size=6, position = position_stack(vjust = 0.5))

boxplot <- ggplot(penguins, aes(y=body_mass_g,fill=species))+ 
geom_boxplot(varwidth=T)  +
labs(x="",y="Body Mass", title="Penguin Body Mass Distribution by Species")+  
theme_classic()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

density2d <- ggplot(penguins, aes(x=culmen_length_mm, y=culmen_depth_mm) ) +
geom_bin2d(bins = 15) + scale_fill_continuous(type = "gradient") +
theme_classic()+
labs(x="Length",y="Depth", title="Distribution of Culmen length and depth (mm)")+  
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

ggsave("BarChart1_PenguinData.png", 
plot = barchart, 
height=h, width=w, units="in") 

ggsave("Pie1_PenguinData.png", 
plot = pie, 
height=h, width=w, units="in")

ggsave("BoxPlot1_PenguinData.png", 
plot = boxplot, 
height=h, width=w, units="in")

ggsave("2DDensity1_PenguinData.png", 
plot = density2d, height=h, width=w, units="in")