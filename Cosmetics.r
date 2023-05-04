install.packages("tidyverse")
install.packages("ggplot2")
install.packages("treemap")
library(treemap)
library(ggplot2)
library(tidyverse)

#https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets

cosmetics <- read.csv("cosmetics.csv")
head(cosmetics)

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8

cosmetics$Label<-as.factor(cosmetics$Label)
var <- cosmetics$Label
head(cosmetics$Label)

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var)-1)))
df$category <- factor(rep(names(categ_table), categ_table))
head(df)

waffle <- ggplot(df, aes(x = x, y = y, fill = category)) + 
geom_tile(color = "black", size = 0.5) +
scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
labs(title="Waffle Chart - Cosmetics Labels",fill="Label") + 
theme(plot.title = element_text(size=t))+
theme(axis.text = element_blank(),
axis.title = element_blank(),axis.ticks = element_blank(),
legend.title = element_text(size=15),legend.position = "right")

cmap <- cosmetics %>% 
  group_by(Brand,Label) %>% 
  summarize(count = n())
cmap <- cmap %>%
  arrange(desc(count))
cmap <- head(cmap,n=30)
cmap$BrandCount <- cmap$count
cmap$count <- NULL
head(cmap)

treecmap <- treemap(cmap,index=c("Brand","Label"),vSize="BrandCount",type="index")

ggsave("Waffle1_Cosmetics.png", plot=waffle, height=h, width=w, units="in")
#ggsave("Treemap1_Cosmetics.png", plot=treecmap, height=h, width=w, units="in")