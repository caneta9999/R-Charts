install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("ggalt")
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(ggalt)

#https://www.kaggle.com/datasets/thedevastator/global-video-game-sales

sales <- read.csv("vgsales.csv")
head(sales)

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8

publishers <- sales %>%
  group_by(Publisher) %>%
  summarize(mean_sales = mean(Global_Sales))

top_publishers <- publishers %>%
  arrange(desc(mean_sales))

top_publishers <- head(top_publishers, n = 8)

#head(top_publishers, n=8)

barchart <- ggplot(top_publishers, aes(x= reorder(Publisher, mean_sales),y=mean_sales))+
geom_bar(stat="identity",width = 0.5,aes(fill=mean_sales))+ 
coord_flip()+
scale_fill_gradient(low = "limegreen", high = "yellow") +
theme_excel()+
geom_text(aes(label = round(mean_sales,2),fontface="bold"),size=3.5, position = position_stack(vjust = 0.88))+
labs(x="Publisher",y="Average Sales", title="Top Publishers by Average Sales")+ 
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

genres <- sales %>%
group_by(Genre) %>%
summarize(Sales = sum(Global_Sales))

lollipop <- ggplot(genres, aes(x=Genre, y=Sales)) + 
geom_point(size=3) + 
geom_segment(aes(x=Genre, xend=Genre, y=0, yend=Sales)) +  
labs(x="Genre",y="Global Sales", title="Distribution of Global Sales by Genre")+ 
theme_wsj()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title, angle=20 ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

platforms <- sales %>%
group_by(Platform) %>%
summarize(Global_Sales = sum(Global_Sales), NA_Sales = sum(NA_Sales))

platforms <- platforms %>%
arrange(desc(Global_Sales))

top_platforms <- head(platforms, n = 10)
#top_platforms <- round(top_platforms,2)
#head(top_platforms)

dumbell <- ggplot(top_platforms, aes(x=NA_Sales, xend=Global_Sales, y=reorder(Platform, Global_Sales), group=Platform)) + 
geom_dumbbell(color="#a3c4dc",size=0.9,colour_x ="red",colour_xend="darkblue",size_x =5,size_xend = 5) + 
labs(x="",y="", title="NA and Global Sales by Platform")+
theme_economist()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

snes <- sales %>%
group_by(Platform,Year) %>%
summarize(JP_Sales = sum(JP_Sales))

snes <- snes[snes$Platform == "SNES",]
snes$Year <- as.Date(paste0(snes$Year,"-01-01"))
#print(snes)
#str(snes)

timeseries <- ggplot(snes, aes(x=Year, y=JP_Sales)) + 
geom_line(aes(y=JP_Sales),linewidth=1.5,color="goldenrod") + 
scale_x_date(date_labels = "%Y") + 
labs(x="Year",y="JP Sales", title="Total Japan Sales of SNES Games by Year")+ 
theme_solarized()+
geom_label(aes(label = JP_Sales),fill="white",color="black", vjust = 1, size=3) +
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

platforms <- sales %>%
group_by(Platform) %>%
summarize(Global_Sales = mean(Global_Sales))
platforms$mean_sales_z = round((platforms$Global_Sales - mean(platforms$Global_Sales))/sd(platforms$Global_Sales), 2)
platforms$type <- ifelse(platforms$mean_sales_z < 0, "below", "above")
platforms <- platforms[order(platforms$mean_sales_z), ] 
platforms$Platform<- factor(platforms$Platform, levels = platforms$Platform)
head(platforms)

diverging <- ggplot(platforms, aes(x=Platform, y=mean_sales_z, label=mean_sales_z)) + 
geom_bar(stat='identity', aes(fill=type), width=.5)  +
scale_fill_manual(name="Sales", labels = c("Above", "Below"), values = c("above"="green", "below"="red")) + 
coord_flip()+
theme_calc()+
labs(y="Normalized Sales",x="Platform", title="Diverging Bar - Normalized Sales")+  
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))
#Not sure about this graphic but I think it's cool

ggsave("BarChart2_GlobalVideoGameSales.png", plot = barchart, height=h, width=w, units="in") 

ggsave("Lollipop1_GlobalVideoGameSales.png", plot = lollipop, height=h, width=w, units="in") 

ggsave("Dumbell1_GlobalVideoGameSales.png", plot = dumbell, height=h, width=w, units="in") 

ggsave("TimeSeries2_GlobalVideoGameSales.png", plot=timeseries, height=h, width=w, units="in")

ggsave("Diverging1_GlobalVideoGameSales.png", plot=diverging, height=h, width=w, units="in")