install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
library(ggthemes)
library(ggplot2)
library(tidyverse)

#https://www.kaggle.com/datasets/koustavghosh149/co2-emission-around-the-world

co2 <- read.csv("CO2_emission.csv")
#head(co2)

co2 <- co2[-1,]
co2 <- pivot_longer(co2, cols = all_of(paste0("X",c(1990:2019))), names_to = "Year", values_to = "Value")
co2 <- co2[,c(-4,-5)]
co2$Year <- substr(as.character(co2$Year), 2, 5)
co2$Year <- as.numeric(co2$Year)
names(co2)[1:3] <- c("Country","Code","Region")
#str(co2)
#head(co2)

countries <- co2 %>%
group_by(Country) %>%
summarize(co2 = mean(Value))

countries <- countries %>%
arrange(desc(co2))
countries <- countries[c(1:5),]
countries <- co2[co2$Country %in% countries$Country,]
head(countries)

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8

timeseries <- ggplot(countries, aes(x=Year,color=Country)) + 
geom_line(aes(y=Value)) + 
labs(x="Year",y="CO2", title="CO2 emissions per capita")+ 
theme_fivethirtyeight()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

ggsave("TimeSeries1_CO2Emissions.png", plot = timeseries, height=h, width=w, units="in") 