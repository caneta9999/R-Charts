install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(tidyverse)

#https://www.kaggle.com/datasets/shubhammeshram579/bank-customer-churn-prediction

data <- read.csv("Churn_Modelling.csv")
head(data)

sample_size <- function(dataset = data, percentage = 0.1){
	return (round(nrow(dataset) * percentage))
}

t <- 14
axis_title <- 12
x_title <- y_title <- 10
h <- 4
w <- 8

bubble <- ggplot(sample_n(data,sample_size(percentage=0.05)), aes(x=EstimatedSalary,y=Age)) + 
geom_jitter(aes(size=Balance,color=EstimatedSalary))+
labs(x="Age",y="Salary", title="Age vs Salary against Balance")+ 
theme_gray()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))+
scale_color_gradient(low = "blue", high = "red")

scatter1 <- ggplot(sample_n(data,sample_size()), aes(x=Age, y=CreditScore)) + 
geom_point() + 
labs(x="Age", y="Credit Score", title="Age vs Credit Score")+
geom_smooth(method = 'lm')+
theme_gray()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

scatter2 <- ggplot(sample_n(data,sample_size(percentage=0.075)), aes(x=CreditScore, y=EstimatedSalary)) + 
geom_point(aes(color=Geography)) + 
labs(x="Credit Score", y="Estimated Salary",
title="Credit Score Vs Estimated Salary against Geography")+ 
theme_gray()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

histogram <- ggplot(data,aes(x=CreditScore)) + 
geom_histogram(binwidth=30, fill="aquamarine",color="black",size=2) + 
geom_vline(aes(xintercept=mean(CreditScore)),color="aquamarine4", linetype="dashed", size=2)+
labs(x="CreditScore",y="Count", title="Distribution of Credit Scores")+  
theme_gray()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

data <- data %>%
  mutate(n_products = paste0(NumOfProducts, " Prod."))
tail(data)

violin <- ggplot(data, aes(x=n_products, y=Balance,fill=n_products)) + 
geom_violin(width=0.7)+
labs(x="",y="Balance", title="Distribution of Balance for Number of Products")+  
theme_gray()+
theme(plot.title = element_text(size=t),axis.text.x= element_text(size=x_title ),axis.text.y= element_text(size=y_title ), axis.title=element_text(size=axis_title))

ggsave("Scatter1_BankCustomerChurnPrediction.png", plot = scatter1, height=h, width=w, units="in") 

ggsave("Scatter2_BankCustomerChurnPrediction.png", plot = scatter2, height=h, width=w, units="in") 

ggsave("Bubble1_BankCustomerChurnPrediction.png", plot = bubble, height=h, width=w, units="in")

ggsave("Histogram2_BankCustomerChurnPrediction.png", plot = histogram, height=h, width=w, units="in")

ggsave("Violin1_BankCustomerChurnPrediction.png", plot=violin, height=h, width=w, units="in")