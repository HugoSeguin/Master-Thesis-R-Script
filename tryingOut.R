#test is dataset
#This is for my own practice, I am able to do all forms of data sets
#Next step is to practice making all of them really nice as these are basic

#Datasets Required
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("stringr")
library("ggplot2")
library("tidyverse")
library("stringr")
library(readxl)
library(QuantPsyc)
library(dplyr)
library("ggplot2")
library(scales)


#Import Data
test <-read_excel("test.xlsx")


#Histogram
hist(test$Rank, breaks = 160, col = "orange",main = "Histogram of Rank", xlab = "Rank")


#Plot
attach(test)
plot(GDPGrowth, GDPperCapita, main = "test", xlab = "Tree Girth", ylab = "Tree Height")
abline(lm(GDPGrowth ~ GDPperCapita), col = "blue", lwd = 2)


#Boxplot
boxplot(GDPGrowth, col = c("yellow", "red", "cyan"), main = "Boxplot for trees dataset")


#Updated_Boxplot
plot(GDPGrowth, type = "o", col = "red", ylab = "", ylim = c(0, 110),
     + main = "Comparison amongst Girth, Height, and Volume of trees")
lines(Name, type = "o", col = "blue")
lines(GDPGrowth, type = "o", col = "green")
legend(1, 110, legend = c("Girth", "Height", "Volume"), col = c("red", "blue", "green"), lty = 1:1, cex = 0.9)

if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

devtools::install_github("calligross/ggthemeassist")
test

#GG scatterplot
options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")


#IDK why did not allow
gg <- ggplot(test, aes(x=GDPGrowth, y=Population)) +
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)

#
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(test, aes(GDPGrowth, Population))
 







