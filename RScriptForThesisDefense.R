#Library Opened
library(readxl)
library(QuantPsyc)
library(dplyr)
library("ggplot2")
library(ggplot2)
library(scales)

#Download the data
Empathy <- read_excel("C:/Users/hseguin/OneDrive - Iowa State University/Desktop/Hugo_Dataset_Sleep_and_Empathy_.xlsx")
View(Empathy)
#Turn 999 tp NA values
Empathy <- na_if(Empathy, 999)
#Check 
View(Empathy)

##########################Histogram of Sleep Length
ggplot(Empathy, aes(x = Total_SleepTime, fill = Sleepcondition)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)

####################Bar graph of time by condition
#Bar Plot, Pre Accuracy
ggplot(Empathy, aes(x=Gender, y=EA_Mean_Pre, fill=Gender)) +
  geom_bar(stat="identity")+
  #geom_errorbar(aes(ymin = EA_Mean_Pre - sd, ymax = EA_Mean_Pre + sd), width = 0.2)+
  theme_minimal()+
  xlab("Gender") +
  ylab("Empathetic Accuracy Post") +
  ggtitle("Total Sleep Time By Condition", subtitle = waiver())+
  theme(legend.position="Center")

##################Bar Plot, Post Accuracy 
ggplot(Empathy, aes(x=Gender, y=EA_Mean_Post, fill=Gender)) +
  geom_bar(stat="identity")+
  #geom_errorbar( aes(x=Gender, ymin=EA_Mean_Post-sd, ymax=EA_Mean_Post+sd), width=0.4, colour="red", alpha=0.9, size=1.3)+
  theme_minimal()+
  xlab("Gender") +
  ylab("Empathetic Accuracy Post") +
  ggtitle("Total Sleep Time By Condition", subtitle = waiver())+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))
#####################################################################
##################Linear graph of Pre-Post Empathy
ggplot(Empathy,aes(x=EA_Mean_Pre,y=EA_Mean_Post))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Pre-Assessment Empathetic Accuracy") +
  ylab("Post-Assessment Empathetic Accuracy") +
  ggtitle("Pre-Post Empathetic Accuracy", subtitle = waiver())+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

#Build Model for Accuracy
library(QuantPsyc)
model <- lm(data = Empathy, EA_Mean_Post ~ EA_Mean_Pre + Gender+ Sleepcondition)
summary(model)
lm.beta(model)

### Assumptions
par(mfrow = c(2, 2))
plot(model)
library(ggfortify)
autoplot(model)
lm.beta(model)

#Build Models for Concern
model2 <- lm(data = Empathy, Concern_Mean_Post ~ Concern_Mean_Pre + Gender+ Sleepcondition)
summary(model2)
lm.beta(model2)

ggplot(Empathy, aes(x=Sleepcondition, y=Concern_Mean_POst)) +
  geom_bar(stat="identity")+
  #geom_errorbar( aes(x=Gender, ymin=EA_Mean_Post-sd, ymax=EA_Mean_Post+sd), width=0.4, colour="red", alpha=0.9, size=1.3)+
  theme_minimal()+
  xlab("Sleep Condition") +
  ylab("Empathetic Accuracy Post") +
  ggtitle("Total Sleep Time By Condition", subtitle = waiver())+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))
### Assumptions
par(mfrow = c(2, 2))
plot(model2)
library(ggfortify)
autoplot(model2)

############Sleep Time For Both

############Concern######################
model3 <- lm(data = Empathy, Concern_Mean_Post ~ Concern_Mean_Pre + Gender+ Total_SleepTime)
summary(model3)
lm.beta(model3)
####################### Empathetic Accuracy ################
model4 <- lm(data = Empathy, EA_Mean_Post ~ EA_Mean_Pre + Gender+ Total_SleepTime)
summary(model4)
lm.beta(model4)







