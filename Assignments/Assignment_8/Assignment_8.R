library(tidyverse)
library(modelr)
library(GGally)
library(lindia)
library(skimr)
library(patchwork)
library(caret)



df <- read_csv("../../Data/mushroom_growth.csv")
names(df)
# Visualize Data
ggplot(df,aes(x=Nitrogen,y=GrowthRate,color=Species))+geom_point()+geom_smooth(method = lm)

ggplot(df,aes(x=Humidity,y=GrowthRate,color=Species))+geom_boxplot()+geom_smooth(method = lm)

ggplot(df,aes(x=Temperature,y=GrowthRate,color=Species))+geom_point()+geom_smooth(method = lm)

ggplot(df,aes(x=Light,y=GrowthRate,color=Species))+geom_boxplot()
# Create models
df%>%ggpairs()
aovmod <- aov(GrowthRate~Humidity*Species*Light*Temperature,data = df)

lmmod <-  lm(GrowthRate~Humidity+Species+Light+Temperature+Nitrogen,data = df)

#mean squares values
mean((aovmod$residuals)^2)
mean((lmmod$residuals)^2)

#add predictions
df1 <- add_predictions(df,aovmod)
  
p1 <- ggplot(df1,aes(x=GrowthRate,color=Species)) +
  geom_point(aes(y=GrowthRate),alpha=.5,size=2) +
  geom_point(aes(y=pred),color="Black") + theme_bw()

p1


  

