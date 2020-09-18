library(tidyverse)



df <-  read.csv("../../Data/ITS_mapping.csv",sep = "\t") 

df
str(df)
summary(df)
summary(df$Home.Value)
glimpse(df)
names(df)
ggplot(df,aes(x=State,y=Home.Value,color=Date ))+geom_point()
?boxplot()
boxplot(Lat~Ecosystem,data = df,main="Lat as a function of Ecosystem boxplots")
names(df)[4]

png("./silly_boxplot.png",width =1000 ,height = 480)

boxplot(Lat~Ecosystem,data = df,main="Lat as a function of Ecosystem boxplots")

dev.off()
