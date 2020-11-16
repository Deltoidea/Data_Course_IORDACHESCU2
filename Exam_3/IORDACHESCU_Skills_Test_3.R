library(tidyverse)
library(ggpubr)

#Plot Recreation####
#read in the salary Data
df <- read_csv("./FacultySalaries_1995.csv")
#explore and clean up the data
df
names(df)
ggplot(df,aes(x = Tier,y=AvgAssocProfSalary))+geom_boxplot()
#need to change some column classes
df

#looks like I need to combine some columns.
?pivot_longer
dflong <- df%>% pivot_longer(cols = c("AvgFullProfSalary","AvgAssocProfSalary","AvgAssistProfSalary"),
                             names_to = "Rank",
                             values_to= "Salary",names_prefix="Avg")
#Adjust ranks to match graph
dflong$Rank <- str_remove(dflong$Rank,pattern = "ProfSalary")
  #need to change some column classes
dflong$Rank <- as.factor(dflong$Rank)
#test out the plot
ggplot(dflong,aes(x=Tier,y=Salary,fill=Rank))+geom_boxplot()
#looks like there is an incorrect tier. Considering the levels of the Tier column and the frequency of VIIB, I will just have it removed 
dflong <- dflong[-(which(dflong$Tier=="VIIB")),]
#let's retest the plot
ggplot(dflong,aes(x=Rank,y=Salary,fill=Rank))+geom_boxplot()+theme_minimal()+facet_wrap(~Tier)+theme(axis.text.x = element_text(angle=60))
#Okay! Now just to save the file as a jpg file
ggsave(filename = "IORDACHESCU_Fig_1.jpg",units = "cm",width = 9.35,height = 12.1)

#ANOVA table task####
ggplot(dflong,aes(x=Rank,y=salary))+geom_boxplot()
dflong$Tier
mod1 <- glm(data = dflong,formula = Salary~State+Tier+Rank)
anova(mod1)
