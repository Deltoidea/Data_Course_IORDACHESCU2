library(tidyverse)
library(ggpubr)
library(broom)
#Plot Recreation####
#read in the salary Data
df <- read_csv("./FacultySalaries_1995.csv")
#explore and clean up the data
#ggplot(df,aes(x = Tier,y=AvgAssocProfSalary))+geom_boxplot()
#need to change some column classes


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
mod1 <- glm(data = dflong,formula = Salary~State+Tier+Rank,method = "glm.fit")
#got my model. Just need to write it to a text file
sink(file = "Salary_ANOVA_Summary.txt")
summary(aov(mod1))
sink(NULL)

#Task III####
#read the data
df1 <- read_csv(file = "./Juniper_Oils.csv")
#Woah, Right off the bat I will combine the oils into two columns. Thanks for the chem names!!

df1long<- df1%>%pivot_longer(cols = c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal"),
                             names_to="Oil_type",values_to= "Concentration")


#Task IV####
library(broom)
ggplot(df1long,aes(x=YearsSinceBurn,y=Concentration))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Oil_type,scales = "free_y")
ggsave("./IORDACHESCU_Fig_2.jpg")


#Task V
modtree <- glm(formula = Concentration~Oil_type*YearsSinceBurn,data = df1long)
modtree%>%tidy()
results_df <- summary(modtree)$coefficients
tbl <- results_df%>%as.data.frame(colnames=TRUE)%>%filter(`Pr(>|t|)`<=.05)%>%tidy()
kableExtra::kable(tbl)
sink("./Significant_Chemicals.txt")
cat("Key `Pr(>|t|)`=P.Value
    ")
results_df%>%as.data.frame(colnames=TRUE)%>%filter(`Pr(>|t|)`<=.05)
sink(NULL)
