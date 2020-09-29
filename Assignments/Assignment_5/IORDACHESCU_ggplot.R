library(tidyverse)



#load data
data(iris)


#recreate plot 1
ggplot(iris,aes(x=Sepal.Length,y=Petal.Length,color=Species))+geom_point()+geom_smooth(method = lm)+labs(title = "Sepal length vs petal length",subtitle = "for three iris species")+theme_minimal()

ggsave(filename = "./iris_fig1.png",width = 7,height = 5)



#recreate plot 2

ggplot(iris,aes(x=Petal.Width,fill= Species))+geom_density(alpha=.5)+
  labs(title = "Distribution of Petal Widths",subtitle = "for three iris species")+
  theme_minimal()


ggsave(filename = "./iris_fig2.png",width = 7,height = 5)



#recreate plot 3


iris$Ratio <- iris$Petal.Width/iris$Sepal.Width

ggplot(iris,aes(x=Species,y=Ratio,fill=Species))+geom_boxplot()+
  labs(title="Sepal- to Petal-Width Ratio",subtitle="for three iris species",y="Ratio of Sepal Width to Petal Width")+
  theme_minimal()

ggsave(filename = "./iris_fig3.png",width = 7,height = 5)


#recreate plot #4

iris$rows <- rownames(iris)#give the data something trivial to order
iris$meanseplen <- mean(iris$Sepal.Length)#make a column for the mean of Sepal.Length
iris <- iris[order(iris$Sepal.Length),]#order the data by Sepal.Length
iris$rows <- factor(iris$rows,levels=iris$rows)# convert the ordering data into a factor

ggplot(iris,aes(x=rows,y=iris$Sepal.Length-iris$meanseplen))+
  geom_bar(stat = 'identity',aes(fill=Species))+coord_flip()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),axis.title.y = element_blank(),)+
  labs(title="Sepal length deviance from the mean of all observations" ,
                    y="Deviance from the Mean",caption = "Note: Deviance=Sepal.Length-mean(Sepal.Length)")

ggsave("./iris_fig4.png",width = 9,height = 6.43)

