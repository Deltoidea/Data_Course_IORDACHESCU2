library(tidyverse)


df <- data("iris")


ggplot(iris, aes(x=Sepal.Length,y=Petal.Length))+geom_point()+geom_smooth(method = lm,aes(color=Species))
?geom_smooth

ggplot(iris, aes(x=Sepal.Length,y=Petal.Width,color=Species))+geom_point()+facet_wrap(~Species)


p <- ggplot(iris,aes(x = Sepal.Length,y= Sepal.Width))+geom_point()+geom_smooth(aes(color=Species))+labs(title = "Another Iris Plot")+
  facet_wrap(~Species)+scale_color_manual(values=c("#20fc03","#fc03ce","#fc0341"))

ggplot(iris,aes(x = Sepal.Length,y= Sepal.Width))+geom_point()+geom_smooth(aes(color=Species))

p+theme_bw()
p+theme_light()
p+theme_void()
p+theme(axis.title = element_text(colour = "Red",face = "bold"),
        axis.title.x=element_text(face = "italic"),
        plot.title = element_text(family = "arial",size = 26,color = "steel blue",face = "italic"),
        panel.grid = element_line(color = "red"),
        panel.background = element_rect(fill = "steel blue"),
        strip.text = element_text(face = "italic"),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(face = "italic"),
        legend.position = "top",
        legend.title = element_blank())

data(mtcars)

library(ggimage)
ggplot(mtcars,aes(x=disp,y=mpg))+geom_point()+geom_image("../../maxlikelyhoodficus.png")
