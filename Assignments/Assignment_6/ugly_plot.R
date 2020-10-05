#set libraries
library(tidyverse)
library(ggimage)
data("USPersonalExpenditure",row.names=FALSE)

data("msleep")
df <- msleep
glimpse(df)
names(df)
#set column for sleep image

df$img <-"./Images/sleep.png"

ggplot(df,aes(y=sleep_total,x=brainwt))+geom_point()+
  geom_image(aes(image=df$img))+
  geom_smooth(method = lm)+
  facet_wrap(~conservation)+
  labs(title = "Sleepy Animals",x="brain mass",y="total sleep")+
  theme(text = element_text(family ="Cambria"))+
  theme(plot.background = element_rect(fill="red"),plot.title = element_text(angle = 355,colour = "lemonchiffon1",hjust =1.0,vjust = .8),panel.background = element_rect(fill="#a64c0c"))+
  scale_x_continuous(breaks=c(0, 15,23,34, 76, 75, 150))+scale_y_continuous(breaks=c(0, 50, 65, 75, 150,166,202,303,404))+
  theme(strip.text.x = element_text(size=11, angle=75),
                                                                           strip.text.y = element_text(size=12, face="italic"),
                                                                           strip.background = element_rect(colour="purple", fill="gold"),element_line(size =2))


?msleep
??ggimage
