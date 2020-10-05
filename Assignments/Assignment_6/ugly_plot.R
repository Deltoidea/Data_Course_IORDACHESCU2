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

ggplot(df,aes(y=sleep_total,x=brainwt,color=vore,Image=img))+geom_point()+
  geom_image(aes(image=df$img),size=.5)+
  geom_smooth(method = lm)+
  facet_wrap(~conservation)+
  labs(title = "Sleepy Animals",x="brain mass",y="total sleep")+
  theme(text = element_text(family ="Cambria"),axis.text = element_text(family = "Palatino",angle = 90))+
  theme(plot.background = element_rect(fill="blue"),legend.background = element_rect(fill="purple"),
        legend.text = element_text(family = "AvantGarde",color="violet",angle = 300),legend.title = element_text(colour = "red",family = "Times"),plot.title = element_text(angle = 355,colour = "lemonchiffon1",hjust =1.0,vjust = .8),panel.background = element_rect(fill="#a64c0c"))+  
    theme(strip.text.x = element_text(size=7, angle=20),
                                                                           strip.text.y = element_text(size=12, face="italic",angle = 270),
                                                                           strip.background = element_rect(colour="purple", fill="gold"),element_line(size =2))

ggsave("./uglyplot.png",height = 10,width = 5
)
?msleep
??ggimage
