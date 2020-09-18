library(tidyverse)

df <- read.delim("../../Data/ITS_mapping.csv")
df2 <- df %>% filter(Ecosys_Type %in% c("Aerial","Marine"))
df[df$Ecosys_Type %in% c("Aerial", "Marine"),]#last comma needed because the square brackets require a row section and a column section. 

table(df$Ecosys_Type)
summary(df$Lat)
#subset of marine vs terrestrial 
# number of samples in each & mean latitiude of each

df %>% group_by(Ecosys_Type) %>% summarise(NumberOfSamples = n) %>% filter(Ecosys_Type %in% c("Marine", "Terrestrial"), Mean_Lat
table(df$Ecosys_Type[df$Ecosys_Type == "Marine"])
marine <- df %>% filter(Ecosys_Type== "Marine")
terrestrial <- df %>% filter(Ecosys_Type== "Terrestrial")
mean(marine$Lat)
length(marine$Lat)
