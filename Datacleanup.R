library(tidyverse)
library(skimr)


df <- read.csv("./Data/MLO_Metadata.csv")


skim(df)

glimpse(df)

#dplyr verbs ####


filter() #picks rows
?select()#picks columns
arrange()#Sorts based on a column
mutate()#modify columns based on existing column
group_by()#create groups based on columns
summarise()#
# %>% # control shift m makes a pipe

  
(df$Day)%>%mean()%>%round(2)



df %>%
  filter(Month %in% c("April","May"))%>%
  select(-c("Month","Day"))#minus sign in front of c inverts the selection
?where()
df%>% filter(Month %in% c("April","May"))%>%
  select(where(is.numeric))#
rlang::last_error()

df