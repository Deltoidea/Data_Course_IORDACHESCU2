###load librarys####

library(tidyverse)
##load first dataset####
df <- read_csv("./landdata-states.csv")
#look at data
glimpse(df)
as.factor(df$State)%>%levels()
#make sure the whole number shows up
#Make plot #1####
options(scipen = 999)
ggplot(df,aes(x=Year,y=Land.Value,color=region))+geom_smooth()+labs(y="Land Value(USD)")+theme_minimal()
#save plot
ggsave("./IORDACHESCU_Fig_1.jpg",device="jpg",height = 4.7,width=5.1)

#find our what States have NA regions:####
#look at what have NA in the region column
narows <- which(is.na(df$region))
#select the state value from the NA row data. 
df$State[narows]
#subset the states as a new value

regionisnastates <- unique(df$State[narows])
regionisnastates


###load 2nd dataset####
cdf <- read_csv("./unicef-u5mr.csv")
#cleaning the data####
#gather all of columns 2:69 into Region, Year, and Mortality

gcdf <- gather(cdf, key="Year",value = "Mortality",2:67)
#clean the year column
str_remove(gcdf$Year,"U5MR.")

gcdf$Year <- str_remove(gcdf$Year,"U5MR.") %>% as.numeric()
gcdf
#Data is clean! I think...
#Creating plot #2####
ggplot(gcdf,aes(x=Year,y=Mortality,color=Continent))+geom_point()+
  theme_minimal()+
  labs(y="MortalityRate")
#save plot
ggsave("./IORDACHESCU_Fig_2.jpg",device="jpg",height = 4.7,width=5.1)


#Plot #3####
#get rid of NAs
na.omit(gcdf)
?geom_path()
gcdf$Year <- as.factor(gcdf$Year)
adf <-gcdf%>%group_by(Continent,Year)%>%summarise(Mortality=mean(Mortality,na.rm = TRUE))
ggplot(adf, aes(x=Year,y=Mortality, color=Continent))+
  geom_path(stat="identity",group=adf$Continent,size=2)+
  scale_x_discrete(breaks = c(1960,1980,2000))+
  theme_minimal()+labs(y="Mean Mortality Rate (deaths per 1000 live births)")

#save plot
ggsave("./IORDACHESCU_Fig_3.jpg",device="jpg",height =4.7,width=5.1 )

#plot #4####
ggplot(gcdf,aes(x=Year,y=1/(1/(Mortality)*1000)))+
  geom_point(color="#4c28c0", size=.5)+
  facet_wrap(~Region,)+
  theme_minimal()+labs(y="Mortality Rate")+
  theme(strip.background = element_rect(color="black"))+
  scale_x_discrete(breaks = c(1960,1980,2000))

#save plot
ggsave("./IORDACHESCU_Fig_4.jpg",device="jpg",height=8.5,width=8)
