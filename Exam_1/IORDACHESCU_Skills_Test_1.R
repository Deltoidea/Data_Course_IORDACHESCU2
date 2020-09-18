library(tidyverse)

#Reading the csv file into R and naming it df

df <- read.csv("./Exam_1/DNA_Conc_by_Extraction_Date.csv")

#exploring the data 
glimpse(df)
##Task 1
#Create Histograms of Ben and Katy's DNA concentrations
hist(df$DNA_Concentration_Katy,breaks = 10, main = "Katy's DNA Concentration Histogram",xlab = "DNA Concentration")
hist(df$DNA_Concentration_Ben, breaks = 10,main = "Ben's DNA Concentration Histogram",xlab = "DNA Concentration")


##Task 2
#Convert year value into a factor
df$factoryear <- as.factor(df$Year_Collected)
katyboxplot<- plot(x=df$factoryear,y=df$DNA_Concentration_Katy,main="Katy's Extractions",ylab="DNA Concentration",xlab="Year")

benboxplot <-  plot(x=df$factoryear,y=df$DNA_Concentration_Ben,main="Ben's Extractions",ylab="DNA Concentration",xlab="Year")

getwd()
#Task 3

jpeg(filename = "./IORDACHESCU_plot1.jpeg")

plot(x=df$factoryear,y=df$DNA_Concentration_Katy,main="Katy's Extractions",ylab="DNA Concentration",xlab="Year")


dev.off()

jpeg(filename = "./IORDACHESCU_plot2.jpeg")

plot(x=df$factoryear,y=df$DNA_Concentration_Ben,main="Ben's Extractions",ylab="DNA Concentration",xlab="Year")


dev.off()

#Task 4

summary(df$DNA_Concentration_Katy)
sd(df$DNA_Concentration_Katy)
summary(df$DNA_Concentration_Ben)
sd(df$DNA_Concentration_Ben)

#Crap ton of subsetting because I can't figure out for loops!!!!!!!

df2000 <- df%>%filter(Year_Collected==2000)

mean00 <- c(mean(df2000$DNA_Concentration_Katy),mean(df2000$DNA_Concentration_Ben))

df2001 <- df%>%filter(Year_Collected==2001)

mean01 <- c(mean(df2001$DNA_Concentration_Katy),mean(df2001$DNA_Concentration_Ben))

df2002 <- df%>%filter(Year_Collected==2002)

mean02 <- c(mean(df2002$DNA_Concentration_Katy),mean(df2002$DNA_Concentration_Ben))

df2003 <- df%>%filter(Year_Collected==2003)

mean03 <- c(mean(df2003$DNA_Concentration_Katy),mean(df2003$DNA_Concentration_Ben))

df2004 <- df%>%filter(Year_Collected==2004)

mean04 <- c(mean(df2004$DNA_Concentration_Katy),mean(df2004$DNA_Concentration_Ben))

df2005 <- df%>%filter(Year_Collected==2005)

mean05 <- c(mean(df2005$DNA_Concentration_Katy),mean(df2005$DNA_Concentration_Ben))

df2006 <- df%>%filter(Year_Collected==2006)

mean06 <- c(mean(df2006$DNA_Concentration_Katy),mean(df2006$DNA_Concentration_Ben))

df2007 <- df%>%filter(Year_Collected==2007)

mean07 <- c(mean(df2007$DNA_Concentration_Katy),mean(df2007$DNA_Concentration_Ben))

df2008 <- df%>%filter(Year_Collected==2008)

mean08 <- c(mean(df2008$DNA_Concentration_Katy),mean(df2008$DNA_Concentration_Ben))

df2009 <- df%>%filter(Year_Collected==2009)

mean09 <- c(mean(df2009$DNA_Concentration_Katy),mean(df2009$DNA_Concentration_Ben))

df2010 <- df%>%filter(Year_Collected==2010)

mean10 <- c(mean(df2010$DNA_Concentration_Katy),mean(df2010$DNA_Concentration_Ben))

df2011 <- df%>%filter(Year_Collected==2011)

mean11 <- c(mean(df2011$DNA_Concentration_Katy),mean(df2011$DNA_Concentration_Ben))

df2012 <- df%>%filter(Year_Collected==2012)

mean12 <- c(mean(df2012$DNA_Concentration_Katy),mean(df2012$DNA_Concentration_Ben))

meandf <- data.frame(mean00,mean01,mean02,mean03,mean04,mean05,mean06,mean07,mean08,mean09,mean10,mean11,mean12)



long <- pivot_longer(meandf,1:13)

longkaty <- long[1:13,]
longben <- long[14:26,]

compvec <- longben$value-longkaty$value
The_frickin_lowest_year_for_Ben_row_number <- which.min(compvec)
compvec

####finally this line of code below will give you the lowest performing year for Ben relative to Katy!!!!!!
The_frickin_lowest_year_for_Ben<- longben[The_frickin_lowest_year_for_Ben_row_number,1]
The_frickin_lowest_year_for_Ben
#lowest year for ben was 2000



#Task 5
Ddf<- df %>% filter(Lab=="Downstairs")
Characterdate <- as.character(Ddf$Date_Collected)
DDf <- as.Date(Characterdate)

jpeg(filename = "./Ben_DNA_over_time.jpeg")

plot(x=DDf, y=Ddf$DNA_Concentration_Ben, ylab = "DNA Concentration",xlab = "Year", main= "Ben's DNA Over Time")

dev.off()


#Task 6

#Looks like I did this one accidentally...

longben
highest_average_year_row <- which.max(longben$value)
bens_best_year <- longben[highest_average_year_row,]
# the best year for ben was 2007
bens_best_year

write.csv(longben,"./Ben_Average_Conc.csv")

