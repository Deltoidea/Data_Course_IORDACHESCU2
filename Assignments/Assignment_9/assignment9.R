library(tidyverse)
library(modelr)


#clean up the data
df <- read.csv("../../Data/GradSchool_Admissions.csv")
#admit as logical vector
df$admit <- as.logical(df$admit)

f#explore the data
ggplot(df,aes(x=gre,y=gpa,color=admit))+geom_point()
