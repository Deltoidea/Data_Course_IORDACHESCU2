library(tidyverse)
library(skimr)
library(car)
df <- read_csv("./Data/BioLog_Plate_Data.csv")
data("MplsStops")
data("MplsDemo")
#quick look
glimpse(df)

df_long <- df%>%pivot_longer(starts_with("Hr_"),names_to="Time",values_to="Absorbance",names_prefix = "Hr_")
  
glimpse(df_long)
unique(df_long$Substrate)
?pivot_longer
df_long$Time <- as.numeric(df_long$Time)

names(df_long)[1] <- "SampleID"

#Visualization
unique(df_long$SampleID)

  ?mutate

df_long<- df_long%>%
  mutate(SampleType=case_when(SampleID %in% c("Clear_Creek","Waste_Water")~"Water",
        SampleID %in% c("Soil_1","Soil_2")~"Soil"))

ggplot(df_long,aes(x=Time,y=Absorbance,color=SampleType))+geom_smooth()+facet_wrap(~Substrate)

unique(df_long$SampleType)
