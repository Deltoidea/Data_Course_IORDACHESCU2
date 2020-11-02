library(measurements)
library(tidyverse)
inches <- c(1,3,5.5,12)
lbs <- c(.3,.6,2,6)
mol <- c(5.8,9.1,12.335,33.5441)
farenheit <- c(86,88,59.2,101)
angle_degree <- c(90,180,30,15)
filesize_bit <- c(1024,885566,20443,6084)
measures <- as.data.frame(cbind(inches,lbs,mol,farenheit,angle_degree,filesize_bit))
measures
# using conv_unit() function within mutate()
measures %>% 
  mutate(centimeters = conv_unit(inches,from="inch",to="cm"),
         kilograms = conv_unit(lbs,from="lbs",to="kg"),
         mmol = conv_unit(mol,from="mol",to="mmol"),
         celcius = conv_unit(farenheit,from="F",to="C"),
         angle_radian = conv_unit(angle_degree,from="degree",to="radian"),
         filesize_byte = conv_unit(filesize_bit,from="bit",to="byte")
  ) %>% 
  select(inches,centimeters,lbs,kilograms,mol,mmol,farenheit,celcius,
         angle_degree,angle_radian,filesize_bit,filesize_byte) # rearrange cols

