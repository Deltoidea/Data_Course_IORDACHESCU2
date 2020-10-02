
library(tidyverse)
library(patchwork)
# 1.  loads the mtcars data set
data("mtcars")
# 2.  subsets the mtcars dataframe to include only **automatic transmissions**
names(mtcars)
?mtcars
dfa<- mtcars%>%filter(am=="0")
dfa
#   3.  saves this new subset as a new file called "automatic_mtcars.csv" in *your* Assignment_6 directory
write_csv(dfa,"./automatic_mtcars.csv")
# 4.  plots the effect of horsepower on miles-per-gallon using ggplot2 (update plot to have meaningful labels and title)
ggplot(mtcars,aes(x=hp,y=mpg))+geom_point()+geom_smooth(method = lm)+labs(title="Fuel Economy as a Function of Horsepower",y="Fuel Economy",x="Horsepower")+theme_minimal()
# 5.  saves this plot as a png image called "mpg_vs_hp_auto.png" in your Assignment_6 directory
ggsave("./mpg_vs_hp_auto.png")
# 6.  plots the effect of weight on miles-per-gallon (with improved labels, again)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+geom_smooth(method = lm)+labs(title="Fuel Economy as a Function of Vehicle Weight",y="Fuel Economy",x="Vehicle Weight")+theme_minimal()

# 7.  saves this second plot as a **tiff** image called "mpg_vs_wt_auto.tiff" in your Assignment_5 directory
ggsave("./mpg_vs_wt_auto.tiff")
# 8.  subsets the *original* mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
class(mtcars$disp)
df_tiny <- mtcars%>%filter(disp<=200)
# 9.  saves that new subset as a csv file called mtcars_max200_displ.csv
write.csv(df_tiny,"./mtcars_max200_displ.csv")
# 10. includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
original_hp <- max(mtcars$hp)
automatic_hp <-max(dfa$hp)
max200_hp <- max(df_tiny)
# 11. prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt

df_txt <- data.frame(original_hp,automatic_hp,max200_hp)
write.table(df_txt,"./hp_maximums.txt")
# 12. combines the following 3 plots into one image using the *patchwork* package (all 3 plots use the full un-subsetted mtcars data)
#create a new df for this part of the assignment labeled df
df <- mtcars
#make cyl a factor so it is not interpreted as continous data
df$cyl <- as.factor(df$cyl)



# - Scatterplot + trendline of the effect of weight on mpg (points and linear trendlines colored by the number of cylinders)
p1<- ggplot(df,aes(x=wt,y=mpg,color=cyl))+geom_point()+geom_smooth(method = lm)+
  labs(title = "Fuel Economy vs Weight",x="Weight",y="MPG")
p1
# - Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
p2 <- ggplot(df,aes(x=mpg,y=cyl,fill=cyl))+geom_violin()+labs(title = "Distribution of fuel economy by cylinder number")
# - Scatterplot + trendline of the effect of horsepower on mpg (points and linear trendlines colored by the number of cylinders)

p3 <- ggplot(df,aes(x=hp,y=mpg,color=cyl))+geom_point()+geom_smooth(method = lm)+labs(title = "Horsepower vs Fuel Economy")
p4<- p1/p2/p3
p4
# 13. saves that combined figure as a single png image file called combined_mtcars_plot.png in your Assignment_6 directory
ggsave("./combined_mtcars_plot.png",width = 7,height = 10)

