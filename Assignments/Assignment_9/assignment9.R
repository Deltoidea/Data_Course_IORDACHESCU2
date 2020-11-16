library(tidyverse)
library(modelr)
library(GGally)
library(lindia)
library(skimr)
library(patchwork)
library(caret)



#clean up the data
dat<- read.csv("../../Data/GradSchool_Admissions.csv")
#admit as logical vector
dat$admit <- as.logical(df$admit)
dat$rank <- as.factor(dat$rank)
f#explore the data
ggplot(dat,aes(x=gre,y=gpa,color=admit))+geom_point()

dat%>% ggpairs()
  #some models


mod1 <- lm(formula = admit ~ gre ,data=dat)
mod2 <- lm(formula = admit ~ gpa + gre + rank,data=dat,family = "binomial")
mod3 <- lm(formula= admit ~ gre * gpa * rank, data = dat)
#model diagnostics

summary(mod1)
gg_diagnose(mod1)
gg_diagnose(mod2)
gg_diagnose(mod3)

# Compare models ####
anova(mod1, mod2) # Significantyly different
anova(mod1, mod3) # Significantyly different
anova(mod2, mod3)# Not significantly different



 # which has better fit ?
mod1mse <- mean(residuals(mod1)^2)
mod2mse <- mean(residuals(mod2)^2)
mod3mse <- mean(residuals(mod3)^2)

mod1mse ; mod2mse ; mod3mse #mod3 is slightly better than mod 2, both of which are better than mod 1


#add predictions

dat <- gather_predictions(dat, mod1,mod2,mod3)

ggplot(dat,aes(x=gre,y=admit,color=rank)) +
  geom_point(aes(y=gpa),alpha=.25) +
  geom_point(aes(y=pred),color="Black") +
  facet_wrap(~model) +
  theme_bw()

