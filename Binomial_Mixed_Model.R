#Mixed Model for Binomial data

#Load Packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

#Read in the data
regressions_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/regressions.csv")

#Mutate relevant variables to factors 
tidied_regressions_data <- regressions_data %>%
  mutate(Subject = factor(Subject), Item = factor(Item), 
         Condition = factor(Condition), DV = DV)

#Check structure of data 
str(tidied_regressions_data)

#Descriptive statistics
tidied_regressions_data %>%
  group_by(Condition) %>%
  summarise(mean_DV = mean(DV), sd_DV = sd(DV))

#Things look pretty similar from condition to condition. Let’s build a binomial model to check.
#The maximal model doesn’t converge so we need to simplify the random effects structure. 
#The following is the most complex one we can find to fit our data.
binomial_model <- glmer(DV ~ Condition + (1 | Subject), 
                        data = tidied_regressions_data,
                        family = binomial)

#Sumary of model
summary(binomial_model)

#It doesn’t look like there’s much going on in the data. 
#We can also compare the binomial model with the fixed effect (above) to a model with only the random effect.
null_binomial_model <- glmer(DV ~ (1 | Subject), 
                             data = tidied_regressions_data,
                             family = binomial)

#The Likilhood Ratio Test can be used to see if they differ.
anova(binomial_model, null_binomial_model)





