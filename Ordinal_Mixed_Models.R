#Ordinal Mixed Model

#Load packages
library(ordinal)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

#Read in data
ordinal_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/ordinal_data.csv")

#Snapshot of data 
head(ordinal_data)

#Mutate data to factor
ordinal_data_tidied <- ordinal_data %>%
  mutate(Subject = factor(Subject), SportsType = factor(SportType)) %>%
  mutate(Ratings = ratings) %>%
  mutate(VideoCondition = as.character(VideoCondition)) %>%
  mutate(VideoCondition = factor(recode(VideoCondition, "2" = "Match", 
                                        "3" = "Mismatch", "4" = "Neutral"))) %>%
  dplyr::select(Subject, SportType, VideoCondition, Ratings)

#DV has to be changed to an ordered factor
ordinal_data_tidied$Ratings <- as.ordered(ordinal_data_tidied$Ratings)

#Data Visualisation
ordinal_data_tidied %>%
  ggplot(aes(x = VideoCondition, y = Ratings, group = VideoCondition)) +
  geom_jitter(aes(colour = VideoCondition), width = .1, alpha = .25, size = 1) + 
  theme_minimal() +
  guides(colour = FALSE) +
  theme(text = element_text(size = 14)) +
  stat_summary(fun = "median", size = 2, alpha = .5)

#Let’s build our ordinal mixed model using the clmm() function from the {ordinal} package to do this.
ordinal_model <- clmm(Ratings ~ VideoCondition + 
                        (1 + VideoCondition | Subject) +
                        (1 + VideoCondition | SportType), 
                      data = ordinal_data_tidied)   

#Let’s also build a null model  to see how it compares to the one above.
null_ordinal_model <- clmm(Ratings ~ 1 + 
                             (1 + VideoCondition | Subject) +
                             (1 + VideoCondition | SportType), 
                           data = ordinal_data_tidied)   

#Compare the two models
anova(null_ordinal_model, ordinal_model)
#The ordinal model is better so lets investigate it further.
summary(ordinal_model)

#Let’s examine which condition differs from each other.
emmeans(ordinal_model, pairwise ~ VideoCondition)
#The pairwise comparisons indicate that the Match vs Mismatch conditions differ from each other, as do the Match vs. Neutral conditions. 
