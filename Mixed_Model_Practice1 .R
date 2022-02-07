#Mixed model with one factor and two levels

#Load packages
library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)

#We first read in the datafile. 
mixed_model_data <- read.csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/mixed_model_data.csv")

#We are also going to use mutate() to turn our subject and gender columns into factors.
mixed_model_data <- mixed_model_data %>%
  mutate(subject = factor(subject),
         item = factor(item),
         condition = factor(condition))

# Descriptive statistics.
mixed_model_data %>% 
  group_by(condition) %>%
  summarise(mean_rt = mean(rt), sd_rt = sd(rt))

# I have built a mixed model with condition as a fixed effect, and subject and item as random effects.
mixed_model <- lmer(rt ~ condition + (1 | subject) + (1 | item), 
                    data = mixed_model_data)
summary(mixed_model)

#The Intercept parameter estimate is 854.140. This corresponds to the mean RT for the Large experimental condition. The estimate -49.780 corresponds to the difference in mean RT between the Large experimental condition and the Small experimental condition. In other words, people are 49ms faster in the Small condition relative to the Large condition. 

#We can use the Likelihood Ratio Test (LRT) to determine whether our model containing our fixed effect of condition is better than a model that contains only the random effects. Note, you can only use the LRT when one model is a subset or (or nested within) the other
mixed_model_null <- lmer(rt ~ (1 | subject) + (1 | item), 
                         data = mixed_model_data)

#We can then compare the two models to each other via the LRT using anova()
anova(mixed_model, mixed_model_null)
#We can see the models differ from each other, with the AIC, BIC, and deviance measures all lower for the model with the fixed effect. This indicates that model with the fixed effect of condition explains more of the variability in our data than does the model with only random effects (and no fixed effect).

#Let’s now build a model which models the slopes of our random effects. This means we are allowing the difference between the two levels of our fixed effect to differ in magnitude from one participant to the next, and from one item to the next.
mixed_model_slopes <- lmer(rt ~ condition + (1 + condition | subject)
                           + (1 + condition | item), data = mixed_model_data)

#We can investigate the model parameter estimates using the summary() function.
summary(mixed_model_slopes)
#We can see that with a more complex random effect structure (i.e., random slopes as well as intercepts), the effect of our fixed effect of condition is still clearly there (and it is significant).

#Mixed model with one factor and three levels.

#Read in data and change relevant variables to factors.
tidied_factor_1_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/one_factor.csv")

tidied_factor_1_data <- tidied_factor_1_data %>%
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Condition = factor(Condition))

#Let’s plot our data.
tidied_factor_1_data %>%
  ggplot(aes(x = Condition, y = Gaze, colour = Condition)) +
  geom_violin(width = .5) +
  geom_jitter(width = .1, alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  labs(x = "Condition",
       y = "Gaze Duration (ms.)") +
  guides(colour = FALSE) +
  coord_flip()

#If we build the following model, we end up with a singular fit warning suggesting we are trying to estimate too many parameters than our data supports.
factor_1_model <- lmer(Gaze ~ Condition + (1 + Condition | Subject) + 
                         (1 + Condition | Item), data = tidied_factor_1_data)

#We can simplify the model by dropping random effect terms until the warning goes away and we have a set of parameter estimates we can be confident in.
factor_1_model <- lmer(Gaze ~ Condition + (1 | Subject) + (1 | Item), 
                       data = tidied_factor_1_data) 

#We can check the model assumptions using the {performance} package. Remember, we want to see the residuals (roughly) normally distributed.
library(performance)
check_model(factor_1_model)

#We can generate the summary of the model.
summary(factor_1_model)
#In this case, the Intercept corresponds to the Negative condition - this is because we are still using R dummy coding of contrasts and the reference level is the condition that occurs first alphabetically. 
#The estimates for conditionNeutral and conditionPositive involve a comparison of these two levels of our factor with the reference level (i.e., the Negative condition). 
#We see that both levels of our factor differ from this reference level.

#To determine whether each condition differs from each other condition, we can run pairwise comparisons using the {emmeans} package.Tukey correction is used for multiple comparisons.
emmeans(factor_1_model, pairwise ~ Condition)
#We can see with an appropriate correction for multiple comparisons, that the only pairwise comparison that is significant is the Negative vs Positive condition comparison.


