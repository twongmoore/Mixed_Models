#Mixed model for 2 X 2 experimental design

#load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(visdat)

#Read in data
factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/2x2.csv")

#Explore data
str(factorial_data)

#Mutate relevant variables to factors
tidied_factorial_data <- factorial_data %>%
  mutate(subject = factor(Subject), item = factor(Item), RT = RT,
         context = factor(Context), sentence = factor(Sentence))

#Generate descriptives
tidied_factorial_data %>%
  group_by(context, sentence) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

#There is missing data here. To explore this we do the following.
vis_miss(tidied_factorial_data)

#There are some missing response time values so we do the following to generate descriptives.
tidied_factorial_data %>%
  filter(!is.na(RT)) %>%
  group_by(context, sentence) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

#Visualise data
tidied_factorial_data %>%
  filter(!is.na(RT)) %>%
  ggplot(aes(x = context:sentence, y = RT, colour = context:sentence)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs(x = "Context X Sentence",
       y = "RT (ms.)") +
  theme_minimal() +
  coord_flip()

#Before building the mixed model, the contrast coding for our factors needs setting. 
#By default, R uses dummy (treatment) coding. The problem with dummy coding for 
#factorial designs is that you can end up misinterpreting simple effects.  

#To address this, we can use sum or deviation coding. This will result in the Intercept of our model correspoding to the grand mean of our conditions (i.e., the mean of means) and makes the interpretation of our fixed effects (and any interaction effects) more straightforward.
contrasts(tidied_factorial_data$context) <- matrix(c(.5, -.5))
contrasts(tidied_factorial_data$sentence) <- matrix(c(.5, -.5))

#We can now build the mixed model. Note that the maximal model did not converge so we dropped the interaction term (context:sentence) from our subject random effect. For this random effect, we are modeling just additive effects of context and sentence.
factorial_model <- lmer(RT ~ context * sentence + 
                          (1 + context + sentence | subject) +
                          (1 + context * sentence | item), 
                        data = tidied_factorial_data)

#Check assumptions of model
check_model(factorial_model)

#We may have an issue with the normality of our residuals. We may want to try to model under a different distribution. We can plot our RT values on a Cullen and Frey plot.
library(fitdistrplus)
missing_data_removed <- tidied_factorial_data %>%
  filter(!is.na(RT))

descdist(missing_data_removed$RT)

#On the Cullen and Frey plot we see our data is quite close to a Gamma distribution. We can try to model our data using a generalised linear model assuming sampling from the Gamma distribution as follows.
gamma_factorial_model <- glmer(RT ~ context * sentence + 
                                 (1 + context + sentence | subject) +
                                 (1 + sentence | item), 
                               family = Gamma,
                               nAGQ = 0,
                               data = tidied_factorial_data)
#In order to fit this model, I had to simplify the random effects terms and set nAGQ to 0 (its default is 1). This means our parameter estimates are a little less exact than if we had gone with the default (but at least the model converges on a solution).

#Let’s look at the summaries of our two models and see if they differ. 
summary(factorial_model)
summary(gamma_factorial_model)
#The interaction between Context and Sentence is still there but the main effect of Context is now non-significant. 
#In a sense this doesn’t matter too much, as the interaction between our two factors tells us we shouldn’t pay too much attention to main effects anyway. 
#Note that the p-values are a lot lower here as our random effects terms didn’t include any random slopes. 

#To interpret the interactions pairwise comparisons are run.
emmeans(factorial_model, pairwise ~ context*sentence, adjust = "none")
#The interaction is being driven by reading times to Negative sentences in Negative vs. Positive contexts.

#For the gamma model
emmeans(gamma_factorial_model, pairwise ~ context*sentence, adjust = "none")
#Similarly, the interaction is being driven by reading times to Negative sentences in Negative vs. Positive contexts.






