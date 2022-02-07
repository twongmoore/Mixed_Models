#Read in the data.
challenge_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/data1.csv")

#Snapshot of the data frame.
head(challenge_data)

#Mutate the relevant variables to factors.
challenge_data <- challenge_data %>% 
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Condition = factor(Condition))

#Summary statistics. NA for Rare condition due to missed values.
challenge_data %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))

#Data Visualisation. Violin plot 
challenge_data %>% 
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin(width = 0.3) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.05, seed = 42)) +
  theme_minimal() +
  guides(colour = 'none') +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
  labs(y = "Reaction Time (ms)",
       title = "Effect of Item Rarity on Reaction Time") +
  coord_flip()

# First we created a model which takes into account the random effect of Subject and Item.
challenge_model <- lmer(RT ~ Condition + (1 | Subject) + (1 | Item), data = challenge_data)

# In this model, the mean RT for common words was 1366.19ms, and the mean RT for rare words was 200.28ms slower - 1566.47ms
summary(challenge_model)

# Our model doesn't seem to violate any of our assumptions
check_model(challenge_model)

# We then created a model which ignores the effect of condition
challenge_model_null <- lmer(RT ~ (1 | Subject) + (1 | Item), data = challenge_data)

# If we compare these two models, we can see that our model which takes into account condition can explain more of the 
# variance in our data set, with a lower AIC, BIC , and lower deviance
anova(challenge_model, challenge_model_null)

# Our next model takes into account the random effect of condition.
challenge_model_slopes <- lmer(RT ~ Condition + (1 + Condition | Subject) + (1 + Condition | Item), data = challenge_data)

# For this model, the mean RT for common words was 1367.51ms, and the mean RT for rare words was 1567.61ms - 200.1ms slower.
summary(challenge_model_slopes)

# This next model ignores the fixed effect of condition on RT
challenge_sloped_null <- lmer(RT ~ (1 + Condition | Subject) + (1 + Condition | Item), data = challenge_data)

# When comparing the two models, the model which takes into account the fixed effect of condition can explain more the 
# variance in our dataset
anova(challenge_model_slopes, challenge_sloped_null)
