#### ASSIGNMENT 3: Binary Outcomes

### Part 1: In-Class (ANES Voter Turnout)

## 1.2  Setup and data preparation

library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(rlang)
library(sandwich)


raw <- read.csv("assignment3/data/anes.csv")

glimpse(raw)
head(raw)

class(NA_character_)
class(NA_real_)
class(NA)

df <- raw %>%
  mutate(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~1,
      V201600 == 1 ~0, 
      TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_), 
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x))

df = na.omit(df)
nrow(df)

mean(df$voted)
summary(df)

## 1.2 Exploratory visualization

turnout_by_edu = df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted))

ggplot(turnout_by_edu, aes (x = factor(education), y = turnout)) +
       geom_col() + 
         labs(x = "Years of education", y = "Turnout rate")

## 1.3 Linear probability model

lpm = lm(voted ~ age + education + income + female, data = df)
tidy(lpm)

preds_lpm = predict(lpm)
sum(preds_lpm < 0)
sum(preds_lpm > 1)
range(preds_lpm)

## 1.4 Logistic regression

logit =  glm(voted ~ age + education + income + female,
            family = binomial, data = df)
tidy(logit)

exp(coef(logit))

preds_logit = predict(logit, type = "response")
range(preds_logit)

## 1.5 Comparing LPM and logit

avg_slopes(logit)

modelsummary(list("LPM" = lpm, "Logit" = logit),
             vcov = list("robust", NULL), output = "markdown")

## 1.6 Predicted probabilities

p1 = plot_predictions(logit, condition = "education")
p1

ggsave("pred_prob_education.png", p1, width = 6, height = 4)

p2 = plot_predictions(logit, condition = c("age", "female"))
p2

ggsave("pred_prob_education.png", p2, width = 6, height = 4)

## 1.7 Presenting results

p3 = modelplot(list("LPM" = lpm, "Logit" = logit),
               vcov = list("robust", NULL))
p3

### Part 2: Take-Home Exercises (STAR — High School Graduation)

## 2.1 Data preparation

raw2 <- read.csv("assignment2/data/star.csv")

df2 <- raw2 %>%
  mutate(
    classtype_factor = factor(
      classtype,
      levels = c(1, 2, 3),
      labels = c("Small", "Regular", "Regular+Aide")
    ),
    race_factor = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("White", "Black", "Asian", "Hispanic", "Native American", 
                 "Other"),
    ),
    small = ifelse(classtype_factor == "Small", 1, 0)
  )

df2 = df2 %>%
  filter(!is.na(hsgrad))

nrow(df2)

# The number of observations drop from 6325 to 3047.

mean(df2$hsgrad, na.rm = TRUE)

df2 %>%
  group_by(classtype_factor) %>%
  summarise(
    grad_rate = mean(hsgrad, na.rm = TRUE),
    n = n()
  )      

# Overall, about 83% of students graduate from high school. Graduation rates are 
# slightly higher in Small (83.6%) and Regular+Aide (83.9%) classes compared to 
# Regular classes (82.5%). The differences are modest, but students in Small or 
# Regular+Aide classes show a small advantage in high school graduation.

## 2.2 LPM and logit

lpm1 <- lm(hsgrad ~ small, data = df2)
summary(lpm1)

logit1 <- glm(hsgrad ~ small, family = binomial, data = df2)
summary(logit1)

# In the LPM, the coefficient on 'small' is 0.00375.This means that students in 
# Small classes have, on average, a 0.375 percentage point higher probability of 
# graduating from high school compared to students in Regular or Regular+Aide 
# classes. The effect is very small and not statistically significant (p = 0.8), 
# suggesting that being in a Small class does not meaningfully change graduation 
# probability.

avg_slopes(logit1)

# The AME of being in a Small class from the logit model is 0.00375, which means 
# it increases the probability of graduating by about 0.375 percentage points
# compared to students not in Small classes. This is essentially identical to 
# the LPM coefficient (0.00375), confirming that both models suggest a trivial
# impact of Small class assignment on high school graduation.

## 2.3 Adding controls

lpm2 = lm(hsgrad ~ small + race_factor + yearssmall, data = df2)
summary(lpm2)

logit2 = glm(hsgrad ~ small + race_factor + yearssmall,
             family = binomial, data = df2)
summary(logit2)

# In the bivariate LPM, the coefficient on 'small' was close to zero (0.00375,
# p = 0.8), suggesting no measurable effect of Small class assignment on high 
# school graduation. After adding controls for race and years in a small class, 
# the LPM coefficient becomes slightly negative (-0.0756, p = 0.003), and the 
# logit coefficient also shows a small negative effect (-0.562, p = 0.003). The 
# change is modest, indicating that the random assignment of students to Small 
# versus other classes was generally effective, and covariates only slightly
# alter the estimated impact.

avg_slopes(logit2, variables = "yearssmall")

# The average marginal effect of 'yearssmall' is 0.0283, meaning that each 
# additional year spent in a Small class increases the probability of graduating
# from high school by about 2.8 percentage points on average, holding race and
# other variables constant. The effect is statistically significant (p < 0.001), 
# indicating that more exposure to small classes positively impacts graduation.

## 2.4 Predicted probabilities

newdata <- tibble(
  small = c(1, 0),
  yearssmall = c(3, 0),
  race_factor = factor(
    c("White", "Black"),
    levels = levels(df2$race_factor)
  )
)

pred_probs <- predict(
  logit2,
  newdata = newdata,
  type = "response"
)

bind_cols(newdata, pred_prob = pred_probs)

# For the white kid, the probabilities of graduation sit at 86.9%.
# For the black kid, the probabilities of graduation are reduced to 72.9%.

## 2.5 Interactions

logit3 <- glm(hsgrad ~ small * race_factor + yearssmall, 
              family = binomial, 
              data = df2)

summary(logit3)

avg_slopes(logit3, variables = "small", by = "race_factor")

ame_race <- avg_slopes(
  logit3, 
  variables = "small", 
  by = "race_factor"
)

print(ame_race)

# The average marginal effects (AME) show that the initial impact of being
# assigned to a small class is numerically larger for Black students (-10.3%) 
# than for White students (-7.6%). However, the interaction term in logit3 is
# not statistically significant (p = 0.69), meaning there is no solid evidence 
# that the effect actually differs by race. Results for minority groups (Asian,
# Native American, Other) are not interpretable due to insufficient sample sizes
# and lack of variance, which caused the rank deficiency and singularities in 
# the model.

## 2.6 Presenting results and discussion

models <- list(
  "LPM (Biv)" = lpm1,
  "LPM (Ctrl)" = lpm2,
  "Logit (Biv)" = logit1,
  "Logit (Ctrl)" = logit2
)

modelsummary(
  models,
  vcov = list("HC1", "HC1", NULL, NULL),
  stars = TRUE,
  coef_map = c(
    "small" = "Small Class Assignment",
    "yearssmall" = "Years in Small Class",
    "race_factorBlack" = "Black",
    "race_factorAsian" = "Asian",
    "race_factorHispanic" = "Hispanic",
    "race_factorNative American" = "Native American",
    "race_factorOther" = "Other"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  title = "Table 1: Comparison of High School Graduation Probability Models"
)

modelplot(
  models, 
  coef_map = c(
    "small" = "Small Class Assignment",
    "yearssmall" = "Years in Small Class",
    "race_factorBlack" = "Black",
    "race_factorAsian" = "Asian",
    "race_factorNative American" = "Native American",
    "race_factorOther" = "Other"
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Coefficient Plot: Predictors of High School Graduation",
    x = "Coefficient Estimate",
    y = ""
  ) +
  theme_minimal()

# The STAR data suggests that simply being assigned to a small class has 
# little impact on graduation, but the "dosage" or duration of the 
# treatment is highly significant. While the initial assignment (Small 
# Class) shows a negligible or even negative effect when controlled, 
# each additional year spent in a small class increases the probability 
# of graduation by approximately 2.7 percentage points (p < 0.001). 
# The LPM and Logit models tell a remarkably consistent story: both 
# identify the same significant predictors and agree on the direction 
# of the effects. Although their coefficients are on different scales 
# (probability vs. log-odds), their statistical significance and 
# conclusions regarding the importance of "yearssmall" are identical. 
# This experimental evidence is more credible than an observational 
# study because random assignment balances both observable and 
# unobservable characteristics (like parental motivation) across groups. 
# In an observational study, students in small classes might come from 
# wealthier districts, making it hard to separate the effect of class 
# size from the effect of socioeconomic status.

