##### Assignment 9: Other Outcomes (Ordinal, Multinomial, Count, Survival) -----

#### 1 Part 1: In-Class (Ordinal, Multinomial, and Count Outcomes) -----------

library(carData)
library(MASS)
library(nnet)
library(marginaleffects)
data(BEPS)

### 1.1 Ordered logit: perceptions of the national economy ----------------------

## a)

table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

# The distribution across the five categories is:
#   - 1 (Much worse): 37
#   - 2 (Worse): 257
#   - 3 (Same): 607
#   - 4 (Better): 542
#   - 5 (Much better): 82
# The most common category is 3 ("Stayed the same"), with 607 observations.

# Using OLS on this variable is problematic because it assumes "equal spacing" 
# between categories. OLS treats the difference between 1 and 2 as 
# mathematically identical to the difference between 3 and 4. In reality, 
# the psychological distance between "much worse" and "worse" is likely 
# much larger than the distance between "stayed the same" and "a little better". 
# Ordered logit avoids this by estimating thresholds that allow the 
# distances between categories to vary on the underlying latent scale.

## b)

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)

summary(m_ologit)

# Raw coefficient on Europe: -0.1227 (negative).

# According to the model, a higher support for European integration (higher 
# values of 'Europe') predicts LESS optimistic views of the national economy. 
# Because the sign is negative, as support for Europe increases, the 
# probability of being in a higher category of economic perception 
# (e.g., "Much Better") decreases, while the probability of being 
# in a lower category (e.g., "Much Worse") increases. 

## c)

avg_slopes(m_ologit)

# AMEs for Europe across categories (rounded):
#   - P(1 - Much Worse): +0.0029
#   - P(2 - Worse):      +0.0158
#   - P(3 - Same):       +0.0098
#   - P(4 - Better):     -0.0222
#   - P(5 - Much Better):-0.0062

# A one-unit increase in pro-Europe attitude decreases the probability of 
# perceiving the economy as improved. Specifically, for every one-unit 
# increase on the Europe scale, the probability of choosing category 4 
# ("Better") drops by approximately 2.2 percentage points, and category 5 
# ("Much Better") drops by about 0.6 percentage points. Conversely, the 
# probability of choosing the "pessimistic" categories increases 
# (summing categories 1, 2, and 3). 

## d)

predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))

# Predicted Probabilities at Covariate Means:

# Category 1 (Much Worse): Female = 0.0267 (2.67%) | Male = 0.0222 (2.22%)
# Category 5 (Much Better): Female = 0.0421 (4.21%) | Male = 0.0506 (5.06%)

# Comparing the extremes, women are slightly more likely than men to be in 
# the most pessimistic category (2.67% vs 2.22%), while men are more likely 
# to occupy the most optimistic category (5.06% vs 4.21%). While these 
# absolute differences are small (less than 1 percentage point), the trend 
# is consistent across the spectrum: men have higher predicted probabilities 
# for all "improvement" categories (4 and 5), while women have higher 
# probabilities for all "worsening" categories (1 and 2). This suggests a 
# modest but statistically significant gender gap where male respondents 
# held more positive views of the British national economy in 1997.

### 1.2 Multinomial logit: vote choice --------------------------------------

## a)

BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)

summary(m_mlogit)

# Blair Coefficient (Labour vs. Conservative): 0.8158 (positive).

# The coefficient for 'Blair' in the Labour equation is positive (0.8158). 
# Substantively, this means that as a respondent's positive feelings 
# toward Tony Blair increase, the likelihood of voting for Labour 
# relative to voting for the Conservatives increases significantly. 
# Because multinomial logit uses a reference category, this positive 
# value indicates that Blair's popularity was a powerful wedge that 
# pulled voters away from the Conservative base and toward the Labour Party.

## b)

avg_slopes(m_mlogit)

# AME of Blair on P(Labour): 0.1156
# Standard Error: 0.0092

# On average, and across all respondents, a one-unit increase in Blair approval 
# increases the probability of voting for Labour by approximately 11.56 
# percentage points, holding all other variables constant. This effect 
# is highly statistically significant (p < 0.001). 

# Interestingly, the AMEs for the other parties are negative: a one-unit 
# increase in Blair approval reduces the probability of voting Conservative 
# by 7.28 percentage points and Liberal Democrat by 4.28 percentage points. 
# This confirms that Blair's popularity didn't just mobilize the Labour 
# base, but actively siphoned support away from both the Conservatives 
# and the Liberal Democrats.

## c)

# In this application, IIA implies that if the Liberal Democrat party were 
# removed from the ballot, the voters who previously chose them would 
# redistribute their votes to Labour and the Conservatives in exact 
# proportion to those parties' current relative odds. Substantively, it 
# assumes that a voter’s preference between Labour and Conservative is 
# entirely independent of whether a third "center" option exists.

# It is unlikely that IIA strictly holds here because the Liberal Democrats 
# and Conservaties are often viewed as "close substitutes" on the center-right 
# of the ideological spectrum. If the Liberal Democrats were to drop out, 
# their supporters would likely disproportionately flock to th Cnservaties  
# rather than Labour. This "similarity" would violate the IIA assumption 
# that all alternatives are distinct and non-substitutable.

### 1.3 Poisson regression: publication counts ------------------------------

library(pscl)
library(AER)
library(MASS)
library(marginaleffects)
data(bioChemists)

## a)

summary(bioChemists$art)
var(bioChemists$art)

pdf("art_histogram.pdf", width = 6, height = 4)
hist(bioChemists$art, breaks = 20, main = "Distribution of articles",
     xlab = "Number of articles", col = "gray80")

# Mean of articles: 1.693
# Variance of articles: 3.710

# The mean of 'art' is 1.693, while the variance is 3.710. In a Poisson 
# distribution, we assume equidispersion, meaning the mean should equal 
# the variance (E[Y] = Var[Y]). Here, the variance is more than double 
# the mean, which indicates clear overdispersion. Substantively, this 
# suggests that the number of articles published varies much more across 
# students than a simple Poisson process would predict—likely because 
# of the long tail of high-achievers visible in the histogram.

## b)

m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)

summary(m_pois)

exp(coef(m_pois)["ment"])

# (1) IRR for 'ment'
# Coefficient: 0.025543
# exp(0.025543) = 1.025872

# The coefficient on 'ment' is 0.0255. Its exponentiated form (IRR) is 
# approximately 1.026. This implies that a one-unit increase in mentor 
# articles multiplies the expected number of student articles by 1.026, 
# which is equivalent to a 2.6% increase in expected publications, 
# holding other factors constant.

# (2) Dispersion Ratio
# Residual Deviance: 1634.4
# Degrees of Freedom: 909
# Ratio: 1634.4 / 909 = 1.798

# The ratio of residual deviance (1634.4) to degrees of freedom (909) 
# is 1.80. While this is not strictly above 2, it is notably higher than 1, 
# suggesting that the Poisson model suffers from moderate overdispersion. 
# This means the variance is greater than the mean, and the Poisson model 
# may be underestimating the standard errors of our coefficients.

## c)

dispersiontest(m_pois)

# Dispersion estimate: 1.82454.
# p-value: 3.681e-09 (p < 0.001).

# The estimated dispersion parameter is 1.82, which is significantly 
# greater than the value of 1 assumed by the Poisson distribution. 
# With a p-value of 3.68e-09, we reject the null hypothesis of 
# equidispersion. There is strong, statistically significant evidence 
# of overdispersion in the publication data.

# This implies that the standard errors from the Poisson model are 
# artificially small (deflated). Consequently, the z-scores and 
# significance levels (p-values) in the Poisson model are likely 
# overstated, potentially leading to false-positive results (Type I errors). 
# To obtain valid standard errors and more reliable inferences, we 
# should use a Negative Binomial model.


### 1.4 Negative binomial regression ----------------------------------------

## a)

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)

summary(m_nb)

# Comparison of 'ment' coefficient:
#   - Poisson Estimate: 0.0255
#   - NB Estimate:      0.0291

# The coefficient for 'ment' increased slightly from 0.0255 in the Poisson 
# model to 0.0291 in the Negative Binomial model, suggesting the direction 
# and general magnitude of the effect are stable. However, the standard error 
# for 'ment' increased from 0.0020 to 0.0032.

# The estimated overdispersion parameter (Theta) is 2.264. Since this value 
# is relatively small (far from infinity), it indicates that overdispersion 
# in the article counts is quite substantial. The NB model is therefore 
# preferred over the Poisson model.

## b)

AIC(m_pois, m_nb)

# AIC Poisson: 3314.11
# AIC NegBin:  3135.92

# The Negative Binomial model has a significantly lower AIC (3135.92) compared 
# to the Poisson model (3314.11). A difference of nearly 178 AIC units is 
# overwhelming evidence that the Negative Binomial model provides a much 
# better fit to the data. 

# This comparison implies that overdispersion is a major characteristic of 
# this dataset that must be addressed. Even though the Negative Binomial 
# model is more complex (it estimates an additional dispersion parameter, 
# theta), the massive improvement in fit far outweighs the penalty for 
# complexity.

## c)

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

# Predicted Articles at Covariate Means:
#   - Men:   2.05 [95% CI: 1.80, 2.32]
#   - Women: 1.65 [95% CI: 1.44, 1.88]

# Holding all other variables at their means, men are predicted to publish 
# approximately 2.05 articles, while women are predicted to publish 1.65. 

# This represents a gender gap of approximately 0.40 articles over a 
# three-year period. While the 95% confidence intervals show a slight 
# overlap (between 1.80 and 1.88), the underlying regression model shows 
# that the 'femWomen' coefficient is statistically significant (p = 0.0029). 

# This indicates that although the uncertainty intervals are close, we can 
# still confidently reject the null hypothesis that men and women publish 
# at the same rate in this population, thus demonstrating that there exists 
# a significant productivity gap between male and female biochemistry 
# students in this dataset.

## d)

# Based on the analysis, a Poisson regression is inadequate for this dataset 
# due to significant overdispersion. The variance (3.71) is more than double 
# the mean (1.69), and the formal dispersion test (p < 0.001) confirms that 
# the Negative Binomial model is required to obtain valid standard errors. 
# The model reveals that mentor productivity is a powerful predictor of 
# student success, with an Incidence Rate Ratio (IRR) of approximately 1.03, 
# meaning each additional mentor publication increases a student's expected 
# article count by about 3%. In the final Negative Binomial model, gender 
# (being female), marital status, number of young children, and mentor 
# productivity are all statistically significant predictors, while PhD 
# program prestige is not. Substantively, these results suggest that 
# academic productivity is driven by a combination of professional 
# mentorship and domestic factors, as evidenced by the significant 
# "productivity penalties" associated with being a woman or having 
# children under age five.

#### 2 Part 2: Take-Home (Survival Analysis) ---------------------------------

library(survival)
library(broom)
library(ggplot2)
library(marginaleffects)

lung_df <- survival::lung

lung_df$dead <- lung_df$status - 1

### 2.1 Kaplan-Meier survival curves ----------------------------------------

## a)

n_total <- nrow(lung_df)
n_total

n_deaths <- sum(lung_df$dead == 1)
n_deaths

n_censored <- sum(lung_df$dead == 0)
n_censored

prop_censored <- n_censored / n_total
prop_censored

#   - Total observations: 228
#   - Number of deaths (events): 165
#   - Number of censored cases: 63
#   - Proportion censored: ~27.6%

# Approximately 28% of the patients are censored. In the context of 
# clinical trials for advanced lung cancer, this is a moderate amount 
# of censoring. It signifies that for nearly one-third of our sample, 
# the "true" survival time is unknown. We only know they survived at least
# as long as their recorded 'time'. While 28% is not so high as to make 
# the data unusable, it is high enough that using standard OLS (which 
# would treat 400+ days as exactly 400 days) would severely underestimate 
# life expectancy and bias our results.

## b)

km_fit <- survfit(Surv(time, dead) ~ 1, data = lung_df)
print(km_fit)

# The median survival time of 310 days means that at this point in time, 
# half of the patients in the sample have died and half are still alive. 

# The 95% confidence interval [285, 363] indicates that if we were to 
# repeat this study many times with different samples from the same 
# population, the median survival time would fall within this range 95% 
# of the time. This interval gives us a sense of our precision.

## c)

km_sex <- survfit(Surv(time, dead) ~ sex, data = lung_df)

log_rank_test <- survdiff(Surv(time, dead) ~ sex, data = lung_df)
print(log_rank_test)

km_sex_df <- tidy(km_sex)

km_sex_df$sex <- factor(km_sex_df$strata, labels = c("Male", "Female"))

surv_plot <- ggplot(km_sex_df, aes(x = time, y = estimate, color = sex, fill = sex)) +
  geom_step(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Survival Curves by Sex",
       x = "Days", y = "Survival Probability") +
  theme_minimal()

surv_plot

ggsave("plots/survival_by_sex.pdf", surv_plot, width = 8, height = 6)

# Visually, females (sex=2) survive longer than males (sex=1). While the 
# 95% confidence interval ribbons do overlap, particularly in the later 
# stages of the study (after ~750 days), the overall trend shows a clear 
# separation for the first two years.

# The log-rank test produces a p-value of 0.001. This test evaluates the 
# null hypothesis that there is no difference between the survival 
# populations. Since p < 0.05, we reject the null hypothesis and conclude 
# that the survival curves are statistically different; females have a 
# significantly higher probability of survival than males in this cohort.

### 2.2 Cox proportional hazards model --------------------------------------

## a)

m_cox <- coxph(Surv(time, dead) ~ age + sex + ph.ecog, data = lung_df)
summary(m_cox)

#   - Hazard Ratio (HR) for sex: 0.575
#   - p-value for sex: 0.000986

# The hazard ratio for sex is 0.575. Since women are coded as 2 and men 
# as 1, this means that for women, the hazard (risk of death) at any 
# given time is approximately 57.5% that of men, holding age and 
# ECOG performance score constant. This is a 42.5% reduction in the 
# risk of death, which is a substantial survival advantage. This effect 
# is highly statistically significant (p < 0.001).

## b)

# The hazard ratio for ph.ecog is 1.59. This means that a one-unit increase 
# in the ECOG performance score (which represents a decline in physical 
# functioning, moving from "active" toward "bedridden") is associated with 
# a 59% higher hazard of death, holding age and sex constant. # Substantively, 
# this indicates that a patient's functional status is a critical predictor
# of survival; for every step higher on the ECOG scale, the instantaneous 
# risk of mortality increases by over half.

## c)

ph_test <- cox.zph(m_cox)
print(ph_test)

#   - age: p = 0.66
#   - sex: p = 0.13
#   - ph.ecog: p = 0.15
#   - GLOBAL: p = 0.22

# All p-values are greater than 0.05, meaning we fail to reject the null 
# hypothesis of proportional hazards for any individual variable or the 
# model as a whole. This indicates that the assumption holds: the effects 
# of age, sex, and physical performance on the hazard of death remain 
# constant over time. Substantively, this confirms that the 42.5% survival 
# advantage for women we found earlier isn't just an early-stage effect; 
# but a consistent "benefit" that applies regardless of whether the patient 
# is at day 10 or day 500 of the study.

## d)

# The Kaplan-Meier analysis revealed a clear survival advantage for female 
# patients, a difference confirmed as statistically significant by the 
# log-rank test (p = 0.001). In the multivariate Cox model, both sex and 
# physical performance (ph.ecog) were highly significant predictors, while 
# age was not statistically significant after adjusting for health status. 
# Specifically, being female was associated with a 42.5% reduction in the 
# hazard of death, whereas each one-unit increase in the ECOG score 
# increased the hazard by 59%. Formal testing via Schoenfeld residuals 
# confirmed that the proportional hazards assumption holds for all 
# variables (Global p = 0.22), meaning these risk factors remain constant 
# over time. Substantively, these results suggest that for patients with 
# advanced lung cancer, functional physical status and biological sex are 
# far more critical indicators of life expectancy than chronological age.
