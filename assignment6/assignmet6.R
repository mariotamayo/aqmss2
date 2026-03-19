##### Assignment 6: Panel Data II

library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(rlang)
library(sandwich)
library(haven)
library(fixest)
library(plm)
library(tidyr)
library(did)

#### 1 Part 1: In-Class (Card-Krueger Minimum Wage) --------------------------

### 1.1 Data setup and exploration ------------------------------------------

df <- read.csv("data/minwage.csv")

## a)

library(dplyr)
df <- df %>%
  mutate(nj = ifelse(location != "PA", 1, 0))

table(df$nj)

wage_summary <- df %>%
  group_by(nj) %>%
  summarise(
    avg_wage_before = mean(wageBefore, na.rm = TRUE),
    avg_wage_after = mean(wageAfter, na.rm = TRUE)
  )

print(wage_summary)

# The policy change had a clear impact. In New Jersey (nj = 1), the average 
# starting wage rose from $4.61 to $5.08, a significant jump that reflects 
# the new $5.05 legal minimum. Meanwhile, in Pennsylvania (nj = 0), the 
# average wage actually slightly decreased (from $4.65 to $4.61), staying 
# well below the new NJ mandate.

## b)

nj_before <- mean(df$fullBefore[df$nj == 1], na.rm = TRUE)
nj_after  <- mean(df$fullAfter[df$nj == 1], na.rm = TRUE)
nj_diff   <- nj_after - nj_before

pa_before <- mean(df$fullBefore[df$nj == 0], na.rm = TRUE)
pa_after  <- mean(df$fullAfter[df$nj == 0], na.rm = TRUE)
pa_diff   <- pa_after - pa_before

did_estimate <- nj_diff - pa_diff

cat("NJ Change:", nj_diff, "\n")
cat("PA Change:", pa_diff, "\n")
cat("DiD Estimate:", did_estimate, "\n")

# The DiD estimate of 2.927 suggests that the minimum wage increase in 
# New Jersey led to a relative increase of approximately 2.93 full-time 
# employees per restaurant compared to Pennsylvania. While NJ saw a slight 
# absolute gain in employment (+0.43), the real story is in the comparison: 
# Pennsylvania saw a sharp decline in employment (-2.49) 
# during the same period. 

## c)

df_long <- df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))

cat("Original rows:", nrow(df), "\n")
cat("Long format rows:", nrow(df_long), "\n")

# The long format is required because the lm() function expects a single 
# column for the dependent variable (full_emp). In the wide format, 
# employment is split across two columns, which prevents the model from 
# treating 'time' as an independent regressor. Reshaping the data allows 
# us to create a 'post' dummy variable and an interaction term (NJ * post).

### 1.2 DiD regression ------------------------------------------------------

## a)

m_did <- feols(full_emp ~ NJ * post, data = df_long)

modelsummary(m_did, stars = TRUE)

# The interaction term coefficient (NJ × post) is 2.927, which exactly
# matches the previous manual calculation  This represents 
# the causal effect of the minimum wage hike on employment. 
# Because the coefficient is positive, it suggests that New Jersey 
# restaurants added about 2.93 more full-time employees per store 
# relative to the Pennsylvania control group. With a p-value < 0.1,
# the result is marginally statistically significant.

## b)

m_did_chain <- feols(full_emp ~ NJ * post | chain, data = df_long)

modelsummary(list("Base DiD" = m_did, "Chain FE" = m_did_chain), stars = TRUE)

# The DiD estimate (NJ x post) remains exactly 2.927 in both models. 
# This indicates that the distribution of chains (Wendy's, KFC, etc.) 
# is balanced across both New Jersey and Pennsylvania. Because 'Chain' 
# is not correlated with the 'NJ' treatment, adding chain fixed effects 
# does not change the treatment estimate. However, the chain fixed 
# effects are absorbing the "brand-level" baseline employment. This is 
# evidenced by the jump in R-squared from 0.008 to 0.066 and the slight
# decrease in the Standard Error of the DiD estimate (from 1.580 to 1.536), 
# as the model now accounts for brand-specific noise.

## c)

# The parallel trends assumption states that in the absence of the 
# minimum wage hike, employment in New Jersey would have followed the 
# same trend as employment in Pennsylvania over the same period. 
# To be confident in the DiD estimate, we would need to observe that 
# NJ and PA employment were moving in parallel (same slopes) during 
# several time periods BEFORE 1992. A concrete violation would be 
# a state-specific economic shock (for instance, if New Jersey 
# implemented a new tax on fast-food packaging at the same time as 
# the wage hike, while Pennsylvania did not). This would decrease NJ 
# employment independently of the minimum wage, biasing our results.

### 1.3 Wages as a validation check -----------------------------------------

## a)

df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

modelsummary(m_wage, stars = TRUE)

# The interaction term (post × NJ) is 0.510 and is highly statistically 
# significant (p < 0.001). This means that starting wages in New Jersey 
# restaurants rose by $0.51 more than they did in Pennsylvania after the 
# law change. The sign is positive as expected, and the magnitude is 
# substantial, confirming that the minimum wage policy successfully 
# "treated" the NJ labor market by forcing firms to pay higher base 
# rates. 

## b)

# The wage result serves as a "first-stage" validation of the experiment. 
# It confirms that the treatment (the minimum wage law) was actually 
# "binding", meaning that it forced firms to change their behavior and 
# increase labor costs. If wages had not risen in NJ, the finding that 
# employment didn't fall would be meaningless: it would simply mean 
# the law was ignored or that everyone was already being paid above 
# the new minimum. It is reassuring that wages rose because it proves 
# we are testing a genuine economic trade-off: NJ firms faced a real 
# increase in the price of labor, yet they did not respond by 
# reducing the quantity of labor (employment).

#### 2 Part 2: Take-Home (Staggered DiD with the did Package) ----------------

library(did)
data(mpdta)

### 2.1 Data structure and visualization ------------------------------------

## a)

cohort_counts <- mpdta %>%
  group_by(first.treat) %>%
  summarise(n_counties = n_distinct(countyreal))

print(cohort_counts)

year_counts <- table(mpdta$year)
print(year_counts)

# Staggered treatment adoption means that different units (counties) 
# begin treatment at different points in time rather than all at once. 
# In this dataset, some counties start in 2004, some in 2006, and some 
# in 2007. It is problematic to simply compare treated vs. untreated 
# counties because the "untreated" group is constantly shrinking and 
# the timing of adoption may be related to local economic trends. 

## b)

mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() + 
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

ggsave("plots/cohort_trends.png")

# The visual evidence suggests that the "Adopted 2006" and "Adopted 2007" 
# cohorts follow a similar downward trajectory to the "Never treated" 
# group between 2003 and 2005. This alignment is a good sign for the 
# parallel trends assumption for those specific groups. However, the 
# "Adopted 2004" cohort is more problematic because we only have one 
# year of pre-treatment data (2003), and its initial drop into 2004 is 
# much steeper than the control group's trend. After their respective 
# treatments, most cohorts show a visible "dip" or a flattened slope 
# compared to the "Never treated" group, which suggests that adopting 
# the policy may have negatively impacted log teen employment.

### 2.2 Naive TWFE vs. Callaway-Santa ́nna estimator ------------------------

## a)

mpdta = mpdta %>%
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat))

m_naive = feols(lemp ~ treated_post | countyreal + year, data = mpdta, cluster = ~countyreal)

modelsummary(m_naive, stars = TRUE)

# The coefficient on 'treated_post' is -0.037, which is statistically 
# significant at the 1% level (p < 0.01). This suggests that, on 
# average, adopting the policy is associated with a 3.7% decrease 
# in log teen employment across all treated counties. 

# This model pools all treatment cohorts together, which relies on the 
# assumption of 'Treatment Effect Homogeneity.' Specifically, it 
# assumes that:

# 1. The treatment effect is constant across all cohorts (e.g., the 
#    2004 group experiences the same 3.7% drop as the 2007 group).

# 2. The treatment effect is constant over time (e.g., the impact 
#    does not grow or fade the longer a county stays treated).

# 3. There are no 'forbidden comparisons' where already-treated 
#    units (like the 2004 cohort in 2007) serve as a control group 
#    for newly-treated units.

## b)

results_cs = att_gt(yname = "lemp",
                    tname = "year",
                    idname = "countyreal",
                    gname = "first.treat",
                    control_group = "nevertreated",
                    data = mpdta)

overall_att = aggte(results_cs, type = "simple")
print(overall_att)

# Naive TWFE: -0.037 (p < 0.01)
# CS Overall: -0.040 (95% CI: -0.063, -0.016)

# The CS estimate is slightly more negative than the naive model. 
# While both indicate a roughly 4% decrease in teen employment, 
# the CS estimator is more reliable because it avoids 'negative 
# weighting', a common bias in staggered designs where early-treated 
# units are used as controls for late-treated units. By using 
# only the 'Never Treated' group as a baseline, the CS model 
# ensures that the dynamic effects of the 2004 cohort don't 
# contaminate the estimates for the 2007 cohort.

## c)

event_study = aggte(results_cs, type = "dynamic")
summary(event_study)

p_event = ggdid(event_study) + 
  theme_minimal() +
  labs(title = "Event Study: Dynamic Effects on Log Teen Employment")

ggsave("plots/event_study_plot.png", plot = p_event)

# 1. Pre-treatment estimates (Leads -3, -2, -1):
# The red points represent the periods before the policy was adopted. 
# Because the 95% confidence intervals for all three pre-periods 
# overlap with the horizontal zero line, these estimates are NOT 
# statistically distinguishable from zero. This is a critical result 
# as it suggests the 'Parallel Trends' assumption holds: the treated 
# and control counties were on similar paths before the law changed.

# 2. Post-treatment estimates (Lags 0, 1, 2, 3):
# The teal points show a clear downward trajectory after adoption. 
# While the effect at Time 0 is relatively small, the coefficients 
# for periods 1, 2, and 3 become increasingly negative and 
# statistically significant (their intervals do not cross zero).

# 3. Dynamic Effects:
# The plot reveals that the treatment effect is not 'static' or 
# immediate. Instead, there is a dynamic 'ramping up' effect where 
# the negative impact on log teen employment grows over time, reaching 
# a drop of roughly 10% to 14% by the third year. This explains why 
# the Naive TWFE model—which just averages these different points—might 
# have underestimated the long-term severity of the policy.


### 2.3 Pre-testing the parallel trends assumption --------------------------

## a)

results_cs_test = att_gt(yname = "lemp",
                         tname = "year",
                         idname = "countyreal",
                         gname = "first.treat",
                         control_group = "nevertreated",
                         data = mpdta,
                         bstrap = TRUE,     
                         cband = TRUE)

summary(results_cs_test)

# The p-value for the joint pre-test is 0.16812.

# This Chi-square test evaluates the null hypothesis (H0) that the 
# treatment effect is zero for all cohorts in all periods before 
# they were actually treated. A large p-value (0.168) tells us that 
# we cannot reject the null hypothesis. This means there is no statistically 
# significant evidence that the parallel trends assumption has been violated. 
# It confirms that the treated and control groups were moving 
# together before the policy started, making our later estimate 
# of a -4% effect much more credible as a causal result.

## b)

p_all_gt = ggdid(results_cs_test) +
  theme_minimal() +
  labs(title = "Group-Time ATTs by Treatment Cohort")

p_all_gt

ggsave("plots/group_time_atts.png", plot = p_all_gt)

# Across all three panels (Groups 2004, 2006, and 2007), the pre-treatment 
# ATT(g,t) estimates are clustered closely around the dashed zero line. 
# This confirms that there were no significant differential trends between 
# these treatment groups and the never-treated counties before the policy 
# was enacted, providing visual support for the parallel trends assumption.
# Group 2004 shows the most immediate and pronounced drop, with estimates 
# becoming increasingly negative and statistically significant over time. 
# Group 2006 and 2007 also show negative point estimates after treatment, 
# but their effects are smaller and less precisely estimated.
# The consistent "near-zero" pre-treatment coefficients across all groups 
# validate the research design. The variability in post-treatment results 
# demonstrates that the "average" effect calculated in the naive model 
# was largely driven by the early (2004) adopters.

## c)

# A non-significant pre-test (p = 0.168) provides evidence that treated and 
# control units followed similar paths historically, but it cannot guarantee 
# that they would have continued to do so in the absence of treatment. The 
# test only validates the "pre-period" trends and  cannot account for 
# unobserved shocks that might have occurred simultaneously with the 
# treatment (confounding factors) or "selection into treatment" based on 
# anticipated future shocks. 


### 2.4 Comparing control group specifications ------------------------------

## a)

results_cs_nyt = att_gt(yname = "lemp",
                        tname = "year",
                        idname = "countyreal",
                        gname = "first.treat",
                        control_group = "notyettreated", # Switch control group
                        data = mpdta,
                        bstrap = TRUE,
                        cband = TRUE)

overall_att_nyt = aggte(results_cs_nyt, type = "simple")
print(overall_att_nyt)

# Comparison of Control Group Specifications:
# 1. Never-Treated: -0.0400 (SE: 0.0120)
# 2. Not-Yet-Treated:    -0.0398 (SE: 0.0121)

# The estimates are nearly identical in both sign and magnitude. This 
# suggests that the "Not-Yet-Treated" counties (those slated for 
# treatment in 2006 or 2007) were behaving very similarly to the 
# "Never-Treated" counties during the early years of the study.

## b)

event_study_nyt = aggte(results_cs_nyt, type = "dynamic")

p_event_nyt = ggdid(event_study_nyt) + 
  theme_minimal() +
  labs(title = "Event Study: Not-Yet-Treated Control Group")

p_event_nyt

ggsave("plots/event_study_nyt_plot.png", plot = p_event_nyt)

# Using the broader "not-yet-treated" control group does not change our 
# core conclusions. The consistency between the two specifications 
# serves as a robustness check, confirming that our results are not 
# sensitive to the specific definition of the control group.

## c)

# The primary trade-off is between internal validity (bias) and statistical 
# power (precision). I would prefer the 'never-treated' group when there 
# is a high risk of 'anticipation effects', where units slated for future 
# treatment change their behavior before the policy actually starts, as 
# this would contaminate the control group and bias the ATT.Conversely, the 
# 'not-yet-treated' group is preferable when the 'never-treated' sample is 
# very small or unrepresentative. By including future-treated units as controls,
# we increase the sample size and precision of our estimates. 

### 2.5 Discussion: why does TWFE fail in staggered settings? ---------------

## a)

# Naive TWFE can produce misleading results bc it estimates a weighted average 
# of all possible 2x2 DiD comparisons, including "forbidden comparisons" where 
# already-treated units (early adopters) serve as controls for late adopters. 
# If treatment effects are heterogeneous, meaning the policy's impact grows or 
# changes over time, these early adopters are no longer a stable baseline. This 
# can lead to "negative weighting", where a true negative policy effect is 
# mathematically masked or even flipped in sign, making the resulting TWFE
# coefficient a biased and unreliable measure of the actual ATT.

## b)

# Naive TWFE (Section 2.2a): -0.0374 (p < 0.01)
# Callaway-Sant'Anna (Section 2.2b): -0.0400 (p < 0.05)

# Based on the event-study results, the Callaway-Sant'Anna estimate is 
# significantly more credible. The event study revealed strong evidence 
# of 'dynamic effects' (the negative impact on employment grows larger 
# the longer a county remains treated). Because the naive TWFE model 
# incorrectly uses early-treated units (whose employment is still 
# dropping) as controls for later-treated units, it likely suffered 
# from 'negative weighting bias,' which slightly attenuated (washed out) 
# the true effect.
