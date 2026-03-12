##### Assignment 5: Panel Data I

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

#### Part 1: In-Class (Presidential Approval) ----------------------------

raw_1 <- read.csv("data/presidential_approval.csv")
nrow(raw)

### 1.1 Setup and data exploration ----------------------------------------

## a)

df_1 <- raw_1

length(unique(df_1$State))
length(unique(df_1$Year))
table(table(df_1$State))

# The dataset contians 50 states nd 32 years. It is not balanced. 

## b)

summary(df_1$PresApprov)

summary(df_1$UnemPct)

df_1_sub = df_1 %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_1_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")

# The line chart shows that while individual states have distinct fluctuations, 
# there is a strong tendency for states to move together over time. 
# Specifically, all three states show synchronized peaks (around 1991 and 2001) 
# and synchronized declines (the downward trend following 2001). 
# This indicates that national-level shocks—such as wars, economic crises, or 
# major political events often overwhelm state-specific factors in 
# determining presidential approval.

## c)

ggplot(df_1, aes(x = UnemPct, y = PresApprov)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relationship between Presidential Approval and Unemployment",
    subtitle = "State-year observations with linear regression line",
    x = "Unemployment Percentage (UnemPct)",
    y = "Presidential Approval (PresApprov)"
  ) +
  theme_minimal()

# The scatter plot indicates a very weak negative relationship between 
# unemployment and presidential approval. The regression 
# line shows a slight downward slope, suggesting that higher unemployment is 
# associated with marginally lower approval ratings. 
# However, the slope is nearly flat and the data points are highly dispersed, 
# meaning that state-level unemployment is a poor predictor of presidential 
# approval in this cross-sectional view. 

### 1.2 Pooled OLS -------------------------------------------------------

## a)

m_pooled = lm(PresApprov ~ UnemPct, data = df_1)
summary(m_pooled)

# The coefficient for UnemPct is -0.1438 with a p-value of 0.671. 
# This indicates that a one-percentage-point increase in state-level 
# unemployment is associated with a decrease of only 0.14 percentage points 
# in presidential approval. However, this relationship is not statistically 
# significant. In other words, in this pooled OLS model, we cannot reject the 
# null hypothesis that unemployment has no effect on approval ratings.

## b)

m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df_1)
summary(m_pooled2)

# Controlling for whether a state is in the South does change the UnemPct 
# coefficient, moving it from -0.144 to -0.347. This change occurs because 
# of Omitted Variable Bias (OVB): the 'South' variable is a significant 
# predictor of approval (with Southern states having 3.37 points higher 
# approval on average, p < 0.01). If Southern states also have systematically 
# different unemployment rates than the rest of the country, the initial 
# model was forcing UnemPct to absorb the "South effect". However, even 
# after this adjustment, the coefficient on UnemPct remains statistically 
# insignificant (p = 0.315)

## c)

# Pooled OLS is limited here because it ignores unobserved, time-invariant 
# state characteristics that correlate with both the economy and politics. 
# 1. "Political Culture": A state with a deep-rooted partisan identity (e.g., 
#    a traditionally "Red" or "Blue" state) might maintain stable approval 
#    ratings regardless of economic fluctuations.
# 2. "Industry Composition": A state reliant on a specific industry (e.g., oil 
#    in Texas or tech in California) may have unique unemployment triggers 
#    that don't reflect national presidential performance.
# 3. "Institutional Quality": Differences in state-level safety nets or labor 
#    laws could affect how much citizens "blame" the President for unemployment.
# Because these factors are "baked into" each state and don't change over 
# time, OLS attributes their influence to the regressors, leading to bias.

### 1.3 Entity fixed effects ---------------------------------------------

## a)

m_fe = feols(PresApprov ~ UnemPct | State, data = df_1)

modelsummary(list("Pooled OLS" = m_pooled, "State FE" = m_fe),
             vcov = ~State,
             stars = TRUE,
             gof_map = c("r.squared", "nobs"),
             output = "markdown")

# The coefficient on UnemPct changed from -0.144 to -0.451. While both remain 
# statistically insignificant (p > 0.1), the increase in magnitude suggests that 
# once we control for permanent differences between states, the relationship 
# between local unemployment and approval appears stronger (more negative).

## b)

# State fixed effects absorb all unobserved, time-invariant factors specific 
# to each state, such as its geographic location, historical partisan 
# baseline, and stable cultural identity. The 'South' variable drops out 
# because it is "perfectly collinear" with the state fixed effects; since 
# being in the South never changes for a given state over time, its effect 
# is mathematically indistinguishable from the unique intercept (alpha_i) 
# assigned to that state. This implies that any variable that does not 
# vary within a state over time cannot be estimated in a Fixed Effects 
# model, as the FE transformation "wipes away" all between-state variation 
# to focus exclusively on changes occurring within each state.

## c)

# The coefficient on UnemPct now identifies the "within-state" effect of 
# unemployment. The State FE estimator effectively "controls for" each state 
# by only looking at changes over time for that specific state. This differs 
# from the Pooled OLS interpretation, which compares "between-state" differences
# by asking if states with high unemployment  have lower approval than states 
# with low unemployment. By using FE, we ignore  those cross-sectional 
# comparisons and focus purely on longitudinal fluctuations, which protects the 
# estimate from being biased by permanent state traits like partisan leanings.

### 1.4 Two-way fixed effects --------------------------------------------

## a) 

m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df_1)

## b)

modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

## c)

# Year fixed effects control for "national shocks" that affect all states 
# simultaneously, such as a national war, a federal policy change, or a 
# country-wide recession. Adding them dramatically changes the UnemPct 
# coefficient from -0.451 to -1.409, and it finally becomes statistically 
# significant (p < 0.05). This suggests that common time trends were 
# previously masking the true relationship: when everyone's unemployment 
# goes up at once (national trend), approval might stay stable due to 
# shared national sentiment, but when a specific state's unemployment 
# rises relative to the national trend, the President is clearly penalized.

#### 2 Part 2: Take-Home (Teaching Evaluations) ------------------------------

raw_2 <- read_dta("data/teaching_evals.dta")
df_2 <- raw_2

### 2.1 Data exploration ------------------------------------------------

## a)

n_instructors <- n_distinct(df_2$InstrID)
n_courses <- n_distinct(df_2$CourseID)
n_obs <- nrow(df_2)

avg_obs_per_instr <- n_obs / n_instructors

cat("Unique Instructors:", n_instructors, "\n")
cat("Unique Courses:", n_courses, "\n")
cat("Average observations per instructor:", round(avg_obs_per_instr, 2), "\n")

# The dataset contains 48 unique instructors and 254 unique courses. 
# With an average of 17.52 observations per instructor, this represents 
# a "short" panel (or micro-panel). In a short panel, the number of 
# units (N = 48) is relatively large compared to the number of time 
# periods or observations per unit (T ≈ 17).

## b) 

ggplot(df_2, aes(x = Apct, y = Eval)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Grading Generosity and Teaching Evaluations",
    subtitle = "Relationship between % of A grades and student ratings",
    x = "Percent of students receiving an A/A- (Apct)",
    y = "Average Course Evaluation (Eval)"
  ) +
  theme_minimal()

# The scatter plot reveals a clear positive cross-sectional relationship 
# between grading generosity and teaching evaluations. 
# As the percentage of students receiving an A or A- (Apct) increases, 
# the average course evaluation score (Eval) also tends to rise, as 
# indicated by the upward-sloping red regression line. 
# This pattern is not specially surprising, as students may reward instructors 
# who give higher grades with better evaluations.

### 2.2 Pooled OLS baseline ---------------------------------------------

## a)

m1_2 <- lm(Eval ~ Apct + Enrollment + Required, data = df_2)

library(modelsummary)
modelsummary(m1_2, 
             stars = TRUE, 
             gof_map = c("nobs", "r.squared"),
             title = "Pooled OLS: Teaching Evaluations (m1_2)")

# The coefficient for Apct is 0.359 (p < 0.001), which indicates a 
# statistically significant positive relationship between grades and 
# evaluations. Specifically, a one-unit increase in Apct (moving from 
# 0% to 100% of A grades) is associated with a 0.359 point increase 
# in the evaluation score on a 5-point scale. Therefore, a one-percentage-point 
# increase in the share of A grades (a 0.01 change in Apct) is associated 
# with an average increase of approximately 0.0036 points in teaching 
# evaluations, holding enrollment and course requirements constant.

## b)

# The OLS estimate for Apct is likely biased because it does not control for 
# unobserved instructor traits (alpha_i) that influence both grades and ratings. 
# For example, "Instructor Charisma" might lead a teacher to be well-liked 
# while also being more relaxed about grading standards. 
# Another example is "Teaching Effort", since a highly dedicated teacher might 
# produce better learning outcomes and also receive higher 
# ratings due to their pedagogical skill. In both cases, these 
# factors create a positive correlation between the error term and Apct. 
# Therefore, the expected bias is likely upward, meaning the OLS model 
# overestimates the true "causal" effect of grading leniency on evaluations.

### 2.3 Fixed effects models -------------------------------------------------

## a) 

m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df_2)
m_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df_2)

## b)

modelsummary(
  list("Pooled OLS" = m1_2, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  title = "Impact of Grading Leniency on Evaluations"
)

## c)

# In the instructor FE model, the coefficient on Apct is 0.306 (p < 0.001). 
# This means that when a specific instructor increases their share of A 
# grades by 10 percentage points relative to their own historical average, 
# their evaluation score is predicted to rise by approximately 0.03 points. 
# The instructor fixed effect controls for all unobserved, time-invariant 
# characteristics of the teacher, such as their innate charisma, baseline 
# grading philosophy, or permanent reputation. Because the FE coefficient 
# (0.306) is smaller than the pooled OLS coefficient (0.359), it indicates 
# that the OLS estimate was biased upward. This suggests that more 
# lenient graders (those who give more As) also tend to possess unobserved 
# qualities—like being naturally more popular or engaging—that earn them 
# higher ratings regardless of the grades they give.

### 2.4 Random effects and the Hausman test ---------------------------------

## a)

pdata = pdata.frame(df_2, index = c("InstrID"))

m_re = plm(Eval ~ Apct + Enrollment + Required, 
           data = pdata, 
           model = "random")

summary(m_re)

## b) 

m_fe_plm = plm(Eval ~ Apct + Enrollment + Required, 
               data = pdata, model = "within")

hausman_results <- phtest(m_fe_plm, m_re)
print(hausman_results)

## c)

# The null hypothesis (H0) of the Hausman test is that the instructor-level 
# unobservables are uncorrelated with the regressors, meaning the Random 
# Effects (RE) estimator is consistent and efficient. With a p-value of 0.178, 
# we fail to reject the null hypothesis at the standard 5% significance level. 
# While our previous substantive reasoning suggested that unobserved traits 
# like charisma likely bias the results, the statistical evidence here does 
# not confirm that the bias is large enough to invalidate the RE model. 
# Therefore, based strictly on this test, the Random Effects estimator is 
# preferred as it is more efficient.
