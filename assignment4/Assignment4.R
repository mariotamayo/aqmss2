#### Assignment 4: Model Interpretation and Diagnostics

library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(rlang)
library(sandwich)
library(haven)

### 1. Part 1: In-Class (Corruption and Wealth)

raw <- read_dta("data/corruption.dta")
nrow(raw)

## 1.1 Setup and data exploration

df1 <- raw %>%
  mutate(
    country = cname,
    corruption = ti_cpi,
    GDPcapita = undp_gdp
  ) %>%
  filter(!is.na(corruption) & !is.na(GDPcapita)) %>%
  select(country, corruption, GDPcapita, everything())

# 170 countries remain in the dataset.

summary(df1$corruption)
sd(df1$corruption)

summary(df1$GDPcapita)
sd(df1$GDPcapita)

# The Corruption index ranges from 1.2 to 9.7 with a standard deviation of 2.11. 
# GDP per capita ranges from $520 to $61,190 with a standard deviation of 
# $9,986.8. The GDP per capita is heavily right-skewed; the mean ($8,950) is 
# significantly higher than the median ($5,280), indicating that a few very 
# wealthy countries pull the average upward while most countries are at the 
# lower end.

## 1.2 Exploratory visualization

ggplot(df1, aes(x = GDPcapita, y = corruption)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Relationship between GDP per Capita and Corruption",
    subtitle = "Higher index value = Less corruption",
    x = "GDP per Capita (PPP, dollars)",
    y = "Corruption Perceptions Index (0-10)"
  ) +
  theme_minimal()

# The relationship looks fairly linear. 

ggplot(df1, aes(x = log(GDPcapita), y = corruption)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Corruption vs. Log(GDP per Capita)",
    subtitle = "Log transformation linearizes the relationship",
    x = "Log of GDP per Capita",
    y = "Corruption Perceptions Index (0-10)"
  ) +
  theme_minimal()

# It looks even more linear, but the data points exhibit a curved pattern. 

## 1.3 Bivariate regression

m1_1 <- lm(corruption ~ GDPcapita, data = df1)
summary(m1_1)

# The coefficient for GDPcapita is 1.730e-04 (0.000173). This means that 
# for every additional dollar of GDP per capita, the corruption index 
# is predicted to increase by 0.000173 points. For a $10,000 increase 
# in GDP per capita, the model predicts an increase of 1.73 points in 
# the corruption index (10,000 * 0.000173), suggesting that wealthier 
# nations tend to be significantly more transparent.

q25 <- quantile(df1$GDPcapita, 0.25)
q75 <- quantile(df1$GDPcapita, 0.75)

print(paste("Percentil 25:", q25))
print(paste("Percentil 75:", q75))

pred_m1_1 <- predictions(m1_1, newdata = datagrid(GDPcapita = c(q25, q75)))
pred_m1_1

# At the 25th percentile of GDP ($1,974), the predicted corruption score 
# is 2.84 (95% CI: 2.62, 3.07). At the 75th percentile ($10,862), the 
# predicted score is 4.38 (95% CI: 4.20, 4.57). The difference in 
# predicted corruption between these two levels of wealth is 1.54 points 
# on the 0-10 scale.

## 1.4 Non-linear specifications

m2_1 <- lm(corruption ~ log(GDPcapita), data = df1)
summary(m2_1)

plot_predictions(m2_1, condition = "GDPcapita") +
  labs(
    title = "Predicted Corruption Scores (Log Model)",
    subtitle = "The curve shows diminishing marginal returns of wealth on transparency",
    x = "GDP per Capita (Original Scale)",
    y = "Predicted Corruption Index"
  ) +
  theme_minimal()

m3_1 <- lm(corruption ~ GDPcapita + I(GDPcapita^2), data = df1)
summary(m3_1)

r2_m1 <- summary(m1_1)$r.squared
r2_m2 <- summary(m2_1)$r.squared
r2_m3 <- summary(m3_1)$r.squared

cat("R2 Lineal (m1_1):", r2_m1, "\n",
    "R2 Logarítmico (m2_1):", r2_m2, "\n",
    "R2 Cuadrático (m3_1):", r2_m3)

# The quadratic model (m3_1) provides the best fit for the data, as it 
# achieves the highest R-squared (0.710), explaining 71% of the total 
# variance in corruption. A non-linear specification is appropriate 
# because the relationship between wealth and institutional quality 
# is not constant. While the log model (m2_1) also captures curvature,
# in this specific dataset, the quadratic form (m3_1) offers a superior 
# mathematical fit to the observed distribution of country scores.

## 1.5 Marginal effects

ame_m2_1 <- avg_slopes(m2_1, variables = "GDPcapita")
ame_m2_1

# The AME (0.000524) differs from the raw log coefficient because the 
# latter represents the effect of a proportional (percentage) change 
# in wealth, while the AME translates this into the original scale of 
# dollars. In substantive terms, the AME tells us that, on average 
# across all countries in the sample, a one-dollar increase in GDP 
# per capita is associated with a 0.000524 point increase in the 
# corruption index.

mfx_m3_1 <- slopes(m3_1, 
                 variables = "GDPcapita",
                 newdata = datagrid(GDPcapita = c(2000, 10000, 30000)))
mfx_m3_1

# The marginal effect of GDP clearly diminishes as countries become richer. 
# At $2,000, a one-dollar increase in GDP per capita is associated with 
# a 0.000254 point increase in the corruption index. This effect drops 
# to 0.000214 at $10,000 and falls significantly to 0.000114 at $30,000. 
# This confirms the presence of diminishing marginal returns

## 1.6 Prediction plots

p_log <- plot_predictions(m2_1, condition = "GDPcapita") +
  labs(
    title = "Predicted Corruption Scores (Log-Linear Model)",
    subtitle = "Higher GDP is associated with lower corruption, but at a decreasing rate",
    x = "GDP per capita (PPP, dollars)",
    y = "Predicted Corruption Index (0-10)"
  ) +
  theme_minimal()

print(p_log)

ggsave("assignment4/prediction_plot_log.png", plot = p_log, width = 8, height = 6, dpi = 300)

p_quad <- plot_predictions(m3_1, condition = "GDPcapita") +
  labs(
    title = "Predicted Corruption Scores (Quadratic Model)",
    subtitle = "The parabolic fit captures the diminishing returns of wealth",
    x = "GDP per capita (PPP, dollars)",
    y = "Predicted Corruption Index (0-10)"
  ) +
  theme_minimal()

print(p_quad)

ggsave("assignment4/prediction_plot_quadratic.png", plot = p_quad, width = 8, height = 6, dpi = 300)

# Both models tell a similar story regarding the overall trend: there is 
# a strong positive relationship between wealth and transparency, characterized 
# by diminishing marginal returns. However, they diverge significantly in 
# their functional form. The log model (m2_1) shows a much sharper increase 
# in transparency for low-income countries that then levels off into a 
# steady, asymptotic growth. In contrast, the quadratic model (m3_1) follows 
# a smoother parabolic arc that eventually peaks around $50,000 before 
# showing a slight downward turn.

## 1.7 Residual diagnostics

diag_data <- augment(m1_1)

p_resids <- ggplot(diag_data, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) + # Línea de tendencia
  labs(
    title = "Residuals vs. Fitted Values (Linear Model)",
    x = "Fitted Values (Predicted Corruption)",
    y = "Residuals"
  ) +
  theme_minimal()

print(p_resids)

diag_data_log <- augment(m2_1)

p_resids_log <- ggplot(diag_data_log, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(
    title = "Residuals vs. Fitted Values (Log-Linear Model)",
    x = "Fitted Values (Predicted Corruption)",
    y = "Residuals"
  ) +
  theme_minimal()

print(p_resids_log)

# The log transformation appears to improves the residual pattern by 
# reducing heteroskedasticity and mitigating the impact of high-income 
# outliers. While the linear model's residuals show a drastic "fan" shape 
# and a sharp downward bias, the log-linear residuals are more uniformly 
# distributed around zero. This suggests that the log model captures the 
# underlying non-linear relationship much more effectively, although the 
# remaining slight U-shape indicates there is still room for improvement.

n <- nrow(df1)
threshold <- 4 / n

cooks_dist <- cooks.distance(m2_1)

influential_indices <- which(cooks_dist > threshold)
influential_countries <- df1[influential_indices, ]

influential_countries[, c("country", "corruption", "GDPcapita")]
print(paste("Umbral 4/n:", round(threshold, 4)))

# These observations should not be removed lightly, as they represent 
# real-world diversity in institutional quality. Countries like Singapore 
# or Denmark are not "errors" but examples of high performance, while 
# Equatorial Guinea illustrates how resource wealth can coexist with high 
# corruption. Removing them would artificially inflate the model's 
# fit and bias the results. Instead, I recommend a robustness check 
# by re-estimating the model without these 14 cases to see if the 
# coefficient on log(GDPcapita) remains stable.

## 1.8 Publication-quality table

regression_table <- modelsummary(
  list(
    "Level-Level" = m1_1, 
    "Level-Log"   = m2_1, 
    "Quadratic"   = m3_1
  ),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  title = "Table 1: Regression Models of Corruption on GDP per Capita",
  notes = "Note: Robust standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01"
)

regression_table

# I would porbably choose the Level-Log model (and, thus: m2_1). 
# Although the Quadratic model (m3_1) has a slightly higher R-squared 
# (0.710 vs 0.603), the Log model is theoretically more robust as it 
# guarantees diminishing returns without the risk of an artificial 
# downward turn at extreme wealth levels. Furthermore, its residuals 
# showed the best balance of linearity and homoskedasticity.

### 2 Part 2: Take-Home (Wealth and Infant Mortality)

raw_2 <- read_dta("data/infantmortality.dta")

df2 <- raw_2
summary(df2)

n_countries <- nrow(df2)
print(n_countries)

# There are 101 countries in the dataset. 

p_infant <- ggplot(df2, aes(x = infant)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Infant Mortality", x = "Infant Mortality", y = "Count") +
  theme_minimal()

p_infant

p_income <- ggplot(df2, aes(x = income)) +
  geom_histogram(bins = 20, fill = "firebrick", color = "white") +
  labs(title = "Distribution of Per-Capita Income", x = "Income", y = "Count") +
  theme_minimal()

p_income

# Both variables are heavily right-skewed. The income distribution shows 
# a massive concentration of low-income nations with a very long tail 
# toward higher wealth levels. Similarly, infant mortality is skewed by 
# a group of countries with exceptionally high rates.

p_scatter <- ggplot(df2, aes(x = income, y = infant, color = region)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Infant Mortality vs. Per-Capita Income",
    x = "Income (per-capita dollars)",
    y = "Infant Mortality (per 1,000 live births)",
    color = "World Region"
  ) +
  theme_minimal()

p_scatter

# The scatter plot reveals a strong, non-linear negative relationship between 
# wealth and infant mortality. As per-capita income increases, mortality 
# rates drop precipitously at first and then level off, forming an L-shaped 
# curve. Regional clustering is evident: African nations (in red) are 
# concentrated in the high-mortality, low-income quadrant, while European 
# countries (in purple) dominate the low-mortality, high-income zone. 

p_log_log <- ggplot(df2, aes(x = log(income), y = log(infant), color = region)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  labs(
    title = "Log-Log Relationship: Infant Mortality vs. Income",
    x = "Log of Per-Capita Income",
    y = "Log of Infant Mortality",
    color = "World Region"
  ) +
  theme_minimal()

p_log_log

# The log-log relationship looks significantly more linear than the original 
# level-level plot. By transforming both variables, the extreme L-shape 
# has been replaced by a consistent downward-sloping cloud of points.

## 2.2 Comparing specifications

m1_2 <- lm(infant ~ income, data = df2)
summary(m1_2)

# In the level-level model, the coefficient for income is -0.0209. This 
# means that for every $1 increase in per-capita income, infant mortality 
# is predicted to decrease by 0.02 units. Therefore, a $1,000 increase 
# in income is associated with a predicted reduction of 20.9 deaths per 
# 1,000 live births.

m2_2 <- lm(log(infant) ~ log(income), data = df2)
summary(m2_2)

# In the log-log model, the coefficient for log(income) is -0.5118. This 
# represents an elasticity: a 1% increase in per-capita income is 
# associated with a 0.51% decrease in infant mortality. For example, 
# a 10% increase in income is associated with a 5.12% reduction in 
# mortality.

diag_m1 <- augment(m1_2)
diag_m2 <- augment(m2_2)

p1 <- ggplot(diag_m1, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Residuals vs Fitted: Level-Level (m1_2)",
       x = "Fitted Values (Predicted Infant Mortality)",
       y = "Residuals") +
  theme_minimal()

p1

p2 <- ggplot(diag_m2, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Residuals vs Fitted: Log-Log (m2_2)",
       x = "Fitted Values (Predicted log-Infant)",
       y = "Residuals") +
  theme_minimal()

p2

# The log-log specification (m2_2) has a significantly better residual 
# pattern. In the level-level model (m1_2), the residuals show a clear 
# non-linear "U" shape and extreme heteroskedasticity, with errors 
# exploding as predicted mortality increases. In contrast, the log-log 
# residuals are much more randomly dispersed around the zero line with 
# a more constant variance (homoskedasticity).

## 2.3 Multiple regression with controls

m3_2 <- lm(log(infant) ~ log(income) + region + oil, data = df2)
summary(m3_2)

# Controlling for region and oil status significantly changes the income 
# effect. The coefficient on log(income) dropped from -0.512 to -0.340. 
# This indicates that the previous model suffered from omitted variable 
# bias, overestimating the impact of wealth. Now, holding region and oil 
# status constant, a 1% increase in income is associated with a 0.34% 
# decrease in infant mortality.

# In model m3_2, Africa serves as the reference category (intercept). 
# Since all other regional coefficients (Americas, Asia, Europe) are 
# negative and statistically significant, it indicates that, holding 
# income and oil status constant, Africa has the highest predicted 
# infant mortality of all regions.

ame_income <- avg_slopes(m3_2, variables = "income")
print(ame_income)

# The Average Marginal Effect (AME) of income is -0.00159. This means that, 
# on average across all countries and holding region and oil status 
# constant, a $1 increase in per-capita income is associated with a 
# decrease of 0.00159 infant deaths per 1,000 live births.

## 2.4 Interaction: oil status and income

m4_2 <- lm(log(infant) ~ log(income) * oil + region, data = df2)
summary(m4_2)

ame_by_oil <- avg_slopes(m4_2, variables = "income", by = "oil")
print(ame_by_oil)

# The relationship between income and infant mortality differs significantly 
# for oil-exporting countries. For non-oil countries, income has a strong 
# protective effect (AME = -0.00209). However, for oil-exporting countries, 
# the effect is actually positive (AME = 0.00111), suggesting that higher 
# income in these specific economies does not correlate with better health 
# outcomes. This disparity might be explained by high income inequality in these
# countries, where oil wealth is concentrated in elite sectors rather than 
# invested in public health infrastructure.

p_slopes_oil <- plot_slopes(m4_2, variables = "income", condition = "oil") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Marginal Effect of Income on Infant Mortality",
    subtitle = "Contrasting Oil-Exporting vs. Non-Oil Countries",
    x = "Oil Exporting Status",
    y = "Marginal Effect of Income (dY/dX)",
    caption = "Note: A negative effect means income reduces mortality."
  ) +
  theme_minimal()

p_slopes_oil

ggsave("assignment4/slopes_income_oil.png", 
       plot = p_slopes_oil, width = 8, height = 6, dpi = 300)

## 2.5 Predicted values for specific scenarios

escenarios <- datagrid(
  income = c(1000, 20000, 10000),
  region = c("Africa", "Europe", "Americas"),
  oil = c("no", "no", "yes"),
  model = m3_2
)

pred_m3 <- predictions(m3_2, newdata = escenarios)

pred_m3$infant_pred <- exp(pred_m3$estimate)

print(pred_m3[, c("income", "region", "oil", "estimate", "infant_pred")])

#???

## 2.6 Publication-quality visualization

p_final <- plot_predictions(m3_2, condition = c("income", "region")) +
  # Transformamos el eje Y de logaritmos a escala real para que sea legible
  scale_y_continuous(trans = "exp", breaks = c(5, 10, 20, 50, 100, 150)) +
  # Usamos una escala logarítmica en X para ver mejor la distribución del ingreso
  scale_x_log10(labels = scales::dollar) +
  # Personalización estética
  labs(
    title = "Impact of Income on Infant Mortality by Region",
    subtitle = "Predicted mortality rates per 1,000 live births (Log-Log Model)",
    x = "GDP per Capita (USD, log scale)",
    y = "Predicted Infant Mortality",
    color = "Region",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_final)

ggsave("assignment4/infant_mortality_predictions.png", 
       plot = p_final, width = 10, height = 7, dpi = 300)

# This plot demonstrates that while increased national wealth (GDP per capita) 
# is universally associated with lower infant mortality, the relationship is 
# non-linear; the greatest health gains occur when moving from very low to 
# moderate income levels. Geography plays a crucial role, as evidenced by the 
# distinct vertical "stacking" of regions: at any given income level, a 
# country in Africa is predicted to have significantly higher mortality than 
# one in Europe, suggesting that regional factors matter as much as money. 
# However, this analysis has key limitations, such as omitted variable 
# bias—factors like female education or healthcare spending, which are not 
# included but likely influence both wealth and health. 

## 2.7 Diagnostics and robust inference

diag_m3 <- augment(m3_2)

p_resid_m3 <- ggplot(diag_m3, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "orange", se = FALSE) +
  labs(
    title = "Residuals vs Fitted: Log-Log Model with Controls (m3_2)",
    x = "Fitted Values (Predicted log-infant)",
    y = "Residuals",
    caption = "The orange line shows the trend of the residuals."
  ) +
  theme_minimal()

print(p_resid_m3)

# The plot for the log-log model (m3_2) shows a significant improvement 
# over the initial linear model (m1_2). While the orange LOESS line 
# exhibits a slight "downward dip" at higher fitted values—likely driven 
# by a few high-income outliers with exceptionally low mortality—the 
# overall spread of residuals is much more uniform (homoscedastic).

model_list <- list(
  "Level-Level" = m1_2, 
  "Log-Log"     = m2_2,
  "Controls"    = m3_2, 
  "Interaction" = m4_2
)

modelsummary(
  model_list,
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  title = "Comparison of Infant Mortality Models",
  notes = "Standard errors are HC3 robust."
)

model_comparison <- list(
  "Default SEs" = m3_2,
  "Robust SEs"  = m3_2
)

modelsummary(
  model_comparison,
  vcov = list(NULL, "robust"), # NULL uses default, "robust" uses HC3
  stars = TRUE,
  gof_map = c("nobs", "r.squared")
)

# Comparing the two columns, we see that the coefficient estimates 
# remain identical (e.g., log(income) is -0.340 in both), but the 
# standard errors (in parentheses) change. For most variables, the 
# robust SEs are larger: log(income) increases from 0.067 to 0.078, 
# and the SE for regionAsia increases from 0.158 to 0.207.
