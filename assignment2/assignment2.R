### PART 1: QoG Dataset

library(dplyr)
library(tidyr) 
library(ggplot2)
library(broom)
library(modelsummary)

## 1.1  Setup and data preparation

qog <- read.csv("assignment2/data/qog_std_cs.csv")

df <- qog %>%
  select(
    country     = cname,       
    epi         = epi_epi,     
    women_parl  = wdi_wip,     
    gov_eff     = wbgi_gee,    
    green_seats = cpds_lg     
  )

df_clean <- df %>%
  drop_na()  

n_countries <- nrow(df_clean)
n_countries 

#36 countries remain in the dataset

summary(df_clean)

## 1.2 Exploratory visualization

ggplot(df_clean, aes(x = women_parl, y = epi)) +
  geom_point(color = "black", size = 2) +        
  geom_smooth(method = "lm", se = TRUE, color = "red") +  
  labs(
    x = "% Women in Parliament",
    y = "EPI (Environmental Performance Index)",
    title = "Scatter plot of Women in Parliament vs. EPI"
  ) +
  theme_minimal()

# The scatter plot suggests a positive relationship between the percentage of 
# women in parliament and Environmental Performance Index (EPI) scores. As women’s 
# representation in parliament increases, EPI values generally tend to rise. 
# However, the points are fairly spread out, indicating that the relationship is 
# moderate rather than strong.

ggsave("plots/epi_vs_women_parl.png", width = 7, height = 5)

## 1.3 Bivariate regression

lm1 <- lm(epi ~ women_parl, data = df_clean)
tidy(lm1)

quantile(df_clean$women_parl, probs = c(0.25, 0.75), na.rm = TRUE)

# The coefficient (0.194) means that a 1-percentage-point increase in women’s 
# representation in parliament is associated with about a 0.19 point increase in EPI, 
# on average.A country at the 75th percentile has about 15.4 percentage points more 
# women in parliament than one at the 25th percentile, which according to the model 
# is associated with an EPI roughly 3 points higher.

## 1.4 Multiple regression

lm2 <- lm(epi ~ women_parl + gov_eff, data = df_clean)
tidy(lm2)

modelsummary(list(lm1,lm2))

# The coefficient on women in parliament decreases from about 0.19 in the bivariate 
# model to about 0.08 in the multiple regression and is no longer statistically 
# significant. This downward change suggests that part of the positive association 
# between women’s representation and EPI in the bivariate model is explained by 
# government effectiveness, indicating omitted variable bias in the bivariate model.

## 1.5 Demonstrating OVB

# From the results above:
# · β~1 (bivariate coefficient on women_parl) = 0.194
# · β^1 (multiple regression coefficient on women_parl) = 0.075
# · β^2 (multiple regression coefficient on gov_eff) = 4.22

aux <- lm(gov_eff ~ women_parl, data = df_clean)
tidy(aux)

# δ~ = 0.0282

# β~1 = β^1 + β^2 · δ~
# β~1 = 0.075 + 4.22 * 0.0282
# β~1 = 0.194

# This confirms the omitted variable bias explanation: women_parl appears 
# more important in the bivariate regression because it is positively correlated 
# with gov_eff. Once we control for gov_eff, the coefficient drops from 0.194 to 0.075.
# As such, the initial positive relationship between women in parliament and epi was 
# partly explained by better government effectiveness in those countries.

## 1.6 Robust standard errors

modelsummary(lm2,
             coef_map = c("women_parl" = "Women in Parliament",
                          "gov_eff" = "Government Effectiveness"),
             title = "Multiple Regression with default standard errors")

modelsummary(lm2, 
             vcov = "robust",
             coef_map = c("women_parl" = "Women in Parliament",
                          "gov_eff" = "Government Effectiveness"),
             title = "Multiple Regression with robust standard errors")

# The robust standard errors are somewhat larger than the classical standard errorsm
# rising from 0.092 to 0.132 for "Women in Parliament" and from 1.685 to 1.864 for 
# "Government Effectiveness". However, these changes are not large enough to affect 
# inference. Women in Parliament remains statistically insignificant in both cases, 
# while Government Effectiveness remains statistically significant.

## 1.7 Presenting results

modelsummary(
  list(lm1, lm2),
  vcov = "robust",
  coef_map = c(
    "women_parl" = "Women in Parliament",
    "gov_eff" = "Government Effectiveness"
  ),
  title = "Comparison of Bivariate vs Multiple Regression Models (Robust SEs)",
  stars = TRUE
)

coef_plot <- modelplot(list("Bivariate" = lm1, "Multiple" = lm2),
          vcov = "robust")

coef_plot

ggsave(
  filename = "coefficient_plot_bivariate_multiple.png",
  plot = coef_plot,
  width = 8,
  height = 5)

### PART 2: STAR Dataset

## 2.1 Data preparation

star <- read.csv("assignment2/data/star.csv")

df2 <- star %>%
  mutate(
    classtype_factor = factor(
      classtype,
      levels = c(1, 2, 3),
      labels = c("Small", "Regular", "Regular+Aide")
    )
  )

df2 %>% count(classtype_factor)

df2 <- df2 %>%
  mutate(
    race_factor = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")
    )
  )

df2 %>% count(race_factor)

df2 <- df2 %>%
  mutate(
    small = if_else(classtype_factor == "Small", 1, 0)
  )

df2 %>% count(small)

df2 %>%
  summarise(
    n_obs = n(),                          
    g4reading_nonmiss = sum(!is.na(g4reading)),  
    g4math_nonmiss    = sum(!is.na(g4math))      
  )

## 2.2 Comparing groups

df2 %>%
  group_by(classtype_factor) %>%
  summarise(
    mean_g4reading = mean(g4reading, na.rm = TRUE),
    n = n()  
  ) 

# Students in "small" classes have the highest mean 4th grade reading score, 
# albait just slightly: 723 (small), vs. 721 (regular+aide) and 720 (regular.)

lm_reading_small <- lm(g4reading ~ small, data = df2)
tidy(lm_reading_small)

# Students in small classes have slightly higher 4th grade reading scores 
# (+3.10 points) compared to students in regular or regular+aide classes. 
# However, this difference is not statistically significant, so we cannot 
# confidently conclude that small classes cause higher scores on its own.

# From part (a), the mean 4th grade reading score is 723 for students in small 
# classes and 720 for students in regular/regular+aide classes. The difference 
# in means is therefore: β^1 = 723 − 720 ≈ 3.10; which matches (up to rounding) 
# the difference in mean reading scores between students in small classes and 
# those in non-small classes.

lm_math_small <- lm(g4math ~ small, data = df2)
tidy(lm_math_small)

# The pattern is similar for 4th grade math as it was for reading: students in 
# small classes have slightly higher average scores compared to students in regular 
# or regular+aide classes, but the difference is smaller for math (0.59 points). 
# That said, in both cases the differences are not statistically significant.

## 2.3 Adding controls

lm_reading_controls <- lm(g4reading ~ small + race_factor + yearssmall, data = df2)
tidy(lm_reading_controls)

# In the bivariate regression, the coefficient on small was +3.10, indicating that 
# students in small classes scored about 3 points higher in 4th grade reading on average.
# After adding controls for race and years spent in a small class, the coefficient on 
# small changes to −4.00 and remains statistically insignificant.

# Overall, the lack of a robust or stable effect of small after adding controls supports 
# the view that the randomization was well-done. Students assigned to small versus 
# non-small classes were broadly comparable, and the estimated effect of class size 
# is not confounded by race or prior exposure to small classes.

# The coefficient on yearssmall (~2.17) captures the effect of additional exposure 
# to small classes, holding current small-class assignment and race constant. 
# Specifically, it implies that each extra year a student spent in a small class 
# is associated with an increase of roughly 2.2 points in 4th grade reading scores. 

## 2.4 Interactions

lm_reading_race <- lm(g4reading ~ small * race_factor + yearssmall, data = df2)
tidy(lm_reading_race)

# For white students, being assigned to a small class is associated with a 5.3-point 
# decrease in 4th grade reading scores, holding race and years spent in small classes 
# constant. This effect is not statistically significant.

# For black students, the effect of being in a small class is the sum of the main 
# effect of "small" and the interaction term "small:race_factorBlack" (-5.32 + 6.97 = 1.65).
# This implies that, for black students, being in a small class is associated with 
# an increase of about 1.7 points in 4th grade reading scores, relative to black 
# students in non-small classes. This effect is also not statistically significant.

# While the estimated effect of small classes differs in sign and magnitude between 
# white and black students, there is no strong statistical evidence that small classes 
# have a meaningful effect on reading scores for either group in this specification.

## 2.5 Presenting results

reading_models <- list(
  "Bivariate"   = lm_reading_small,
  "Controls"    = lm_reading_controls,
  "Interaction" = lm_reading_race
)

modelsummary(
  reading_models,
  vcov = "robust",
  coef_map = c(
    "small" = "Small class",
    "yearssmall" = "Years in small class",
    "race_factorBlack" = "Black",
    "race_factorAsian" = "Asian",
    "race_factorHispanic" = "Hispanic",
    "race_factorOther" = "Other",
    "small:race_factorBlack" = "Small × Black",
    "small:race_factorAsian" = "Small × Asian",
    "small:race_factorOther" = "Small × Other"
  ),
  stars = TRUE,
  title = "Reading Score Regressions (Robust Standard Errors)"
)

modelsummary(
  reading_models,
  vcov = "robust",
  output = "reading_models_table.html"
)


coef_plot_2 <- modelplot(
  reading_models,
  vcov = "robust",
  coef_map = c(
    "small" = "Small class",
    "yearssmall" = "Years in small class",
    "race_factorBlack" = "Black",
    "race_factorAsian" = "Asian",
    "race_factorHispanic" = "Hispanic",
    "race_factorOther" = "Other",
    "small:race_factorBlack" = "Small × Black",
    "small:race_factorAsian" = "Small × Asian",
    "small:race_factorOther" = "Small × Other"
  )
) +
  theme_minimal() +
  labs(
    title = "Coefficient Estimates for Reading Score Models",
    x = "Estimate",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12)
  )

coef_plot_2

ggsave(
  filename = "reading_models_coefficients.png",
  plot = coef_plot_2,
  width = 9,
  height = 6,
  dpi = 300
)

## 2.6 Brief discussion

# The STAR data suggest that small classes have only a modest effect on reading scores.
# In the bivariate model, small classes increase reading by about 3 points, but this
# effect disappears or becomes negative once we control for race and years in small class.
# Each additional year in a small class increases reading by about 2 points. Race differences
# are large: black students score much lower, and "other" students score higher than whites.
# This evidence is credible because students were randomly assigned, reducing confounding.
# Limitations include small sample sizes for some groups and interactions that are not 
# statistically significant. Overall, small classes have a small and uncertain effect.
