##### Assignment 8: Spatial Data II -------------------------------------------

#### 1 Part 1: In-Class (Detecting Spatial Autocorrelation) ------------------

library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)

### 1.1 Setup and OLS baseline ----------------------------------------------

data(world)

## a) 

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ] 
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

# Number of remaining observations: 161 countries.

# We log-transform GDP per capita because economic data is typically 
# highly right-skewed, with a few extremely wealthy nations creating 
# a long tail that can disproportionately influence linear models. 
# Applying a log transformation compresses the upper end of the 
# distribution and expands the lower end, resulting in a more normal 
# (symmetric) distribution that satisfies the homoscedasticity 
# assumption of linear regression. This allows us to interpret 
# coefficients as percentage changes (elasticities) rather than 
# absolute dollar units, which is more theoretically sound for 
# cross-national economic comparisons.

## b)

ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

# Estimated Coefficient (log_gdp): 5.54.

# Since the independent variable is log-transformed, this represents a 
# semi-elasticity. Specifically, a 1% increase in GDP per capita is associated 
# with an increase of approximately 0.055 years in life expectancy. 
# The coefficient is highly significant (p < 2e-16), as indicated by a t-value
# of 17.024, exceeding the standard threshold for rejecting the null hypothesis.
# The R-squared is 0.6472, which indicates that approximately 64.7% of the 
# global variation in life expectancy is explained by the log of GDP 
# per capita alone, suggesting a very strong positive correlation.

## c)

world$ols_resid = residuals(ols_fit)

ols_residuals_map <- ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")

ols_residuals_map

ggsave("plots/ols_residuals_map.pdf", width = 10, height = 5)


# The map reveals strong spatial clustering of residuals, suggesting 
# that the OLS model suffers from spatial autocorrelation. We see a 
# prominent cluster of negative residuals (dark blue) across Sub-Saharan 
# Africa, meaning these countries have a significantly lower life 
# expectancy than their GDP levels would predict—likely due to systemic 
# healthcare challenges or historical disease burdens like HIV/AIDS. 

# Conversely, positive residuals (red/pink) are visible in parts of 
# Latin America, North Africa, and Southeast Asia. In these regions, 
# people live longer than the model predicts based strictly on 
# economic output, suggesting that factors like diet, social safety 
# nets, or efficient public health systems are providing a "longevity 
# bonus" that GDP alone cannot explain.

### 1.2 Spatial weights matrix ----------------------------------------------

## a)

nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

# Number of countries with zero neighbors: 16.

# In a contiguity-based weights matrix (Queen or Rook), countries have 
# no neighbors if they do not share a land border or a vertex with any 
# other polygon in the dataset. These "islands" typically include actual 
# island nations (like Madagascar, New Zealand, or Japan) or countries 
# where neighboring landmasses were removed during the data cleaning 
# process (missing GDP/LifeExp data). 

## b)

moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

# There is highly statistically significant positive spatial autocorrelation 
# in the residuals (p < 0.001). This indicates that the OLS model violates 
# the fundamental assumption of Independence of Errors (i.e., residuals 
# should be independent and identically distributed, or i.i.d.). 

# In substantive terms, the error for one country is correlated with the 
# error of its neighbors. This implies that OLS is missing a "spatial 
# process" which means our standard errors are likely underestimated
# and our t-statistics are artificially inflated.

### 1.3 Lagrange Multiplier tests -------------------------------------------

## a)

lm_tests = lm.LMtests(ols_fit, listw = listw,
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                      zero.policy = TRUE)

summary(lm_tests)

# LMerr Statistic: 52.17 (p-value: 5.089e-13 ***).
# LMlag Statistic: 0.06 (p-value: 0.8040).

# Only the LMerr test is statistically significant. 

#These tests are used to diagnose the specific nature of spatial dependence: 
 
# 1. LMerr (Spatial Error): Tests the null hypothesis that the spatial 
#    autoregressive coefficient in the error term is zero (lambda = 0). 
#    A significant result suggests that unobserved factors (like regional 
#    shocks or shared environments) are spatially clustered in the residuals.

# 2. LMlag (Spatial Lag): Tests the null hypothesis that the spatial 
#    autoregressive coefficient on the dependent variable is zero (rho = 0). 
#    The non-significant result suggests that life expectancy in one 
#    country is not directly "caused" by the life expectancy of its 
#    neighbors (no direct spillover effect).

## b)

# RLMerr Statistic: 54.31 (p-value: 1.716e-13 ***)
# RLMlag Statistic: 2.20 (p-value: 0.1383)

# The RLMerr test is highly significant (***), while the RLMlag test is 
# not significant (p > 0.05). Following the LM decision rule, where we 
# choose the model associated with the most significant robust test, I 
# would choose the Spatial Error Model (SEM). 

# The standard LM tests already pointed toward SEM, but the 
# robust versions confirm that even after controlling for potential 
# lag effects, the spatial dependence is clearly located in the error 
# term. This implies that the spatial clustering we see is due to 
# unobserved regional factors (omitted variables) rather than a 
# direct spillover effect where one country's life expectancy 
# inherently boosts its neighbor's.

### 1.4 Spatial Error Model (SEM) -------------------------------------------

## a)

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)

summary(sem_fit)

# Estimated Coefficient (log_gdp) in SEM: 3.958.
# Estimated Coefficient (log_gdp) in OLS: 5.540.

# The coefficient on log_gdp has decreased significantly 
# (from ~5.5 to ~4.0). This suggests that in the original OLS model, 
# the effect of income was likely "overestimated" because it was 
# capturing some of the regional effects that are now being handled 
# by the spatial error term.
 
# Lambda (λ) Parameter: 0.7625.
# Lambda p-value: < 2.22e-16.

## b)

# In the SEM, lambda (λ) represents the intensity of spatial 
# dependence in the error term (u = λWu + ε). Since our λ is 0.76 and highly 
# significant, it tells us that unmeasured factors affecting life expectancy 
# are strongly clustered across space. Substantively, this means that a "shock" 
# or unobserved characteristic in one country is likely to be shared by its 
# geographical neighbors, creating regional pockets of longevity or 
# vulnerability that exist independently of national wealth levels.

## c)

world$sem_resid = residuals(sem_fit) 
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# In the OLS model, the Moran's I was 0.437 (p < 0.001), indicating severe 
# spatial autocorrelation. In the SEM, the Moran's I has dropped to -0.086 
# with a p-value of 0.8843. Thus, he spatial autocorrelation has been 
# effectively removed. A p-value of 0.88 means we fail to reject the null 
# hypothesis of spatial randomness. By explicitly modeling the spatial
# structure of the error term (lambda), the SEM has "soaked up" the regional 
# clustering, leaving behind residuals that are essentially white 
# noise. This confirms that the SEM was the correct specification 
# for handling the geographic dependencies in this dataset.


### 1.5 Distance-based weights: an alternative neighborhood -----------------

## a)

coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)

# Number of countries with zero neighbors: 114

# The resulting number is significantly higher than the 16 isolated 
# countries found in the queen contiguity neighborhood. While the queen 
# approach only isolates islands, the 300 km centroid threshold isolates 
# any country whose geometric center is far from the center of its 
# neighbor. This frequently occurs with large nations (where centroids are 
# thousands of kilometers apart despite sharing a border) or sparsely 
# populated regions.

## b)

listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)

# Lambda (λ): 0.425 (p-value: 0.00013)
# log_gdp Coefficient: 5.469

# The results are substantially different from the contiguity 
# model. In this distance-based version, the log_gdp coefficient (5.47) 
# is very close to the original OLS estimate (5.54), whereas the 
# contiguity SEM reduced it to 3.96. Additionally, Lambda is much lower 
# here (0.425 vs 0.763), and the AIC is much higher (953.3 vs 894.7).

# This demonstrates that spatial models are highly sensitive 
# to the definition of the "neighborhood". Because the 300km threshold 
# isolated 114 countries, the model treated most of the world as 
# independent observations, failing to capture the broad regional clusters 
# that the queen contiguity picked up. 

## c)

world$sem_dist_resid = residuals(sem_dist)
moran.test(world$sem_dist_resid, listw = listw_dist, zero.policy = TRUE)

# With a p-value of 0.4491, we fail to reject the null hypothesis of 
# spatial randomness. This indicates that the distance-based SEM has 
# technically removed the spatial autocorrelation from the 
# residuals relative to the 300km weights matrix.

# While both models succeeded in removing residual autocorrelation, the 
# results are qualitatively different. In 1.4c, the queen contiguity model 
# addressed a massive amount of real-world global clustering (original 
# Moran's I = 0.437). In contrast, the distance-based model (listw_dist) is 
# so sparse (isolating 114 countries) that it "cleaned" the residuals simply 
# by ignoring most of the world's spatial relationships. As evidenced by 
# the significantly higher AIC (953.3 vs 894.7), the distance-based model 
# is a much poorer fit for the data than the contiguity-based SEM.

#### 2 Part 2: Take-Home (Spatial Lag Model and Model Comparison) ------------


### 2.1 Spatial Lag Model (SLM) ---------------------------------------------

slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world, 
                   listw = listw, zero.policy = TRUE)

summary(slm_fit)

## a)

# With a p-value of 0.805, p^ (rho) is not statistically significant. 
# This indicates that there is no evidence of a "spatial lag" effect, 
# meaning the life expectancy of a country is not directly influenced by 
# the life expectancy levels of its neighbors in this specific model.

## b)

# The p^ (rho) parameter represents the strength of the spatial 
# dependence between the dependent variable (life expectancy) of a 
# country and the weighted average of its neighbors' life expectancy. 
# If ρ^ were positive and significant, it would indicate a "contagion" 
# or diffusion effect, where a country’s life expectancy increases 
# simply because its neighbors' life expectancy is high. In this 
# specific model, however, ρ is effectively zero and non-significant, 
# suggesting that such direct spatial spillovers of health outcomes 
# are not present in this dataset.

## c)

# The log_gdp coefficient in the SLM is not the marginal effect because 
# of the spatial multiplier captured by the term (I - ρW)^-1. In this 
# model, an increase in one country's GDP doesn't just raise its own 
# life expectancy (the direct effect), but also feeds into its
# neighbors' outcomes via p^, which in turn feeds back into the original 
# country and out to the neighbors' neighbors (indirect effects). 

# Therefore, the total marginal effect is a combination of these 
# feedback loops. The equilibrium matrix (I - ρW)^-1 implies that a 
# change in any x_i propagates through the entire network, meaning 
# a policy change in one nation can theoretically shift the 
# equilibrium life expectancy of an entire continent.

### 2.2 Direct and Indirect Effects -----------------------------------------

## a)

set.seed(123)
slm_impacts = impacts(slm_fit, listw = listw, R = 500)
summary(slm_impacts, zstats = TRUE, short = TRUE)

# Direct Effect: 5.5482
# Indirect Effect: -0.0235
# Total Effect: 5.5247

# The Direct Effect (5.5482) is almost identical to the raw log_gdp 
# coefficient from the SLM output (5.5482) and very close to the 
# OLS coefficient (5.540). 

# In a model with a high, significant rho, the direct effect 
# would be larger than the OLS coefficient because it includes "feedback 
# loops" (Country A affects B, which then feeds back into A). However, 
# because the rho is nearly zero and statistically insignificant in this
# case (p = 0.815), there is effectively no spatial diffusion. This is why 
# the indirect effect is near zero and non-significant, meaning the 
# total impact of GDP is limited strictly to the country where the 
# income is located.

## b)

# The indirect effect represents the spatial "spillover" or the average 
# impact that a one-unit change in a specific country's log GDP has on 
# the life expectancy of all other countries in the network. A positive 
# and significant indirect effect would imply that neighbors also experience 
# a boost in life expectancy when a specific country manages to increase its
# individual GDP log. In this model, however, the indirect effect is 
# negligible and non-significant, suggesting that wealth-driven health 
# improvements in one country do not provide meaningful benefits to the life 
# expectancy of surrounding nations.

## c)

# In our specific results, the Total Effect is actually smaller than the 
# Direct Effect because our estimated rho is negative (-0.0042). 
# Mathematically, a negative rho creates a negative indirect effect 
# (-0.0235), which subtracts from the direct impact.

# While a positive rho (where Total > Direct) is the expected feature 
# of a spatial diffusion process, our results show no meaningful 
# spillover. Since the p-value for rho is 0.815, the fact that the 
# total effect is smaller is likely due to statistical noise rather 
# than a substantive "competitive" spatial process. As rho approaches 0, 
# these differences vanish and the Total Effect simply converges to 
# the Direct Effect.

### 2.3 Model Comparison ----------------------------------------------------

## a)

aic_ols = AIC(ols_fit)
aic_sem = AIC(sem_fit)
aic_slm = AIC(slm_fit)

print(paste("OLS AIC:", aic_ols))
print(paste("SEM AIC:", aic_sem))
print(paste("SLM AIC:", aic_slm))

# The SEM has the lowest AIC by a wide margin (approx. 71 units lower 
# than OLS). This result perfectly agrees with the LM-tests in 1.3b, 
# which indicated that the Spatial Error Model was the most appropriate 
# specification for this data. The SLM actually has a worse (higher) 
# AIC than OLS because it adds complexity (the rho parameter) without 
# improving the model's explanatory power.

## b)

# Our analysis began with a standard OLS regression, where the residuals 
# exhibited strong and highly significant spatial autocorrelation 
# (Moran’s I ≈ 0.44, p < 0.001), indicating that the model failed to 
# account for geographic clustering. 

# Based on the Lagrange Multiplier (LM) tests, the Spatial Error Model (SEM) 
# was selected as the superior specification because the "LMerr" and "RLMerr" 
# statistics were highly significant while the Lag diagnostics were not, 
# suggesting the spatial dependence resides in omitted variables rather 
# than direct diffusion.

# The coefficient for log_gdp varied across models: it was highest in 
# the OLS (5.54) and SLM (5.55), but dropped significantly in the 
# SEM (3.96), revealing that OLS likely overestimates the impact of 
# wealth by conflating it with regional characteristics. 

# The SLM produced a non-significant rho (ρ), implying that there are no 
# meaningful "spillovers" where health outcomes in one nation directly raise 
# life expectancy in another. 

# Finally, a key limitation of using queen contiguity weights is that it 
# ignores the role of physical distance and isolation; it treats all 
# neighbors as equally influential regardless of their size and completely 
# excludes island nations from the spatial process unless manual links 
# are added.

### 2.4 Extension: Spatial Durbin Model (optional/bonus) --------------------

## a)

sdm_fit = lagsarlm(lifeExp ~ log_gdp, data = world, 
                   listw = listw, Durbin = TRUE, zero.policy = TRUE)

summary(sdm_fit)

# lag.log_gdp Coefficient: -3.827
# p-value: 3.41e-12

# lag.log_gdp is highly statistically significant (p < 0.001), which 
# indicates that the GDP of neighboring countries is a strong predictor 
# of a country’s life expectancy, even after controlling for the country's 
# own GDP. 

# Interestingly, the coefficient is negative (-3.83). This suggests 
# that, holding a country's own wealth constant, being surrounded by 
# wealthier neighbors is associated with lower life expectancy in this 
# specific model. This might capture regional inequalities or "brain 
# drain" effects where wealthy neighbors attract resources or 
# healthcare professionals away from surrounding nations, or it may 
# simply be a corrective adjustment for the strong positive Rho (0.48).

## b)

# Calculating SDM AIC manually since summary() reports NA:
aic_sdm = 2*5 - 2*(-464.9167) 
print(aic_sdm)

# While the SDM (939.83) is a significant improvement 
# over the OLS and the simple SLM, it still does NOT beat the SEM (894.70). 
# The SEM remains the best-fitting model by a margin of about 45 AIC units. 
# Therefore, while the SDM revealed interesting dynamics (like the 
# significant neighbor-wealth effect), the added complexity is not 
# strictly justified if our goal is simply the best statistical fit.



