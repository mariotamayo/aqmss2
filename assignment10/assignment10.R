library(haven)
library(modelsummary)
library(ggplot2)

raw <- read_dta("data/corruption.dta")
df <- raw

### 1 Task: Build a Mini Research Project -----------------------------------

## 1.1 What to build -------------------------------------------------------

# Model 1: CPI on GDP only
m1 <- lm(ti_cpi ~ undp_gdp, data = df)

# Model 2: CPI on log(GDP)
m2 <- lm(ti_cpi ~ log(undp_gdp), data = df)

modelsummary(
  list("Model 1" = m1, "Model 2" = m2),
  output = "assignment10/tab/regression_table.tex",
  title = "Corruption Perceptions Index Regressions",
  coef_rename = c(undp_gdp = "GDP/Capita (PPP)", "log(undp_gdp)" = "log GDP/Capita (PPP)"),
  stars = TRUE
)

plot1 <- ggplot(df, aes(x = undp_gdp, y = ti_cpi, label = ccodealp)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  geom_text(size = 2.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    x = "GDP per Capita, PPP (constant USD)",
    y = "Corruption Perceptions Index",
    title = "GDP per Capita and Corruption"
  ) +
  theme_bw()

ggsave("assignment10/img/scatter_gdp_cpi.pdf", plot = plot1, width = 8, height = 6)
