library(ggplot2)
gapminder <- read.csv("data/gapminder.csv")

head(gapminder)
str(gapminder)

countries <- c("Spain", "France", "Germany", "United Kingdom", "Greece", "Japan")
df <- gapminder[gapminder$country %in% countries, ]

#Life expectancy

ggplot(df, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Life expectancy",
       title = "Life expectancy over time") +
  theme_minimal()
ggsave("ass1_plot_1.png", width = 7, height = 5)

#GDP per capita

ggplot(df, aes(x = year, y = gdpPercap, color = country)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "GDP per capita",
       title = "GDP/cap over time") +
  theme_minimal()
ggsave("ass1_plot_2.png", width = 7, height = 5)
