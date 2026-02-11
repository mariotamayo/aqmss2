# Assignment 1
# AQMSS II, Spring 2026
# [Your Name]
# This file will contain my solutions for Assignment 1
print("Hello, Git!")

install.packages("gapminder")
library(gapminder)
write.csv(gapminder, "data/gapminder.csv", row.names = FALSE)