# Chap 2
install.packages("ggplot2")
library("ggplot2")

# exercise 1
x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
qplot(x3, binwidth = 1)

#exercise 2
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE,
                 prop = c(1/8,1/8,1/8,1/8,1/8,3/8))
  sum(dice)
}