#The R User Interface
1 + 1
100:130
2 * 3
4 - 1
6 / (4 - 1)
# Exercise 1 pg.6
1 + 2
3 * 6
6 - 6
6 / 3
#Objects
a <- 1
a + 2
die <- 1:6
die
my_number <- 1
my_number
my_number <- 999
my_number
ls()
die - 1
die / 2
die * die
1:2
1:4
die
die + 1:2
die + 1:4
die %*% die
die %o% die
#Functions
round(3.1415)
factorial(3)
mean(1:6)
mean(die)
round(mean(die))
sample (x = 1:4, size = 2)
args(round)
round(3.1415, digits = 2)
sample(die, 1)
sample (size = 1, x = die)
#sample with replacement
sample(die, size = 2)
sample(x = die, size = 2, replace = TRUE)
dice <- sample(die, size = 2, replace = TRUE)
dice
#Write your own functions
die <- 1:6
dice <- sample(die, size = 2, replace = TRUE)
sum(dice)
#Function Constructor
roll <- function() {
  die <- 1:6
  dice <- sample(x = die, size = 2, replace = TRUE)
  sum(dice)
}
roll()
#Arguments
roll2 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2(bones = 1:4)
roll2(bones = 1:6)
roll2(1:20)
roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2()
