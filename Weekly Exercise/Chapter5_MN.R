# Minh Nguyen
# Chapter 5 

cards <- data.frame(face = c("ace","two","six"),
                    suit = c("clubs","clubs","clubs"), value = c(1, 2, 3),
                    stringsAsFactors = FALSE)

deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace",
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five",
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine",
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),
  suit = c("spades", "spades", "spades", "spades", "spades", "spades",
           "spades", "spades", "spades", "spades", "spades", "spades", "spades",
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs",
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts"),
  value=c(13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8, 7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11, 10,9,8,7,6,5,4,3,2,1)
)
deck2 <- deck
# exercise 1
sum(deck2$face == "ace")

# exercise 2
deck4 <- deck
deck4$value <- 0

deck4$suit == "hearts"

deck4$value[deck4$suit == "hearts"]

deck4$value[deck4$suit == "hearts"] <- 1

deck4$value[deck4$suit == "hearts"]

# Exercise 3
w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

# Is w positive
w > 0
# Is x greater than 10 and less than 20?
10 < x & x < 20
# Is object y the word February?
y == "February"
# Is every value in z a day of the week?
all(z %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
             "Saturday", "Sunday"))
