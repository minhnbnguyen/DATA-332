# Minh Nguyen

# Chapter 6
# Exercise 1: Will R be able to find deck and return an answer when I call the new version of deal?
# Answer: Yes, deal will still work the same as before


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
# Exercise 2
deal <- function() {
  card <- deck[-1, ]
  assign("deck", deck[-1, ], envir = globalenv())
}

# Exercise 3
shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

# Chapter 7
# Exercise 1
symbols <- c("A", "A", "A")
symbols[1] == symbols[2] & symbols[2] == symbols[3]

# Exercise 2
# Method 1
symbols[1] == "B" | symbols[1] == "BB" | symbols[1] == "BBB" &
  symbols[2] == "B" | symbols[2] == "BB" | symbols[2] == "BBB" &
  symbols[3] == "B" | symbols[3] == "BB" | symbols[3] == "BBB"
# Method 2
all(symbols %in% c("B", "BB", "BBB"))

# Exercise 3
symbols <- c("C", "DD", "C")
symbols == "C"

# Exercise 4
# identify case
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B","BB","BBB")

# get prize
if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
               "B" = 10, "C" = 10, "0" = 0)
  prize <- uname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  cherries <- sum(symbols == "C")
  prize <- c(0, 2, 5)[cherries + 1]
}
# adjust for diamonds
diamonds <- sum(symbols == "DD")
prize * 2 ^ diamonds
