# Exercise 1
#1 is a number. "1" and "one" are strings.

#Exercise 2
hand <- c("ace", "king","queen","jack","ten")
hand

# Exercise 3
# first way
hand1 <- c("ace","king","queen","jack","ten","spades","spades","spades","spades","spades")
matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5,2)

#second way
hand2 <- c("ace","spades","king","spades","queen","spades","jack","spades","ten","spades")
matrix(hand2, nrow = 5, byrow = TRUE)
matrix(hand2, ncol = 2, byrow = TRUE)

#Exercise 4
card <- c("ace","hearts",1)
card
