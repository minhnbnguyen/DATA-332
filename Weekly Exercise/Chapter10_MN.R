# Minh Nguyen
# Chapter 10

# Exercise 1
abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}
abs_sets <- function(vec){
  negs <- vec < 0
  vec[negs] <- vec[negs]* -1
  vec
}
long <- rep(c(-1,1),5000000)
system.time(abs(long))

# Exercise 2
vec <- c(1, -2, 3, -4, 5, -6, 7, -8, 9, -10)
vec <- 0
vec[vec<0]
vec[vec<0] * -1
change_symbols <- function(vec){
  for (i in 1:length(vec)){
    if (vec[i] <- "DD") {
      vec[i] <- "joker"
    } else if (vec[i] == "C") {
      vec[i] <- "king"
    } else if (vec[i] == "B") {
      vec[i] <- "queen"
    } else if (vec[i] == "BB") {
      vec[i] <- "jack"
    } else if (vec[i] == "BBB") {
      vec[i] <- "ten"
    } else {
      vec[i] <- "nine"
    }
  }
}
vec <- c("DD","C","7","B","BB","BBB","0")
change_symbols(vec)
many <- rep(vec, 1000000)
system.time(change_symbols(many))

change_vec <- function (vec) {
  vec[vec == "DD"] <- "joker"
  vec[vec == "C"] <- "ace"
  vec[vec == "7"] <- "king"
  vec[vec == "B"] <- "queen"
  vec[vec == "BB"] <- "jack"
  vec[vec == "BBB"] <- "ten"
  vec[vec == "0"] <- "nine"
  vec
}
system.time(change_vec(many))

# Exercise 3
score_many <- function(symbols) {
  diamonds <- rowSums(symbols == "DD")
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  
  same <- symbols[, 1] == symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB"=25,"B"=10,"C"=10,"0"=0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  bars <- symbols == "B" | symbols ==  "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  two_wilds <- diamonds == 2
  
  one <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] &
    symbols[, 2] != symbols[, 3]
  
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  one_wild <- diamonds == 1
  182 |
    Chapter 10: Speed
  
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  unname(prize * 2^diamonds)
}
system.time(play_many(10000000))