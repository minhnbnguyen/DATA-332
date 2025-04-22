# Minh Nguyen

# Chapter 8
# Exercise 1
play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
}
# Exercise 2
print.slots <- function(x,...) {
  slot_display(x)
# Exercise 3
play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

# Chapter 9
# Exercise 1
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos
# Exercise 2
get_symbols <- function() {
  wheel <- c("DD","7","BBB","BB","B","C","0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
}
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
# Exercise 3
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)
# Exercise 4
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
head(combos, 3)
# Exercise 5
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
# Challenge
score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B","BB","BBB")
  
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  prize * 2^diamonds
}
# Exercise 6
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
sum(combos$prize * combos$prob)