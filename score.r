score <- function(x) UseMethod("score", x)
score.integer <- function(x) {
  score <- 0
  # Pull out any three-of-a-kinds
  for (i in 1:6) {
    ndx <- x == i
    triple <- sum(ndx) >= 3
    if (triple) break
  }
  if (triple) {
    score <- score + ifelse(i == 1, 1000, i * 100)
    w <- which(ndx)
    x <- x[-w[1:3]]
  }
  # pull out aces
  w <- which(x == 1L)
  if (l <- length(w)) {
    score <- score + l * 100
    x <- x[-w]
  }
  # pull out fives
  w <- which(x == 5L)
  if (l <- length(w)) {
    score <- score + l * 50
    x <- x[-w]
  }
  attr(score, "diceremaining") <- x
  score
}
score.selectionSet <- function(x) score(c(x))
score.list <- function(x) 0L
score.savedSelectionSets <- function(x) ifelse(length(x), sum(sapply(x, score)), 0L)
score.turn <- function(x) ifelse(length(x), sum(sapply(x, score)), 0L)
score.player <- function(x) ifelse(length(x), sum(sapply(x, score)), 0L)
score.NULL <- function(x) 0L

assign("score", score, envir = .GlobalEnv)
assign("score.integer", score.integer, envir = .GlobalEnv)
assign("score.selectionSet", score.selectionSet, envir = .GlobalEnv)
assign("score.list", score.list, envir = .GlobalEnv)
assign("score.savedSelectionSets", score.savedSelectionSets, envir = .GlobalEnv)
assign("score.turn", score.turn, envir = .GlobalEnv)
assign("score.player", score.player, envir = .GlobalEnv)
assign("score.NULL", score.NULL, envir = .GlobalEnv)

