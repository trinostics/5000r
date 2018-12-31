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

list2env(list(score = score,
              score.integer = score.integer,
              score.selectionSet = score.selectionSet,
              score.list = score.list,
              score.savedSelectionSets = score.savedSelectionSets,
              score.turn = score.turn,
              score.player = score.player,
              score.NULL = score.NULL),
         envir = .GlobalEnv)

