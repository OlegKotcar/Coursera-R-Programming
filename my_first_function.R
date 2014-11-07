add2 <- function(x,y){
  x+y
}

above10 <- function(x){
  use <- x > 10
  x[use]
}

above <- function(x, n){
  use <- x > n
  x[use]
}

columnmean <- function(x, rmNA = TRUE){
  numcol <- ncol(x)
  means <- numeric(numcol)
  for(i in 1:numcol) {
    means[i] <- mean(x[, i], na.rm = rmNA)
  }  
  means
}