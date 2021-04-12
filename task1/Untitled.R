myacf <- function(x, lag.max=10){
  x <- as.matrix(as.ts(x))
  x <- sweep(x, 2, colMeans(x, na.rm = TRUE), check.margin = FALSE)
  n <- nrow(x)
  macf <- as.numeric(sapply(0:lag.max, function(k) {sum(x[1:(n-k)]*x[(k+1):n])/sum(x^2)}))
  names(macf) <- 0:lag.max
  return(macf)
}

## Examples from Venables & Ripley ?acf
# require(graphics)
x=rnorm(200)
rbind(acf(x,10,plot=F)$acf,
      myacf(x,10)) 


system.time(myacf(x,10))

lag.max=10
n <- nrow(x)
system.time(as.numeric(sapply(0:lag.max, function(k) {sum((x[1:(n-k)] - mean(x[1:(n-k)])) * (x[(k+1):n] - mean(x[(k+1):n]))) /
    sqrt((x[1:(n-k)] - mean(x[1:(n-k)])))^2 * (x[(k+1):n] - mean(x[(k+1):n])^2)})))
#########################
mycor <- function(x, d=NA){
  n <- length(x)
  nd <- n/4
  out <- numeric(nd)
  for (d in 0:nd) {
    x1 <- x[1:(n-d)]
    x2 <- x[(d+1):n]
    dif1 <- (x1 - mean(x1))
    dif2 <- (x2 - mean(x2))
    sk1 <- sum(dif1 * dif2)
    vard1 <- sqrt(sum(dif1^2) * sum(dif2^2))
    out[d+1] <- sk1 / vard1
  }
  return(out)

  # sum(
  #   (x[1:(n-d)] - mean(x[1:(n-d)])) * (x[(d+1):n] - mean(x[(d+1):n]))
  # ) /
  #   sqrt(
  #     sum((x[1:(n-d)] - mean(x[1:(n-d)]))^2) * sum((x[(d+1):n] - mean(x[(d+1):n]))^2)
  #   )
}


x <- rnorm(200)
d <- 5
mycor(x)
