# DFT - Discrete Fourier Transform ----------------------------------------
myDFT <- function(X, inverse = FALSE) {
  N <- length(X)
  j <- 0:(N-1)
  c_k <- rep(1i, N)
  
  # mult <- if(inverse) c(1, 1) else c(-1, 1/N)
  mult <- if(inverse) c(1, 1) else c(-1, 1) # R pakete nedalina is N, lyginant su FFT ir nereikia dalinti
  
  const <- mult[1] * 1i * (2 * pi / N) * j # paskaiciuojama dalis, kuri nepriklauso nuo k
  c_k <- sapply(1:N, function(k) {mult[2] * sum(X * exp(const * (k-1)))})
  
  # for (k in 1:N) {
  #   # exponent <- exp(mult[1] * 1i * (2 * pi / N) * j * k)
  #   exponent <- exp(const * (k-1)) # nes R skaiciuoja nuo 1, ne nuo 0
  #   c_k[k] <- mult[2] * sum(X * exponent)
  # }
  return(c_k)
}

# FFT - Fast Fourier Transform --------------------------------------------
myFFT <- function(X, inverse = FALSE) {
  N <- length(X)
  
  mult <- if (inverse) 1 else -1
  
  if (N != 1){
    dd <- X
    dd[1:(N/2)] <- myFFT(X[(1:(N/2)) * 2 - 1], inverse)
    dd[(N/2+1):N] <- myFFT(X[(1:(N/2)) * 2], inverse)
    
    X_even <- dd[1:(N/2)]
    X_odd <- dd[(N/2+1):N]
    W_n <- exp(mult * 1i * 2 * pi * (0:(N/2-1)) / N)
    
    X[1:(N/2)] <- X_even + W_n * X_odd
    X[(N/2+1):N] <- X_even - W_n * X_odd
    
  }
  return(X)
}

# FFT with check for non zero imaginary part
# myFFTrun <- function(X, inverse = FALSE){
#   X <- myFFT(X, inverse)
#   if (inverse) {
#     if(max(abs(Im(X))) <= 10^(-10)){
#       return(X)
#     } else {
#       print("Imaginary part is non zero. Check for mistakes.")
#     }
#   } else{
#     return(X)
#   }
# }

# myfft <- function(X, inverse = FALSE){
#   N <- length(X)
#   mult <- ifelse(inverse, 1, -1)
#   if (N <= 2) {
#     myDFT(X)
#   } else {
#     X_even <- myfft(X[seq(1, N-1, 2)], inverse)
#     X_odd = myfft(X[seq(2, N, 2)], inverse)
#     W_k <- exp(mult * 2i * pi * 0:(floor(N/2 - 1)) / N)
#     left <- X_even + W_k * X_odd
#     right = X_even - W_k * X_odd
#     c(left, right)
#   }
# }



# Helper functions --------------------------------------------------------

# check if number is a power of 2
myIsPower2 <- function(X) {
  n1s <- sum(as.numeric(intToBits(X)))
  if (n1s == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# find closest number which is in the power of 2, select direction (prev=T means previous)
myToPower2 <- function(X, prev = TRUE) {
  if (prev) {
    2^floor(log2(X))
  } else {
    2^ceiling(log2(X))
  }
}

# make data length equal to power of 2
myDataToPower2 <- function(X, type = "drop"){
  # c("drop", "mean", "median")
  N <- length(X)
  if (type == "drop") {
    # truncate signal to the length equal to the previous power of 2
    N <- myToPower2(N, prev = TRUE)
    X <- X[1:N]
  } else {
    # extend signal to the length of next power of 2
    N2 <- myToPower2(N, prev = FALSE)
    X[N:N2] <- switch(type,
                      mean = mean(X),
                      median = median(X))
  }
  return(X)
}

# save image to .pdf
mySavePdf <- function(gg, nm){
  cairo_pdf(paste0("out/", nm, ".pdf"), width = 7.2, height = 4)
  plot(gg)
  dev.off()
}

# frequency calculation ---------------------------------------------------
myFreq <- function(X, samplingr = 1/48000){
  N <- length(X)
  value <- 1 / (N * samplingr)
  freq_p <- 1:(N / 2)
  freq_n <- (N / 2):1 * (-1)
  freq <- c(freq_p, freq_n)
  
  out <- freq * value
  
  return(out)
}

# filtering ---------------------------------------------------------------

myFilter <- function(X, samplingr = 1/48000, low=NULL, high=NULL){
  dd <- myFFT(X)
  freq <- abs(myFreq(dd, samplingr))
  
  dd[freq < low | freq > high] <- 0
  dd <- Re(myFFT(dd, inverse = TRUE))
  return(dd)
}


