# koreliacijos funkcija
myCor <- function(x){
  n <- length(x)
  nd <- n/2 # lagu skaicius
  kor <- numeric(nd) # reikalingo atsakymo vektoriaus ilgio tuscias vektorius
  for (d in 0:nd) {
    x1 <- x[1:(n-d)] # stebejimai
    x2 <- x[(d+1):n] # uzlaginti stebejimai
    dif1 <- (x1 - mean(x1))
    dif2 <- (x2 - mean(x2))
    sk1 <- sum(dif1 * dif2)
    vard1 <- sqrt(sum(dif1^2) * sum(dif2^2))
    kor[d+1] <- sk1 / vard1
    # kor[d+1] <- cor(x1,x2) # palyginimui
  }
  out <- data.frame(lag = 0:nd, kor = kor)
  return(out)
}

# sutvarkyti datai lietuviskiems duomenims
myDate <- function(kint){as.Date(paste0(gsub("M", "-", kint), "-01"), format = "%Y-%m-%d")}

# paprasto dvipusio slenkancio vidurkio funkcija
myMa <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}


# funkcija skirta isbrezti autokoreliacijos grafika paduodant vektoriu
myPlotAcf <- function(gg, spalva = "black", laik = "Dienos", x.breaks = 20) {
  conf.lims <- c(-1,1)*(2/sqrt(length(gg))) # pasikliautinis intervalas acf 
  acfplot <- myCor(gg) # paskaiciuojama autokoreliacija
  
  # acf grafikelis
  g <- ggplot(data = acfplot, mapping = aes(x = lag, y = kor)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0), color = spalva) +
    theme_bw() + geom_point(color = spalva) +
    scale_x_continuous(n.breaks = x.breaks) +
    scale_y_continuous(n.breaks = 10) +
    geom_hline(yintercept=conf.lims, lty=2, col='blue') +
    labs(x = laik, y = "Autokoreliacijos koeficientas") +
    theme(axis.title.x = element_text(vjust = 0, size = 12),
          axis.title.y = element_text(vjust = 2, size = 12))
  
  return(g)
}
