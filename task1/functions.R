# autokoreliacijos funkcija
myCor <- function(x){
  N <- length(x) # stebėjimų skaičius
  nd <- N/2 # lagų skaičius
  kor <- numeric(nd) # atsakymų vektoriaus ilgio tuščias vektorius
  for (d in 0:nd) {
    x1 <- x[1:(N-d)] # stebėjimai
    x2 <- x[(d+1):N] # užlaginti stebėjimai
    dif1 <- (x1 - mean(x1))
    dif2 <- (x2 - mean(x2))
    skait <- sum(dif1 * dif2) # skaitiklis
    vard <- sqrt(sum(dif1^2) * sum(dif2^2)) # vardiklis
    kor[d+1] <- skait / vard
  }
  out <- data.frame(lag = 0:nd, kor = kor)
  return(out)
}

# sutvarkyti datai lietuviskiems duomenims
myDate <- function(kint){as.Date(paste0(gsub("M", "-", kint), "-01"), format = "%Y-%m-%d")}

# paprasto dvipusio slenkancio vidurkio funkcija
myMa <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

# funkcija issaugoti grafika i .pdf
mySavePdf <- function(gg, nm){
  cairo_pdf(paste0("out/", nm, ".pdf"), width = 7.2, height = 4)
  plot(gg)
  dev.off()
}

# funkcija skirta isbrezti autokoreliacijos grafika paduodant vektoriu
myPlotAcf <- function(gg, spalva = "black", laik = "Dienos", x.breaks = 20,
                      pdf.out = F, nm = "name") {
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
  
  if (pdf.out) return(mySavePdf(g, nm)) else return(g)
}
