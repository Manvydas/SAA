# 1. DFT and FFT testing --------------------------------------------------
s1.max <- 2^3
s1 <- 1:s1.max
cat("1. Real signal (s1): ", s1, ";\n")

# 1.1. DFT ----------------------------------------------------------------
real1 <- Re(myDFT(myDFT(s1), inverse = T)) / s1.max
imag1 <- max(abs(Im(myDFT(myDFT(s1), inverse = T)) / s1.max))

cat("1.1. Inverse_myDFT(myDFT(s1)):",
    "real:", real1,
    ", maximum absolute imaginary:", imag1, ";\n")

# 1.2. FFT ----------------------------------------------------------------
real2 <- Re(myFFT(myFFT(s1), inverse = T)) / s1.max
imag2 <- max(abs(Im(myFFT(myFFT(s1), inverse = T)) / s1.max))

cat("1.2. ImyFFT(myFFT(s1)):",
    "real:", real2,
    ", maximum absolute imaginary:", imag2, ";\n")

# 1.3. myFFT vs stats::fft ------------------------------------------------
diff1 <- myFFT(s1) - fft(s1)
real3 <- max(abs(Re(diff1)))
imag3 <- max(abs(Im(diff1)))

cat("1.3. Maximum absolute difference between myFFT(s1) and stats::fft(s1):",
    "real:", real3,
    ", imaginary:", imag3, ";\n")

# 1.4. microbenchmark -----------------------------------------------------
s2.max <- 2^10
s2 <- 1:s2.max
bench1 <- microbenchmark::microbenchmark(
  myDFT(s2),
  myFFT(s2),
  fft(s2)
)

cat("1.4. Microbenchmarking (signal (s2) from 1 to", s2, "): \n")
print(bench1)

# 1.5. microbenchmark -----------------------------------------------------
s5.max <- 14
ind <- 0
for (j in 2^(1:s5.max)) {
  X <- 1:j
  dd <- microbenchmark::microbenchmark(
    myDFT(X),
    myFFT(X),
    fft(X),
    times = 10
  )
  # calculate means
  dd <- aggregate(. ~ expr, dd, function(x) mean(x/1000))
  dd <- cbind(log2 = log2(j), reshape2::dcast(dd, . ~ expr, value.var = "time"))
  
  if (ind == 0) {
    bench5 <- dd
  } else {
    bench5 <- rbind(bench5, dd)
  }
  
  ind <- ind + 1
}
# cat("1.4. Microbenchmarking (signal (s2) from 1 to", s2, "): \n")
# print(bench1)

bench5$. <- NULL
names(bench5) <- gsub("[(X)]", "", names(bench5))
dat <- data.table::melt(bench5, id.vars = "log2", variable.name = "variable")

require(ggplot2)
gg <- ggplot(data = dat, aes(x = log2, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Signalo ilgis log_2", y = "Laikas, mikrosekundės") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # legend.position = c(0.2, 0.8),
        legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# save to pdf
mySavePdf(gg, nm = "bench")
