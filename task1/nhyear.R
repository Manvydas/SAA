# uzkraunami, paruosiami duomenys
nhyear <- read.table("https://archive.uea.ac.uk/~gj/book/data/nhyear.dat", header=F)
df <- as.data.table(nhyear)
names(df) <- c("dt", "kint")
df[ , "Realūs stebėjimai" := as.numeric(kint)]

# skaiciuojamas slenkantis vidurkis
df[ , "5 metų slenkantis vidurkis" := myMa(kint, 5)]
df[ , "20 metų slenkantis vidurkis" := myMa(kint, 20)]
df[ , kint := NULL]


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c( "#6e6a6a","darkred", "steelblue", "red", "lightblue", "orange")) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Metai", y = "Vidutinė temperatūra") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.25, 0.85),
        # legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "nhyear1")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[-1]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Metai",
            pdf.out = T, nm = paste0("nhyear2", which(nm.df == x)))
}

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))

# minimali autokoreliacijos reiksme per kintamaji
lapply(df[ , -1], function(x) min(myCor(na.omit(x))[[2]]))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x)))))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0)))
