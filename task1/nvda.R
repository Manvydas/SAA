# uzkraunami, paruosiami duomenys
nvda <- fread("data/NVDA.csv")# [ , 1:4, with = T]
nvda[ , diff := abs(Open - Close)]
nvda <- nvda[year(Date) > 2018]
df <- nvda[ , .(Date, diff)]
names(df) <- c("dt", "kint")
df[ , dt := as_date(dt)]
df[ , "Realūs stebėjimai" := as.numeric(kint)]

# skaiciuojamas slenkantis vidurkis
df[ , "5 dienų slenkantis vidurkis" := myMa(kint, 5)]
# df[ , "20 dienų slenkantis vidurkis" := myMa(kint, 20)]
df[ , "60 dienų slenkantis vidurkis" := myMa(kint, 60)]
df[ , "120 dienų slenkantis vidurkis" := myMa(kint, 120)]
df[ , kint := NULL]


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  scale_x_date(breaks = "4 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Absoliutus kainos pokytis") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "nvda1")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[-1]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Dienos",
            pdf.out = T, nm = paste0("nvda2", which(nm.df == x)))
}

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x)))))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0)))
