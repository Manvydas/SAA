# uzkraunami, paruosiami duomenys
ivykiai <- fread("data/ivykiai.csv")
df <- ivykiai[ , c(1, 5), with = T]
names(df) <- c("dt", "kint")
df[ , dt := myDate(dt)]
df <- df[year(dt) >= 2010 & year(dt) < 2021]
df[ , "Realūs stebėjimai" := as.numeric(kint)]

# skaiciuojamas slenkantis vidurkis
# df[ , "2 mėnesių slenkantis vidurkis" := myMa(kint, 2)]
# df[ , "3 mėnesių slenkantis vidurkis" := myMa(kint, 3)]
# df[ , "4 mėnesių slenkantis vidurkis" := myMa(kint, 4)]
df[ , "6 mėn. slenkantis vidurkis" := myMa(kint, 6)]
df[ , "12 mėn. slenkantis vidurkis" := myMa(kint, 12)]
# df[ , "14 dienų slenkantis vidurkis" := myMa(kint, 14)]
# # df[ , "Slenkančio vidurkio algoritmas, 30 dienų" := myMa(kint, 30)]
# # df[ , "Slenkančio vidurkio algoritmas, 90 dienų" := myMa(kint, 90)]
df[ , kint := NULL]


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c( "#6e6a6a","darkred", "steelblue", "red", "lightblue", "orange")) +
  scale_x_date(breaks = "19 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Gyventojų skaičius") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.75, 0.9),
        # legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "ivykiai1")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[-1]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Mėnesiai",
            pdf.out = T, nm = paste0("ivykiai2", which(nm.df == x)))
}

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))

# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))) - 1))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0) - 1))
