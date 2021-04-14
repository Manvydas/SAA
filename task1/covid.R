# uzkraunami, paruosiami duomenys
covid <- fread("data/covid.csv")# [ , 1:4, with = T]
df <- covid[ , .(day, confirmed_daily)] # teigiam
names(df) <- c("dt", "kint")
df[ , dt := myDate(dt)]
df[ , "Realūs stebėjimai" := as.numeric(kint)]

# skaiciuojamas slenkantis vidurkis
df[ , "7 dienų slenkantis vidurkis" := myMa(kint, 7)]
df[ , "14 dienų slenkantis vidurkis" := myMa(kint, 14)]
df[ , kint := NULL]


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  scale_x_date(breaks = "2 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Naujų atvejų skaičius per dieną") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "covid1")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[-1]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Dienos",
            pdf.out = T, nm = paste0("covid2", which(nm.df == x)))
}


# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x)))))
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0)))


# Isfiltruojamas antros bangos laikotarpis
df3 <- df[dt >= "2020-10-01" & dt < "2021-02-01", ]

# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df3, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  scale_x_date(breaks = "1 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Naujų atvejų skaičius per dieną") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "covid3")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df3 <- names(df3)[-1]
for (x in nm.df3) {
  myPlotAcf((df3[ , get(x)]), laik = "Dienos",
            pdf.out = T, nm = paste0("covid4", which(nm.df3 == x)))
}

# autokoreliacijos koeficientas
lapply(df3[ , -1], function(x) length(x))
lapply(df3[ , -1], function(x) myCor(na.omit(x)))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df3[ , -1], function(x) head(which(myCor((x))[[2]] <= 2/sqrt(length(x)))))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df3[ , -1], function(x) head(which(myCor((x))[[2]] <= 0)))
