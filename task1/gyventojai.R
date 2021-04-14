# uzkraunami, paruosiami duomenys
set.seed(95) # fiksuoja random generavima
dat1 <- fread("data/datatable.csv") # gyventojai, naujagymiai, mirtys
dat1 <- dat1[Rodiklis == "Nuolatini\305\263 gyventoj\305\263 skai\304\215ius m\304\227nesio prad\305\276ioje"]
dat1[ , Date := myDate(Laikotarpis)]
df <- dat1[ , c("Date", "Reik\305\241m\304\227"), with = T]
names(df) <- c("dt", "kint")
df[ , dt := as_date(dt)]


# uztriuksminamas signalas
triuksmas <- rnorm(nrow(df), mean = 0, sd = 50000)
df[, "Užtriukšmintas signalas (sd=50000)" := kint + triuksmas]
triuksmas <- rnorm(nrow(df), mean = 0, sd = 200000)
df[, "Užtriukšmintas signalas (sd=200000)" := kint + triuksmas]
triuksmas <- rnorm(nrow(df), mean = 0, sd = 400000)
df[, "Užtriukšmintas signalas (sd=400000)" := kint + triuksmas]


# originaliu duomenu kreive
g <- ggplot(data = df[ , 1:2], aes(x = dt, y = kint)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red", "lightblue")) +
  scale_x_date(breaks = "34 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Gyventojų skaičius") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.75, 0.85),
        # legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "gyventojai1")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[2]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Mėnesiai",
            pdf.out = T, nm = paste0("gyventojai2", which(nm.df == x)))
}

# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df[ , -2], id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c( "darkgreen","darkred", "steelblue", "red", "lightblue")) +
  scale_x_date(breaks = "34 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Gyventojų skaičius") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.75, 0.86),
        # legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "gyventojai3")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[-(1:2)]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Mėnesiai",
            pdf.out = T, nm = paste0("gyventojai4", which(nm.df == x)))
}

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
lapply(df[ , -1], function(x) min(myCor(na.omit(x))[[2]]))

# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x)))))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0)))
