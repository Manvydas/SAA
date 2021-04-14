# myecg <- fread("data/ecg_2021-04-12.csv", fill=T)[[1]]
# df1 <- data.table(dt = seq(0, 30, length.out = length(myecg)),
#                   kint = as.numeric(myecg))
# myecg <- fread("data/ecg_2021-02-10.csv", fill=T)[[1]]
# myecg <- fread("data/ecg_2021-02-09.csv", fill=T)[[1]]
# myecg <- fread("data/ecg_2021-02-07.csv", fill=T)

# uzkraunami, paruosiami duomenys
myecg <- fread("data/ecg_2020-10-14.csv", fill=T)[[1]]
df <- data.table(dt = seq(0, 30, length.out = length(myecg)),
                 kint = as.numeric(myecg))

# skaiciuojamas slenkantis vidurkis
langas <- round(nrow(df)/25/10)
df[ , paste0(langas, " stebejimų (1/10 dūžio) slenkantis vidurkis") := as.numeric(myMa(kint, langas))]

# originaliu duomenu kreive
g <- ggplot(data = df, aes(x = dt, y = kint)) +
  geom_line(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red", "lightblue")) +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Laikas, sekundės", y = "EKG") +
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
mySavePdf(g, nm = "ekg1")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[2]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Laikas", x.breaks = 15,
            pdf.out = T, nm = paste0("ekg2", which(nm.df == x)))
}

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))

# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))) - 1)
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0) - 1)



df1 <- df[dt >= 6 & dt <= 12]
setnames(df1, "kint", "Realūs stebėjimai")
# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df1, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Laikas, sekundės", y = "EKG") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # legend.position = c(0.2, 0.8),
        legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "ekg3")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df1 <- names(df1)[-1]
for (x in nm.df1) {
  myPlotAcf(na.omit(df1[ , get(x)]), laik = "Laikas", x.breaks = 15,
            pdf.out = T, nm = paste0("ekg4", which(nm.df1 == x)))
}

# autokoreliacijos koeficientas
lapply(df1[ , -1], function(x) length(x))
lapply(df1[ , -1], function(x) myCor(na.omit(x)))

# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df1[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))) - 1))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df1[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0) - 1))



############################################################################
### Viskas tas pats sumazinus stebejimu skaiciu 4 kartus -------------------
############################################################################

myecg <- fread("data/ecg_2020-10-14.csv", fill=T)[[1]]
df <- data.table(dt = seq(0, 30, length.out = length(myecg)),
                 kint = as.numeric(myecg))
df <- df[seq(1, nrow(df), by = 4), ]
langas <- round(nrow(df)/25/10)
df[ , paste0(langas, " stebejimų (1/10 dūžio) slenkantis vidurkis") := as.numeric(myMa(kint, langas))]

# originaliu duomenu kreive
g <- ggplot(data = df, aes(x = dt, y = kint)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  # geom_line(data = df1, aes(x = dt, y = kint),size = 0.6, color = "red") +
  # geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red", "lightblue")) +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Laikas, sekundės", y = "EKG") +
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
mySavePdf(g, nm = "ekg5")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df <- names(df)[2]
for (x in nm.df) {
  myPlotAcf(na.omit(df[ , get(x)]), laik = "Laikas", x.breaks = 15,
            pdf.out = T, nm = paste0("ekg6", which(nm.df == x)))
}

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))

# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))) - 1))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0) - 1))


df1 <- df[dt >= 6 & dt <= 12]
setnames(df1, "kint", "Realūs stebėjimai")
# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df1, id.vars = "dt", variable.name = "variable")
g <- ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.6) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Laikas, sekundės", y = "EKG") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # legend.position = c(0.2, 0.8),
        legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# issaugomas bendras grafikas i pdf
mySavePdf(g, nm = "ekg7")

# issaugo acf grafikus visiems stulpeliams i atskirus pdf
nm.df1 <- names(df1)[-1]
for (x in nm.df1) {
  myPlotAcf(na.omit(df1[ , get(x)]), laik = "Laikas", x.breaks = 15,
            pdf.out = T, nm = paste0("ekg8", which(nm.df1 == x)))
}
# autokoreliacijos koeficientas
lapply(df1[ , -1], function(x) length(x))
lapply(df1[ , -1], function(x) myCor(na.omit(x)))

# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= reiksmingumo lygi
lapply(df1[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))) - 1))
# pirmos 6 reiksmes kai autokoreliacijos koeficientas <= 0
lapply(df1[ , -1], function(x) head(which(myCor(na.omit(x))[[2]] <= 0) - 1))
