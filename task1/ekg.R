myecg <- fread("data/ecg_2021-04-12.csv", fill=T)[[1]]
df1 <- data.table(dt = seq(0, 30, length.out = length(myecg)),
                  kint = as.numeric(myecg))
# myecg <- fread("data/ecg_2021-02-10.csv", fill=T)[[1]]
# myecg <- fread("data/ecg_2021-02-09.csv", fill=T)[[1]]
# myecg <- fread("data/ecg_2021-02-07.csv", fill=T)[[1]]
myecg <- fread("data/ecg_2020-10-14.csv", fill=T)[[1]]
df <- data.table(dt = seq(0, 30, length.out = length(myecg)),
                 kint = as.numeric(myecg))
# df <- df[seq(1, nrow(df), by = 2), ]
langas <- round(nrow(df)/25/10)
df[ , paste0(langas, " stebejimų slenkantis vidurkis") := as.numeric(myMa(kint, langas))]

# originaliu duomenu kreive
ggplot(data = df, aes(x = dt, y = kint)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  # geom_line(data = df1, aes(x = dt, y = kint),size = 0.7, color = "red") +
  # geom_point(size = 0.7) +
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

myPlotAcf(df[[2]], laik = "Laikas", x.breaks = 15)

df1 <- df[dt >= 6 & dt <= 12]
setnames(df1, "kint", "Realūs stebėjimai")
# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df1, id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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


# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df1[, -1], function(x) myPlotAcf((x), laik = "Laikas", x.breaks = 15))

# autokoreliacijos koeficientas
lapply(df1[ , -1], function(x) length(x))
lapply(df1[ , -1], function(x) myCor(na.omit(x)))
lapply(df1[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0))
lapply(df1[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))))


############################################################################
############################################################################
############################################################################



myecg <- fread("data/ecg_2020-10-14.csv", fill=T)[[1]]
df <- data.table(dt = seq(0, 30, length.out = length(myecg)),
                 kint = as.numeric(myecg))
df <- df[seq(1, nrow(df), by = 4), ]
langas <- round(nrow(df)/25/10)
df[ , paste0(langas, " stebejimų slenkantis vidurkis") := as.numeric(myMa(kint, langas))]

# originaliu duomenu kreive
ggplot(data = df, aes(x = dt, y = kint)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  # geom_line(data = df1, aes(x = dt, y = kint),size = 0.7, color = "red") +
  # geom_point(size = 0.7) +
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

myPlotAcf(df[[2]], laik = "Laikas", x.breaks = 15)

df1 <- df[dt >= 6 & dt <= 12]
setnames(df1, "kint", "Realūs stebėjimai")
# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df1, id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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


# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df1[, -1], function(x) myPlotAcf((x), laik = "Laikas", x.breaks = 15))

# autokoreliacijos koeficientas
lapply(df1[ , -1], function(x) length(x))
lapply(df1[ , -1], function(x) myCor(na.omit(x)))
lapply(df1[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0))
lapply(df1[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))))
