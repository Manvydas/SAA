
covid <- fread("data/covid.csv")# [ , 1:4, with = T]
df <- covid[ , .(day, confirmed_daily)] # teigiam
names(df) <- c("dt", "kint")
df[ , dt := myDate(dt)]
df[ , "Realūs stebėjimai" := as.numeric(kint)]
df[ , "7 dienų slenkantis vidurkis" := myMa(kint, 7)]
df[ , "14 dienų slenkantis vidurkis" := myMa(kint, 14)]
# df[ , "Slenkančio vidurkio algoritmas, 30 dienų" := myMa(kint, 30)]
# df[ , "Slenkančio vidurkio algoritmas, 90 dienų" := myMa(kint, 90)]
df[ , kint := NULL]


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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

# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df[ , -1], function(x) myPlotAcf(na.omit(x), laik = "Dienos"))

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))))

# Isfiltruojamas antros bangos laikotarpis
df3 <- df[dt >= "2020-10-01" & dt < "2021-02-01", ]

# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df3, id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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

# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df3[ , -1], function(x) myPlotAcf(x, laik = "Dienos"))

# autokoreliacijos koeficientas
lapply(df3[ , -1], function(x) length(x))
lapply(df3[ , -1], function(x) myCor(na.omit(x)))
lapply(df3[ , -1], function(x) which(myCor((x))[[2]] <= 0))
lapply(df3[ , -1], function(x) which(myCor((x))[[2]] <= 2/sqrt(length(x))))



