ivykiai <- fread("data/ivykiai.csv")
df <- ivykiai[ , c(1, 5), with = T]
names(df) <- c("dt", "kint")
df[ , dt := myDate(dt)]
df <- df[year(dt) >= 2010 & year(dt) < 2021]
df[ , "Realūs stebėjimai" := as.numeric(kint)]
df[ , "2 mėnesių slenkantis vidurkis" := myMa(kint, 2)]
df[ , "3 mėnesių slenkantis vidurkis" := myMa(kint, 3)]
df[ , "4 mėnesių slenkantis vidurkis" := myMa(kint, 4)]
df[ , "6 mėnesių slenkantis vidurkis" := myMa(kint, 6)]
df[ , "12 mėnesių slenkantis vidurkis" := myMa(kint, 12)]
# df[ , "14 dienų slenkantis vidurkis" := myMa(kint, 14)]
# # df[ , "Slenkančio vidurkio algoritmas, 30 dienų" := myMa(kint, 30)]
# # df[ , "Slenkančio vidurkio algoritmas, 90 dienų" := myMa(kint, 90)]
df[ , kint := NULL]

# originaliu duomenu kreive
ggplot(data = df, aes(x = dt, y = `Realūs stebėjimai`)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red", "lightblue")) +
  scale_x_date(breaks = "24 month") +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Data", y = "Įvykių skaičius") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.75, 0.85),
        # legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
  theme_bw() +
  scale_color_manual(values = c( "darkgreen","darkred", "steelblue", "red", "lightblue", "orange")) +
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

# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df[ , -1], function(x) myPlotAcf(na.omit(x), laik = "Mėnesiai"))

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))))


