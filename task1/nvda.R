nvda <- fread("data/NVDA.csv")# [ , 1:4, with = T]
nvda[ , diff := abs(Open - Close)]
nvda <- nvda[year(Date) > 2018]
df <- nvda[ , .(Date, diff)]
names(df) <- c("dt", "kint")
df[ , dt := as_date(dt)]
df[ , "Realūs stebėjimai" := as.numeric(kint)]
df[ , "5 dienų slenkantis vidurkis" := myMa(kint, 5)]
# df[ , "20 dienų slenkantis vidurkis" := myMa(kint, 20)]
df[ , "60 dienų slenkantis vidurkis" := myMa(kint, 60)]
df[ , "120 dienų slenkantis vidurkis" := myMa(kint, 120)]
df[ , kint := NULL]


# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df, id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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

# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df[ , -1], function(x) myPlotAcf(na.omit(x), laik = "Dienos"))

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))))

