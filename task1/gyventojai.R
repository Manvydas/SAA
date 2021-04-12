set.seed(95)
dat1 <- fread("data/datatable.csv") # gyventojai, naujagymiai, mirtys
dat1 <- dat1[Rodiklis == "Nuolatini\305\263 gyventoj\305\263 skai\304\215ius m\304\227nesio prad\305\276ioje"]
dat1[ , Date := myDate(Laikotarpis)]

df <- dat1[ , c("Date", "Reik\305\241m\304\227"), with = T]
names(df) <- c("dt", "kint")
df[ , dt := as_date(dt)]


# df[ , "Realūs stebėjimai" := kint]
# triuksmas <- rnorm(nrow(df), mean = 0, sd = 5000)
# df[, "Užtriukšmintas signalas (sd=5000)" := kint + triuksmas]
# triuksmas <- rnorm(nrow(df), mean = 0, sd = 10000)
# df[, "Užtriukšmintas signalas (sd=10000)" := kint + triuksmas]
triuksmas <- rnorm(nrow(df), mean = 0, sd = 50000)
df[, "Užtriukšmintas signalas (sd=50000)" := kint + triuksmas]
# triuksmas <- rnorm(nrow(df), mean = 0, sd = 100000)
# df[, "Užtriukšmintas signalas (sd=100000)" := kint + triuksmas]
triuksmas <- rnorm(nrow(df), mean = 0, sd = 200000)
df[, "Užtriukšmintas signalas (sd=200000)" := kint + triuksmas]
# triuksmas <- rnorm(nrow(df), mean = 0, sd = 300000)
# df[, "Užtriukšmintas signalas (sd=300000)" := kint + triuksmas]
triuksmas <- rnorm(nrow(df), mean = 0, sd = 400000)
df[, "Užtriukšmintas signalas (sd=400000)" := kint + triuksmas]
# df[ , kint := NULL]

# originaliu duomenu kreive
ggplot(data = df[ , 1:2], aes(x = dt, y = kint)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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

# duomenu kreives itraukiant kintamuosius suglodintus slenkanciu vidurkiu
df2 <- melt(data = df[ , -2], id.vars = "dt", variable.name = "variable")
ggplot(data = df2, aes(x = dt, y = value, color = variable)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
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

# isspaussdinti acf grafikus visiems stulpeliams
# lapply(df[ , -1], function(x) myPlotAcf(acf(na.omit(x), lag.max = length(x)/2, plot = F), laik = "Metai"))
lapply(df[ , -1], function(x) myPlotAcf(na.omit(x), laik = "Mėnesiai"))

# autokoreliacijos koeficientas
lapply(df[ , -1], function(x) length(x))
lapply(df[ , -1], function(x) myCor(na.omit(x)))
lapply(df[ , -1], function(x) min(myCor(na.omit(x))[[2]]))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 0))
lapply(df[ , -1], function(x) which(myCor(na.omit(x))[[2]] <= 2/sqrt(length(x))))






