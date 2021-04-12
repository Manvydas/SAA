nhyear <- read.table("https://archive.uea.ac.uk/~gj/book/data/nhyear.dat", header=F)
df <- nhyear
names(df) <- c("dt", "kint")



# originaliu duomenu kreive
ggplot(data = df, aes(x = dt, y = kint)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_point(size = 0.7) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red", "lightblue")) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Metai", y = "Vidutinė temperatūra") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.75, 0.85),
        # legend.position = "bottom",
        legend.title = element_blank(), # element_text(size = 0),
        legend.text = element_text(size = 12),
        legend.background=element_blank())


myPlotAcf(df[ , -1], laik = "Metai")
