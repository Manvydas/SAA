## 16 bits
s.steph <- readWave("data/footsteps-heels.wav")
s.steph <- mono(s.steph, which = "left")
s.steph <- myDataToPower2(s.steph)

s.step <- readWave("data/footsteps-5.wav")
s.step <- mono(s.step, which = "left")
s.step <- myDataToPower2(extractWave(s.step, from = 0, to = 10, xunit = "time"))

s.step.comb <- s.steph + s.step
ss <- fft(s.step.comb@left)

# s.step.comb@left <- 

# s.step.comb <- myFFT(myFFT(s.steph@left) + myFFT(s.step@left), inverse = TRUE)
# 
# s.step.comb <- Wave(s.step.comb, )

ilgis <- 1:length(as.numeric(s.step.comb@left))
d <- data.frame(ilgis=ilgis, ejimas="sujungta", bb=as.numeric(s.step.comb@left))
d <- rbind(d, data.frame(ilgis=ilgis, ejimas="su aukstakulniais", bb=as.numeric(s.steph@left)))
d <- rbind(d, data.frame(ilgis=ilgis, ejimas="per kilima", bb=as.numeric(s.step@left)))


# ilgis <- 1:length(as.numeric(s.step.comb@left))
# d <- data.frame(ilgis=ilgis, ejimas="sujungta", bb=abs(Re(fft(as.numeric(s.step.comb@left)))^2))[1:(max(ilgis)/50), ]
# d <- rbind(d, data.frame(ilgis=ilgis, ejimas="aukstakulniais", bb=abs(Re(fft(as.numeric(s.steph@left)))^2))[1:(max(ilgis)/50), ])
# d <- rbind(d, data.frame(ilgis=ilgis, ejimas="per kilima", bb=abs(Re(fft(as.numeric(s.step@left)))^2))[1:(max(ilgis)/50), ])


# plot
gg <- ggplot(data = d, aes(x = ilgis, y = bb, color = ejimas)) +
  # geom_point(data = df2[variable == "kint"], aes(x = dt, y = value, color = variable)) +
  geom_line(size = 0.5) +
  # geom_point(size = 0.7) +
  theme_bw() +
  scale_color_manual(values = c("#6e6a6a", "darkred", "steelblue", "darkgreen", "red")) +
  # scale_x_continuous(n.breaks = 10) +
  # scale_y_continuous(n.breaks = 10) +
  labs(x = "Laikas", y = "Signalo reikšmės") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # legend.position = c(0.2, 0.8),
        legend.position = "bottom",
        # legend.title = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.background=element_blank())

# save to pdf
mySavePdf(gg, nm = "steps")
