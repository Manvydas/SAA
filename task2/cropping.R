## rain and train
s.rain <- readWave("rain-01.wav")
s.rain <- mono(s.rain, which = "left")
s.rain <- myDataToPower2(extractWave(s.rain, from = 0, to = 11, xunit = "time"))

s.train <- readWave("train-pass-by-02.wav")
s.train <- mono(s.train, which = "left")
s.train <- myDataToPower2(extractWave(s.train, from = 0, to = 11, xunit = "time"))

play(s.rain)
play(s.train)

s.train.comb <- 0.5*s.rain + 0.5*s.train
s.train.comb@left <- Re(myFFT(myFFT(s.rain@left) + myFFT(s.train@left), inverse = TRUE))
# play(s.train.comb)

ss@left <- myFilter(s.train.comb@left, low = 50, high = 10000)
summary(ss@left)
ss@left <- ss@left/10^14
play(ss)

################

ss <- s.step.comb
ss@left<-myFilter(s.step.comb@left, low = 5, high = 1000000)
summary(ss@left)
ss@left <- ss@left/10^6
play(ss)

play(s.step)
play(s.steph)
play(s.step.comb)

# ll <- 1:(length(s.step.comb@left))/5
# a <- myFreq(s.step.comb@left)
# plot(a[ll], abs(fft(s.step.comb@left))[ll], type="l")
# lines(a[ll], abs(fft(ss@left))[ll], col="red")


######### glass sound
s.man <- readWave("glass-clink-1.wav")
s.man <- mono(s.man, which = "left")
s.man@left <- myDataToPower2(s.man@left, type = 'mean')

play(s.man)

ss <- s.man
ss@left <- myFilter(s.man@left, low = 2000, high = 5000)
summary(ss@left)
ss@left <- ss@left/10^5
summary(ss@left)
play(ss)

ll <- 1:(length(s.man@left))/5
a <- myFreq(s.man@left)
plot(a[ll], abs(fft(s.man@left))[ll], type="l", lwd=2)
lines(a[ll], abs(fft(ss@left))[ll], col="red")

