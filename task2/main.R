# //////////////////////////////////////////////////////////////////////////
# main.R ------------------------------------------------------------------
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# libraries ---------------------------------------------------------------
library(microbenchmark)
library(tuneR)
# tuneR::setWavPlayer('/usr/bin/afplay')
library(ggplot2)

source("functions.R")

# 1. DFT and FFT testing --------------------------------------------------
source("1_testing.R")

# 2. Walking signals seperation -------------------------------------------
source("2_steps.R")


# 3. Load signals ---------------------------------------------------------

# s.bird <- readWave("data/Birds-chirping-sound-effect.wav")
# s.bird <- mono(s.bird)
# s.bird <- myDataToPower2(s.bird)

## 24 bits
s.camp <- readWave("data/campfire-1.wav")
s.camp <- mono(s.camp, which = "left")
s.camp <- myDataToPower2(extractWave(s.camp, from = 0, to = 11, xunit = "time"))

s.man <- readWave("data/man-laughing-02.wav")
s.man <- mono(s.man, which = "left")
s.man <- myDataToPower2(s.man)

s.rain <- readWave("data/rain-01.wav")
s.rain <- mono(s.rain, which = "left")
s.rain <- myDataToPower2(extractWave(s.rain, from = 0, to = 11, xunit = "time"))

s.train <- readWave("data/train-pass-by-02.wav")
s.train <- mono(s.train, which = "left")
s.train <- myDataToPower2(extractWave(s.train, from = 0, to = 11, xunit = "time"))

## 16 bits
s.steph <- readWave("data/footsteps-heels.wav")
s.steph <- mono(s.steph, which = "left")
s.steph <- myDataToPower2(s.steph)

s.step <- readWave("data/footsteps-5.wav")
s.step <- mono(s.step, which = "left")
s.step <- myDataToPower2(extractWave(s.step, from = 0, to = 10, xunit = "time"))



# 
# 
# 
# # mono(campfire, "both")
# # s5 <- downsample(s4, 44100)
# str(campfire)
# s1 <- campfire@left
# 
# fft(1:16)-
#   myDft(1:16)
# 
# # se
# rain <- readWave("data/rain-04.wav")
# s2 <- rain@left
# 
# 
# ggg <- Wave(s2, samp.rate = 48000, bit = 24)