#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Signalu analize ir apdorojimas, 1 uzduotis ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# |-- uzkrauna reikalingas bibliotekas
library(data.table)
library(lubridate)
library(ggplot2)

# |-- uzkrauna mano funkcijas 
source("functions.R")

# paleidus viena is sekanciu "source()" eiluciu bus sugeneruoti atitinkama tema
# .pdf grafikai i "./out/" aplanka

# |-- COVID-19 LT (dieninis) --------------------------------------------------
source("covid.R")

# |-- NVDA absoliutus dieninis skirtumas (dieninis) ---------------------------
source("nvda.R")

# |-- Nuolatiniu gyventoju skaicius LT (menesinis) ----------------------------
source("gyventojai.R")

# |-- Keliu eismo ivykiai, kuriuose nukentejo zmones (menesinis) --------------
source("ivykiai.R")

# |-- Metine vidutine temperatura Siaures pusrutulyje (metinis) ---------------
source("nhyear.R")

# |-- Mano EKG (30 sekundziu - 15407 stebejimu) -------------------------------
source("ekg.R")
