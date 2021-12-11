library(tidyverse)
# load in brexit data
brexit <- readRDS(url('https://github.com/QMUL-SPIR/Public_files/blob/master/datasets/BrexitResults.rds?raw=true'))

# We'll be using data on constituency-level results of the 2016 referendum on membership of the EU. Load in the data and familiarise yourself with the codebook below.

summary(brexit)

# T-test (one sample hypothesis test)

brexit_400 <- sample_n(brexit, 400)

mean(brexit_400$PercFemale) # an estimate of the true population mean

n <- length(brexit_400$PercFemale)
n

# standard error calculation
se.y_bar <- sd(brexit_400$PercFemale) / sqrt(n)
