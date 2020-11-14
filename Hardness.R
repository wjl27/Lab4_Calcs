####### HV Calculations ########

library(tidyverse)

data <- as_tibble(read.csv("Lab4_HardnessData.csv"))

####### Base Metal ########

base.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 5041 | x.coord > 23889)

N.base = nrow(base.dat) # Number of Samples

nu.base <- N.base - 1 # degrees of freedom

x_bar.base = (1/N.base)*sum(base.dat$Hardness)

s_x2.base <- (1/nu.base)*sum((base.dat$Hardness - x_bar.base)^2) # this is the sample variance of the data set

s_x.base <- sqrt(s_x2.base) # this is the standard deviation of the data set

s_x_bar.base <- s_x.base/sqrt(N.base) # This is the standard error in the data set

####### HAZ ########

HAZ.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 20441 & x.coord > 7699) %>%
  filter(x.coord > 18049 | x.coord < 10091) %>%
  filter(y.coord < 8241 & y.coord > 4789)

N.HAZ = nrow(HAZ.dat) # Number of Samples

nu.HAZ <- N.HAZ - 1 # degrees of freedom

x_bar.HAZ = (1/N.HAZ)*sum(HAZ.dat$Hardness)

s_x2.HAZ <- (1/nu.HAZ)*sum((HAZ.dat$Hardness - x_bar.HAZ)^2) # this is the sample variance of the data set

s_x.HAZ <- sqrt(s_x2.HAZ) # this is the standard deviation of the data set

s_x_bar.HAZ <- s_x.HAZ/sqrt(N.HAZ) # This is the standard error in the data set

####### FZ ########

FZ.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 15141 & x.coord > 11679)

N.FZ = nrow(FZ.dat) # Number of Samples

nu.FZ <- N.FZ - 1 # degrees of freedom

x_bar.FZ = (1/N.FZ)*sum(FZ.dat$Hardness)

s_x2.FZ <- (1/nu.FZ)*sum((FZ.dat$Hardness - x_bar.FZ)^2) # this is the sample variance of the data set

s_x.FZ <- sqrt(s_x2.FZ) # this is the standard deviation of the data set

s_x_bar.FZ <- s_x.FZ/sqrt(N.FZ) # This is the standard error in the data set
