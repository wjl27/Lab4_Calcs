####### HV Calculations v1.1 ########

library(tidyverse)

data <- as_tibble(read.csv("Lab4_HardnessData.csv"))

####### Section 1 ########

one.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 2919 | x.coord > 26809) %>%
  filter(y.coord < 3989)

####### Section 2 ########

two.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 2919 | x.coord > 26809) %>%
  filter(y.coord > 3989)

####### Section 3 ########

three.dat <- data %>%
  select(2,3,5) %>%
  filter((x.coord < 5041 & x.coord > 2919) |
           (x.coord > 23889 & x.coord < 26809)) %>%
  filter(y.coord < 3989)

####### Section 4 ########

four.dat <- data %>%
  select(2,3,5) %>%
  filter((x.coord < 5041 & x.coord > 2919) |
           (x.coord > 23889 & x.coord < 26809)) %>%
  filter(y.coord > 3989)

####### Section 5 ########

five.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 20441 & x.coord > 7699) %>%
  filter(x.coord > 18049 | x.coord < 10091) %>%
  filter(y.coord < 8241 & y.coord > 4789)

####### Section 6 ########

six.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 15141 & x.coord > 11679) %>%
  filter(y.coord < 3729)

####### Section 7 ########

seven.dat <- data %>%
  select(2,3,5) %>%
  filter(x.coord < 15141 & x.coord > 11679) %>%
  filter(y.coord > 3729)

####### Data Manipulation ########

a <- one.dat$Hardness
b <- two.dat$Hardness
c <- three.dat$Hardness
d <- four.dat$Hardness
e <- five.dat$Hardness
f <- six.dat$Hardness
g <- seven.dat$Hardness

# Fusion Zone Comparison

n.f = length(f) # number of samples in region six

n.g = length(g) # number of samples in region seven

x_bar.f = (1/n.f)*sum(f) # average hardness in region six

x_bar.g = (1/n.g)*sum(g) # average hardness in region seven

s_x2.f <- (1/(n.f-1))*sum((f - x_bar.f)^2) # this is the sample variance of the data set

s_x.f <- sqrt(s_x2.f) # this is the standard deviation of the data set

s_x2.g <- (1/(n.g-1))*sum((g - x_bar.g)^2) # this is the sample variance of the data set

s_x.g <- sqrt(s_x2.g) # this is the standard deviation of the data set

f.part <- ((s_x.f^2)/n.f)

g.part <- ((s_x.g^2)/n.g)

nu <- (f.part + g.part) /
  (((f.part^2)/(n.f-1)) + ((g.part^2)/(n.g-1)))# degrees of freedom

new_nu <- round(nu)

t_crit = 1.963

t_stat = (x_bar.f - x_bar.g) /
  sqrt(f.part + g.part)

if(t_stat > t_crit | t_stat < (-1)*t_crit) {
  print("rejects the null hypothesis")
} else print("failed to reject the null hypothesis")

results1 <- tibble(n.f,n.g,x_bar.f,x_bar.g,s_x.f,s_x.g,nu,new_nu,t_crit,t_stat)

# Average hardness of each area

n.a = length(a) # number of samples in region six
n.b = length(b) # number of samples in region seven
n.c = length(c) # number of samples in region six
n.d = length(d) # number of samples in region seven
n.e = length(e) # number of samples in region six

x_bar.a = (1/n.a)*sum(a) # average hardness in region six
x_bar.b = (1/n.b)*sum(b) # average hardness in region six
x_bar.c = (1/n.c)*sum(c) # average hardness in region seven
x_bar.d = (1/n.d)*sum(d) # average hardness in region six
x_bar.e = (1/n.e)*sum(e) # average hardness in region seven

s_x2.a <- (1/(n.a-1))*sum((a - x_bar.a)^2) # this is the sample variance of the data set
s_x2.b <- (1/(n.b-1))*sum((b - x_bar.b)^2) # this is the sample variance of the data set
s_x2.c <- (1/(n.c-1))*sum((c - x_bar.c)^2) # this is the sample variance of the data set
s_x2.d <- (1/(n.d-1))*sum((d - x_bar.d)^2) # this is the sample variance of the data set
s_x2.e <- (1/(n.e-1))*sum((e - x_bar.e)^2) # this is the sample variance of the data set

s_x.a <- sqrt(s_x2.a) # this is the standard deviation of the data set
s_x.b <- sqrt(s_x2.b) # this is the standard deviation of the data set
s_x.c <- sqrt(s_x2.c) # this is the standard deviation of the data set
s_x.d <- sqrt(s_x2.d) # this is the standard deviation of the data set
s_x.e <- sqrt(s_x2.e) # this is the standard deviation of the data set

Sx_bar.a <- s_x.a / sqrt(n.a) # standard deviation of means
Sx_bar.b <- s_x.b / sqrt(n.b) # standard deviation of means
Sx_bar.c <- s_x.c / sqrt(n.c) # standard deviation of means
Sx_bar.d <- s_x.d / sqrt(n.d) # standard deviation of means
Sx_bar.e <- s_x.e / sqrt(n.e) # standard deviation of means
Sx_bar.f <- s_x.f / sqrt(n.f) # standard deviation of means
Sx_bar.g <- s_x.g / sqrt(n.g) # standard deviation of means

HV <- c(x_bar.a,x_bar.b,x_bar.c,x_bar.d,x_bar.e,x_bar.f,x_bar.g)
Sx_bar <- c(Sx_bar.a,Sx_bar.b,Sx_bar.c,Sx_bar.d,Sx_bar.e,Sx_bar.f,Sx_bar.g)
N <- c(n.a,n.b,n.c,n.d,n.e,n.f,n.g)

results2 <- tibble(HV,Sx_bar,N)

write.csv(results1, file = "FZ_HV_Comp.csv")
write.csv(results2, file = "Avg_HV.csv")