# R code for statistics courses by Xinhai Li
#============================================================================================================================

# lecture 1. Probability distribution
#############################################################################################################################
#' basic calculations and plotting probability distribution functions
#' @description R codes for the statistics course about R and probability distribution
#' @author Xinhai Li (Xinhai_li_edu@126.com)
#############################################################################################################################

# Use R for basic calculation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
9+2
6+2^2
(3+4)^2
sqrt(2)
log(2)

seq(1, 4, by=.5)
v1 = c(7,6,5,4,3,2,1)
v1

v2 = c(11,10,9,8,7,6,5)
v3 = v1 + v2
v3

max(v3); min(v3)
length(v3)
mean(v3)
sd(v3)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Functions to show attribute of R object
#ooooooooooooooooooooooooooooooooooooooooooooo
x = c('a','b','c','d')
mode(x) # "character"
x = as.factor(x)
mode(x)
class(x)  # “factor"
attributes(x)
#ooooooooooooooooooooooooooooooooooooooooooooo


# the following code should be adjusted
##############################################
getwd()
setwd(“C:/models”)
rm(mydata)
source(“function1.R”)
##############################################



ls()
q()

# Getting help
? t.test
help(t.test)
example(t.test)
t.test

apropos("test")
help.search("test")


# Importing and exporting Data
# IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
x <- read.csv("D:/path/filename.csv", header = TRUE)
x <- read.table("filename.txt", sep = " ", header = T)
x <- read.delim("filename.txt", sep = "\t" , header = T)
write.table(x, file = "x.txt", sep="\t",  col.names = F)
write.csv  (x, file = "D:/path/x.csv",  col.names = F)

save(mydata, file = "mydata.RData")
load("mydata.RData")
save.image("all_data_and_others.RData")
# IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII





# Probability distribution
# PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
# PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

# Define a R function for a binomial distribution
p.x <- function(n=10, p=1/6, x)
       {gamma(n+1)/gamma(x+1)/gamma(n-x+1)*p^x*(1-p)^(n-x)}
p.x(x=5)

barplot(table(rbinom(100000, 6, 1/6)))


binomial.PDF = dbinom(0:10, 10, 0.5)
plot(binomial.PDF * 10, type = 'l', ylim = c(0, 4.5),
     xlab = "Number of success", ylab = "Frequency",
     main = paste('dbinom (0:10, 10, P) # P = 0.1, 0.2, ..., 0.9', sep = " "))


for (i in seq(0.1, 0.9, by = 0.1)) {
  binomial.PDF = dbinom(0:10, 10, i)
  lines(binomial.PDF * 10, type = 'l',
        col = rainbow(9)[i*10], lwd=2)

  legend(-0.3 + 11 * i, 4.5,
         paste('p =', i, sep =  " "),
         text.col = rainbow(9)[i * 10],
         box.lty = 0, cex = 0.8)
}


# Poisson distribution
x1 = rep(0, 242)
x2 = rep(1, 148)
x3 = rep(2, 49)
x4 = rep(3, 15)
x5 = rep(4, 4)
x = c(x1, x2, x3, x4, x5)
sum(x) # 307
mean(x) # 0.67
var(x) # 0.738

options(digits = 3)
dpois(0:4, 0.67)

barplot(table(rpois(1000,1.25)))

# Binomial distribution vs. Poisson distribution
B = dbinom(0:150, 150, 0.008) # PDF of binomial distribution
P = dpois(0:150, 1.25) # PDF of Poisson distribution

plot(0:150, B, type = "l", xlim=c(0, 10), xlab = "X", ylab = "Probability")
lines(0:150, P, col = 2)
legend("topright", c("Binomial distribution", "Poisson distribution"), lty = 1, col = c(1, 2))

lambda <- 1
poisson.d <- dpois(seq(1,20), lambda)
plot(poisson.d, xlab = 'X', ylab='p', type='l')

     for (lambda in 1:10){
     poisson.d <- dpois(seq(1,20), lambda)
     lines(poisson.d, type = 'l',
     col = rainbow(10)[lambda])
     legend(lambda, 0.4-lambda/50,
     paste('lambda =', lambda, sep =' '),
     text.col = rainbow(10)[lambda],
     box.lty = 0, cex = 0.8)}

size  <- seq(1, 10, len = 10); prob = seq(0.01, 1, len = 20)
p.nb <- matrix(apply(expand.grid(size, prob), 1,
                     function(z) dnbinom(5, size = z[1], prob = z[2])),   # x=5
               nrow = length(size), ncol = length(prob))
colnames(p.nb) <- round(prob, 2); rownames(p.nb) <- size
image(size, prob, p.nb, col = terrain.colors(12), xlab = 'Size', ylab = 'p')
contour(size, prob, p.nb, add = TRUE)


par(mfrow = c(2,1), mar = c(4,4,2,2))
plot(c(1,1,3,3), c(0,0.5,0.5,0), type='l', xlim=c(0,4), ylim=c(0,1), xlab='x', ylab='p', cex=2, xaxt='n')
mtext(c('a','b'), side = 1,at = c(1,3)); legend(0.3, 0.6, '1/(b-a)', box.lty = 0, cex = 1, bty='n')
polygon(c(1,1, 3,3), c(0,0.5,0.5,0), col='grey')

plot(c(1,3),c(0,1), type='l', xlim=c(0,4), ylim=c(0,1), xlab='x', ylab='p', cex=2, xaxt='n')
polygon(c(1,3,3), c(0,0,1), col='grey'); mtext(c('a','b'), side=1, at = c(1,3))


# Chi square distribution
plot(dchisq(seq(0.1, 10, by=0.1), df = 1), type='l',
     xlab = 'x', xlim = c(0,10))

# F distribution
X = seq(0.1, 3, length=30); Y = df(X, 1,1)
par(mfrow=c(2,2))

plot(X, Y, type='n', xlab = 'X', ylab = 'P', xlim=c(0,2), main="DF1=1, DF2=1:10")
for (i in 1:10)  lines(X, df(X, 1, i), col=rainbow(10)[i])

plot(X, Y, type='n', xlab = 'X', ylab = 'P', xlim=c(0,2), main="DF1=1:10, DF2=1")
for (i in 1:10) lines(X, df(X, i, 1), col=rainbow(10)[i])

plot(X, Y, type='n', xlab = 'X', ylab = 'P', xlim=c(0,2), main="DF1=10, DF2=1:10")
for (i in 1:10) lines(X, df(X, 10, i), col=rainbow(10)[i])

plot(X, Y, type='n', xlab = 'X', ylab = 'P', xlim=c(0,2), main="DF1=1:10, DF2=10")
for (i in 1:10)  lines(X, df(X, i, 10), col=rainbow(10)[i])

# Normal distribution
dnorm(seq( -3, 3,      by = 0.1), mean = 0, sd = 1)
pnorm(seq( -3, 3,      by = 0.1), mean = 0, sd = 1)
qnorm(seq( 0.1, 0.9, by = 0.1), mean = 0, sd = 1)
rnorm(10, mean = 0, sd = 1)

x = 2
N = 10
p = 0.5
q = 0.05
n = 100
dbinom(x, N, p) # Binomial distribution
pbinom(x, N, p)
qbinom(q, N, p)
rbinom(n, N, p)
# PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
# PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


# Assignment 1
obs = c(7,5,6,6,7,5,3,4,5,8,2,4,5,6,7,6,4,5,9,3,6,4)

hist(obs, freq = F)
mean = mean(obs)
SD     = sd(obs)

x = seq(min(obs), max(obs), by = .1)
norm = dnorm(x, mean = mean, sd = SD)
lines(x, norm, type = 'l', col = 'red')
abline(v=c(qnorm(.025, mean, SD), qnorm(.975, mean, SD)), col = 'brown')
abline(v=c(mean - 2*SD, mean + 2*SD), col = 'brown')













