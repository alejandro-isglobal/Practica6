
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
xbar <- mean(x)
sigma <- 5 #sigma^2=25
n <- length(x)
m <- qnorm(0.95)*sigma/sqrt(n)
c(xbar-m, xbar+m)

#install.packages("BSDA")
library(BSDA)
z.test(x, sigma.x = 5, conf.level = 0.9)
c(xbar-m, xbar+m)

zc <- qnorm(0.90)
zc

mu0<- 500
zobs <- (xbar - mu0)/(sigma/sqrt(n))
zobs

z.test(x, conf.level = 0.9, 
       alternative="greater",
       sigma.x=sigma,
       mu=mu0)
pvalor <- 1-pnorm(zobs) #cola suprior
pvalor


####
xbar <- mean(x)
s <- sd(x)
n <- length(x)
m <- qt(0.99+0.005, n-1)*s/sqrt(n)

t.test(x, conf.level = 0.99)
c(xbar-m, xbar+m)

# H0: mu = mu0, donde mu0=500
# H1: mu > mu0  (cola superior)

mu0 <- 500
t.test(x, mu= mu0, alternative = "greater",
       conf.level = 0.99)


#tc
qt(0.99, n-1)
tobs <- (xbar -mu0)/(s/sqrt(n))
tobs
pvalor <- 1- pt(tobs, n-1)
pvalor

###
#install.packages("EnvStats")
library(EnvStats)

varTest(x)
icvar <- c(s^2*(n-1)/qchisq(0.975, n-1), 
  s^2*(n-1)/qchisq(0.025, n-1))

icsigma <- sqrt(icvar)
icsigma

varTest(x, conf.level = 0.95, 
        sigma.squared = 5^2, 
        alternative = "two.sided")
###
prop.test(136, 400, correct=FALSE,
          p = 0.3, alternative = "less")
