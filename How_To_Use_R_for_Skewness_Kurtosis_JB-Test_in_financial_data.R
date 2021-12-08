######## This is the methods for using moments package to carry out Skewness test, Kurtosis test and Jarque-Bera test in R ##########
## For more information, you may visit https://www.coursera.org/learn/financial-risk-management-with-r/ ####


# Load Financial Data
#install.packages("quantmod")
library(quantmod)
wilsh <- getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"

# Calculate daily log returns
logret <- diff(log(wilsh))[-1]

# install moment package for Skewness, Kurtosis and Jarque-Bera Test
#install.packages("moments")

#### Skewness, Kurtosis and JB Test ####
# Skewness test: The coefficient of skewness is 0 (sysmetric), -ve(left-skewed), +ve(right-skewed)
library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)

# Kurtosis: test heavy-tail or thin-tail, 3 (normal), <3 (thin-tailed), >3 (heavy-tailed)
library(moments)
rvec <- as.vector(logret)
round(kurtosis(rvec),2)

# Jarque-Bera test: test for normality 
library(moments)
rvec <- as.vector(logret)
jarque.test(rvec)

#### Estimate parameters of the scaled student-t distribution  ####
#### To Test if daily log returns are normally distributted    ####

## After loading data
# Estimate parameters of scaled student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)

# using the estimated parameters, estimate VaR and ES
alpha <- 0.05
RNGkind(sample.kind='Rounding')
# install.packages("metRology")
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

####  Estimate VaR and ES for Multi-day Horizon  ####
# in example, suppose we want a 10-day return simulation, there are 3 method

# Method 1: Simulate from estimated student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)

alpha <- 0.05
RNGkind(sample.kind='Rounding')
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])

# Method 2: Simulate from empirical distribution with IID Draw (Independent and identically distributed)
alpha <- 0.05
RNGkind(sample.kind='Rounding')
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha) 
ES <- mean(rvec[rvec<VaR])

# Method 3: Simulate from empirical distribution with block draw (continuously, consecutive drawing for 10 days)
alpha <- 0.05
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])

