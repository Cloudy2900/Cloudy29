######## This is the methods to calculate time series logret and discret return for daily, weekly monthly and yearly ##########
## For more information, you may visit https://www.coursera.org/learn/financial-risk-management-with-r/ ####


#### Retrieving Data from FRED ####
install.packages("quantmod")
library(quantmod)
wilsh <- getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
head(wilsh,3)
tail(wilsh,3)

#### Calculating Daily Returns ####
# 1. daily log returns # 
# daily log return sum up will equal to period return, but no equal to weighted return #
logret <- diff(log(wilsh))
head(logret,3)

logret <- diff(log(wilsh))[-1] # remove the first line
round(head(logret,3),6)        # round the number to 6 decimal only

# 2. daily discrete returns #
# daily log return sum up will equal to weighted return, but not time series bound #
ret <- exp(logret) - 1         # calculate discrete return
round(tail(ret,3),6)

#### Calculating Longer Returns ####
# Calculating longer horizon log return
logret.w <- apply.weekly(logret,sum)
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)
# Calculating longer horizon discrete return
ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1

round(tail(ret.y,3),6)
