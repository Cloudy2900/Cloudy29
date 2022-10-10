Time series handling and risk management with statistical approach
================
Ragdoll99
2022-09-26

### Time series handling and risk management with statistical approach

  
This project is to showcase time series data handling in R and financial
risk management with statistical approach for estimating share price
value-at-risk and Expected shortfall.  

``` r
library(knitr)
library(quantmod)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)
```

  
Firstly, we need to extract an example of share price.  
  
We will use microsoft share price as example.  
  
Time series data could be directly extracted from Yahoo finance
website.  

``` r
msft <- getSymbols("MSFT",src="yahoo",auto.assign=FALSE)
msft <- data.frame(msft)
head(msft, 5)
```

    ##            MSFT.Open MSFT.High MSFT.Low MSFT.Close MSFT.Volume MSFT.Adjusted
    ## 2007-01-03     29.91     30.25    29.40      29.86    76935100      21.68802
    ## 2007-01-04     29.70     29.97    29.44      29.81    45774500      21.65171
    ## 2007-01-05     29.63     29.75    29.45      29.64    44607200      21.52822
    ## 2007-01-08     29.65     30.10    29.53      29.93    50220200      21.73887
    ## 2007-01-09     30.00     30.18    29.73      29.96    44636600      21.76066

  
Before proceed further, we will need to extract the relevant data.  
  
Here, we are keeping only the closing price for each day.  

``` r
msft_close <- dplyr::select(msft, "MSFT.Close")
tail(msft_close, 5)
```

    ##            MSFT.Close
    ## 2022-10-03     240.74
    ## 2022-10-04     248.88
    ## 2022-10-05     249.20
    ## 2022-10-06     246.79
    ## 2022-10-07     234.24

  

``` r
qplot(data = msft_close,y = MSFT.Close)+geom_line(color='darkgreen')
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Calculate log return for each day.

In time series analysis, log-return, instead of simply return are often
used because of is time-additive/ time-consistence.  
  
For instance, for daily log-return, the sum of daily log-return over a
period, e.g. a month, will hence equal to monthly log-return.  
  
Because of the time-additive attributes, it is widely used by finance
professionals in risk management.  
  
log return is calculated by taking the natural log of the ending value
divided by the beginning value  
  
Here is the daily log-return for tail of the dataset:  

``` r
msft_close <- as.matrix(msft_close)
logret <- diff(log(msft_close))
round(tail(logret,5),6)  
```

    ##            MSFT.Close
    ## 2022-10-03   0.033108
    ## 2022-10-04   0.033253
    ## 2022-10-05   0.001285
    ## 2022-10-06  -0.009718
    ## 2022-10-07  -0.052191

  
As contrast, below are the daily discrete returns:  

``` r
ret <- exp(logret) - 1         # calculate discrete return
round(tail(ret,5),6)
```

    ##            MSFT.Close
    ## 2022-10-03   0.033663
    ## 2022-10-04   0.033812
    ## 2022-10-05   0.001286
    ## 2022-10-06  -0.009671
    ## 2022-10-07  -0.050853

  
We can also calculating longer horizon log-returns and discrete
returns  
Here is the Weekly, monthly, quarterly and yearly log-returns  

``` r
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
```

  
Let’s compare the monthly return and monthly log-return.  

``` r
logret_vs_ret <- cbind(round(tail(logret.m,5),6), round(tail(ret.m,5),6))
colnames(logret_vs_ret) <- c("log-return", "discrete-return")  
logret_vs_ret
```

    ##            log-return discrete-return
    ## 2022-06-30  -0.056910       -0.055321
    ## 2022-07-29   0.089015        0.093097
    ## 2022-08-31  -0.071109       -0.068640
    ## 2022-09-30  -0.115710       -0.109267
    ## 2022-10-07   0.005737        0.005754

### Value at Risk (VaR) and Expected shortfall (ES)

Let’s ASSUME that our log-return follow a normal distribution,  
we can now calculate the VaR and Expected shortfall based on
log-return  
  
before we proceed, we would need to calculate two terms, which are the
mean and standard deviation of log-return  

``` r
mu <- mean(logret)
sig <- sd(logret)
cat("mean: ", round(mu,6), " SD:", round(sig,6))
```

    ## mean:  0.000519  SD: 0.017862

  
Value at risk (VaR) is a statistic that quantifies the extent of
possible financial losses within a firm, portfolio, or position over a
specific time frame.  
  
$Value\ at\ Risk = vm (v_{i} / v_{(i - 1)})$  
M = the number of days from which historical data is taken  
vi = the number of variables on the day i.  
  

Let’s calculate the VaR of normal distribution  
We use 5% quartile of the probability density function  
.: let, alpha = 0.05 and time period = 1 day  
  
Assume that we invested U\$10,000 into the Microsoft  
What is the 1 day VaR at 95% confidence level of this portfolio?  

``` r
var <- qnorm(0.05,mu,sig)
HFvar <- 10000 * (exp(var)-1 )  # in US dollars
round(HFvar, 2)
```

    ## [1] -284.5

  
What doest this mean? It simply mean that over 1 day, your investment is
not likely to lose more than U\$284.12 dollar at 95% of confidence
level.  

### Expected shortfall 

Expected shortfall is the expected return given that the return is worse
than the associate VaR.  
In other word, it is the average of return(loss) in the situtaion where
the stock price went lower than VaR.  
  
Let’s try to calculate ES based on the same probability level
(1-alpha)  

``` r
es <- mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05
ES <- 10000 * ( exp(es)-1 ) # in millions of dollars
round(es, 6)
```

    ## [1] -0.036326

  
If MSFT share price fall more than the VaR, our investment portfolio is
expected to lose 3.6% which is around U\$360 dollar (follow normal
distribution)  

### Exploratory Data Analysis

  
However, what would happen if the return is not in normal
distribution?  
To answer this, we would need to first explore whether our log-return is
in normal distribution  
  
Density plot for log-return:  

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->  
From the density plot, noted that log-return follow a symmetric
distribution.  
  
Dot plot for log-return:  

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->  
By visual exploration, we notice that log-return are symmetric.  
However, it is still hard to tell if the log-return follow normal
distribution.  
Without clear picture whether Microsoft’s log-return follow normal
distribution,we would not be able to simulate associated risk
adequately.  
  
  
Next, let’s compare the log-return distribution with a normal
distribution by generating a random normal distribution with the same
mean and standard deviation of the sample log-return.  
  
Generating a random norm dist density plot  

``` r
x <- rnorm(length(logret), mu, sig)
 
logret_df <- logret_df %>%
  add_column(Norm.Distribution = x)
head(logret_df)
```

    ##              MSFT.Close Norm.Distribution
    ## 2007-01-04 -0.001675951      -0.022017693
    ## 2007-01-05 -0.005719107      -0.020175572
    ## 2007-01-08  0.009736555      -0.002237785
    ## 2007-01-09  0.001001803      -0.002741365
    ## 2007-01-10 -0.010063789      -0.014898080
    ## 2007-01-11  0.034463350      -0.011491026

``` r
# combine both set of time-series for plotting
st_logret_df <- stack(logret_df)
# head(st_logret_df)
ggplot(data = st_logret_df, aes(x=values,  fill=ind)) + geom_density(alpha=0.3)
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->  
From the visual comparison, it turned up that sample (actual log return)
are having higher peak and longer tail than random normal
distribution.  

Before proceed further, let’s do some check for the sample log-return  

### Skewness check

We use skewness test to check if our log-return follow normal
distribution  

$\tilde{\mu }_{3}=\frac{\sum_{i}^{N}\left(X_{i}-\bar{X}\right)^{3}}{(N-1)*\sigma^{3}}$  

In simple term, we can conclude that if the coefficient of skewness is
0, data is symmetric, else it is left-skewed or right-skewed  
0(symmetric), -ve(left-skewed), +ve(right-skewed)  

``` r
library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)
```

    ## [1] 0

  
From the coefficient of skewness, we noted that our logreturn is
symmetric, which is tallied with visual exploration.  
  
Next we use Kurtosis test to check if the tail of log-return is
thin-tailed or heavy-tailed  

### Kurtosis test

Kurtosis test is another method to check if sample time-series is normal
distributed  

$\mathrm{Kurt} =\frac{\mu_{4}}{\sigma^{4}}$  

In general estimation, if the test score is 3 (normal tailed), \<3
(thin-tailed), \>3 (heavy-tailed)  

``` r
round(kurtosis(rvec),2)
```

    ## [1] 12.11

  
Our result shown that sample log-return have a kurtosis greater than 3,
which implied that it is heavy-tailed and not following normal
distribution.  
  
Lastly, we will try one more test to check the distribution.  

### Jarque-Bera test

Jarque-Bera test: it is a test for normality. It is used for determining
whether a given dataset has skewness and kurtosis that matches
normality.  

Jarque Bera is formulated as follows:  
  
$JB = \frac{n}{6} \left( S^{2} + \frac{1}{4} (K-3)^{2} \right)$  
where, n = number of observations in a sample k = number of regressors S
= skewness of the sample  
  
With the resulted p value, we can determine if the sample log-return
distribution followed a normal distribution.  
(p-value \< alpha, reject null hypothesis and conclude that sample not
following normal distribution and vice versa.)  
  

``` r
jarque.test(rvec)
```

    ## 
    ##  Jarque-Bera Normality Test
    ## 
    ## data:  rvec
    ## JB = 13718, p-value < 2.2e-16
    ## alternative hypothesis: greater

  
Noted that the p-value is less than alpha (0.05, which is 5%), hence we
rejected that normality in log-return  
  
From above 3 test, we can conclude that the sample log-return symmetric
yet heavy-tailed.  
  
  

### Student t-distribution

  
Now we noted that our stock price’s log return, or simply the return do
not follow normal distribution’s kurtorsis.  
To tackle this, we describe the model with another variable terms –
error  
  
Recall that when we estimate the VaR from actual data, we are using two
variable in OLS model, which are mean and sd, to get the error terms, we
use max-likelihood estimation (MLE) for a t-distribution (log-return is
sysmetric and heavy-tailed)  
  
Without going into too much math background of MLE (involved setting all
other terms as constant and using chain-rule derivative to find the peak
or 0 slope), let’s fit our sample log-return to a t-dist and calculate
MLE for our three variables terms  
  

In R, there is “fitdistr” function which is very handy to fit our
log-return into t-dist and get our variables estimate  

here are the results:  

``` r
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)
```

    ##        m        s       df 
    ## 0.000657 0.011160 3.038938

  
Next, we can use newly three variable to generate a series of
simulation:  

### VaR and ES in T-distribution Simulation

$t=\frac{m-\mu}{s / \sqrt{n}}$  
  
Now, we are ready to simulate the VaR and expected shortfall in
T-distribution.  
Let’s set simulation count of 100,000 times, to derive a new VaR and
Expected shortfall following a T-distribution.  

``` r
alpha <- 0.05
RNGkind(sample.kind='Rounding')
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
t_VaR <- quantile(rvec,alpha)
t_ES <- mean(rvec[rvec<t_VaR])
```

  

Let’s check the VaR and Expected Shortfall in T distribution  

``` r
cat("VaR is ", round(t_VaR,6), " and Expected Shortfall is ", round(t_ES,6))
```

    ## VaR is  -0.025339  and Expected Shortfall is  -0.04153

### VaR and ES in normal distribution Simulation

On the other hand, we can use the original mu and sig to get the same
count of simulation under normal distribution,  
so that we can compare the result:  

``` r
# Simulation 1 : Assuming that daily log returns are normally distributed
mu <- mean(logret)
sig <- sd(logret)
RNGkind(sample.kind='Rounding')
set.seed(123789)
rvec1 <- rnorm(100000,mu,sig)
n_VaR <- quantile(rvec1,0.05)
n_ES <- mean(rvec1[rvec1<n_VaR])

cat("VaR is ", round(n_VaR,6), " and Expected Shortfall is ", round(n_ES,6))
```

    ## VaR is  -0.029014  and Expected Shortfall is  -0.03668

### VaR and ES in actual historial log return

``` r
a_VaR <- quantile(logret,alpha)
a_ES <- mean(logret[logret<a_VaR])

cat("VaR is ", round(a_VaR,6), " and Expected Shortfall is ", round(a_ES,6))
```

    ## VaR is  -0.027301  and Expected Shortfall is  -0.04199

  
Let’s put them into a table for comparison:  

``` r
simulation <- c("actual", "t-distribution", "normal-distribution")
Value.at.Risk <- c(a_VaR, t_VaR, n_VaR)
Expected.shortfall <- c(a_ES, t_ES, n_ES)

c.df <- data.frame(simulation, Value.at.Risk, Expected.shortfall)
c.df
```

    ##            simulation Value.at.Risk Expected.shortfall
    ## 1              actual   -0.02730072        -0.04198984
    ## 2      t-distribution   -0.02533877        -0.04152958
    ## 3 normal-distribution   -0.02901372        -0.03668049

  
Noted that T-distribution simulation VaR and ES are closer to actual
data.  
At this point of time, We would take the **t-distribution** simulation
in order not to underestimate risk.  

### Serial Correlation and Cluster Volatility

okay now, we still have two more items to check.  
In previous simulation, we are simply assume that log-return follow
random t-distribution.  
However, it might not be true for stock price, especially with
involvement of market sentiment and market/industry/business cycles .  
Hence, there is a need to check whether there is serial correlation and
volatility cluster in the sample time-series.  
  
Without answering this question, our assumption would be log-return are
simply following random return in a t-distribution setup.  
In other words, if we can prove that there is no serial correlation and
volatility cluster, then we can conclude that Microsoft share price
follow random-walk.  
  
  
In order to proceed, firstly, we will look at the serial correlation.  
In simple term, it is to check whether an above-average return will be
followed by another above-average return.  

### Testing for serial correlation

We use autocorrelation coefficient test for our sample data.

$ρ_{k}=\frac{γ_{k}}{γ_{0}}$  
where γk = cov(yi, yi+k) and yi+k is the lag of period i  

The handy part is that there is an acf function which we can directly
apply in r:  

``` r
acf(logret)
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->  
The blue dash line are the 95% of confidence level  
ACF showing less evidence that log-return presented with some level of
serial correlation.

### Testing for volatility cluster

Now, let’s check for volatility cluster existed in log-return.  
Because volatility cluster referred to both increase and decrease of
shares price, hence we are using absolute value for acf function.  

$ρ_{|k|}=\frac{|γ_{k}|}{|γ_{0|}}$  

``` r
acf( abs(logret) )
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

  
Noted from the visual analytics, it is a different story for the
volatility clusters.  
There is evidence for strong presence of volatility clusters.  

Perhaps we should try to check if indeed it was due to volatility
clustering.  
We will now re-ordering the log-return randomly (like shuffling data
randomly) and check if the presence of auto-correlation are really due
to volatility clustering.  
  

``` r
shuffled_logret= logret[sample(1:nrow(logret)), ]
acf( abs(shuffled_logret) )
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->  
Clearly, by shuffling the log-return, acf function do not indicate the
volatility clustering.  
  
Now that we can conclude that there is volatility clusters in our
sample, which mean, high volatility tend to followed by high volatility
and  
low volatility tend to followed by low volatility.  
  
As we are now handling more than a decade time-series data, it would be
difficult to predict the risk associated with the underlying assets in
the presence of volatility clusters.  

We would need to deal with the volatility clusters for better risk
management  

One of the method is to apply the volatility model for the time-series  

### GARCH - Generalized AutoRegressive Conditional Heteroskedasticity

Generalized AutoRegressive Conditional Heteroskedasticity (GARCH) is a
statistical model used in analyzing time-series data  
where the variance error is believed to be serially autocorrelated.  

GARCH models are used when the variance of the error term is not
constant. That is, the error term is heteroskedastic.  
Heteroskedasticity describes the irregular pattern of variation of an
error term, or variable, in a statistical model.  

We will use GARCH distribution equation to a re-scaled t distribution
GARCH(1,1)-t model:

$r_{t}=a_{0}+{\sqrt{h_{t}\varepsilon_{t}}}$ (mean equation)  
$h_{t}=a_{0}+\beta_{1}h_{t-1}+a_{1}\varepsilon^{2}_{t-1}$ (variance
equation)  
$\varepsilon_{t}$ \~ $t(v) / \sqrt{v/v-2}$ (distribution equation)  

Notation:  

$r_{t}$ is the return series with time varying volatility  
$a_{0}$ is its expected return (typically close to 0)  
$\sqrt{h_{t}\varepsilon_{t}}$ is the unexpected return  
$h_{t}$ is the predicatable variance, changing over time  
$\varepsilon_{t}$ is the normally distributed, with mean 0 and variance
1  
  
To apply the GARCH model in R, there is a readily function: rugarch  
In our case, I use t-dist “std” instead of normal-distribution “norm” in
distribution.model argument:  

``` r
library(rugarch)
```

``` r
garch.t <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch.t <- ugarchfit(spec = garch.t, data = logret)    # estimated parameters are in fit.garch@fit$coef
fit.garch.t
```

    ## 
    ## *---------------------------------*
    ## *          GARCH Model Fit        *
    ## *---------------------------------*
    ## 
    ## Conditional Variance Dynamics    
    ## -----------------------------------
    ## GARCH Model  : sGARCH(1,1)
    ## Mean Model   : ARFIMA(0,0,0)
    ## Distribution : std 
    ## 
    ## Optimal Parameters
    ## ------------------------------------
    ##         Estimate  Std. Error  t value Pr(>|t|)
    ## mu      0.000933    0.000195   4.7837 0.000002
    ## omega   0.000006    0.000003   2.4044 0.016197
    ## alpha1  0.096765    0.012161   7.9570 0.000000
    ## beta1   0.888961    0.013173  67.4842 0.000000
    ## shape   4.587040    0.353121  12.9900 0.000000
    ## 
    ## Robust Standard Errors:
    ##         Estimate  Std. Error  t value Pr(>|t|)
    ## mu      0.000933    0.000169  5.51980  0.00000
    ## omega   0.000006    0.000006  0.96268  0.33571
    ## alpha1  0.096765    0.017826  5.42826  0.00000
    ## beta1   0.888961    0.022228 39.99211  0.00000
    ## shape   4.587040    0.505433  9.07547  0.00000
    ## 
    ## LogLikelihood : 11088.99 
    ## 
    ## Information Criteria
    ## ------------------------------------
    ##                     
    ## Akaike       -5.5853
    ## Bayes        -5.5774
    ## Shibata      -5.5853
    ## Hannan-Quinn -5.5825
    ## 
    ## Weighted Ljung-Box Test on Standardized Residuals
    ## ------------------------------------
    ##                         statistic p-value
    ## Lag[1]                      6.156 0.01310
    ## Lag[2*(p+q)+(p+q)-1][2]     6.353 0.01772
    ## Lag[4*(p+q)+(p+q)-1][5]     7.803 0.03298
    ## d.o.f=0
    ## H0 : No serial correlation
    ## 
    ## Weighted Ljung-Box Test on Standardized Squared Residuals
    ## ------------------------------------
    ##                         statistic p-value
    ## Lag[1]                     0.0318  0.8585
    ## Lag[2*(p+q)+(p+q)-1][5]    0.8968  0.8826
    ## Lag[4*(p+q)+(p+q)-1][9]    1.8696  0.9200
    ## d.o.f=2
    ## 
    ## Weighted ARCH LM Tests
    ## ------------------------------------
    ##             Statistic Shape Scale P-Value
    ## ARCH Lag[3]     1.122 0.500 2.000  0.2894
    ## ARCH Lag[5]     1.631 1.440 1.667  0.5585
    ## ARCH Lag[7]     1.906 2.315 1.543  0.7371
    ## 
    ## Nyblom stability test
    ## ------------------------------------
    ## Joint Statistic:  3.1513
    ## Individual Statistics:             
    ## mu     0.6122
    ## omega  0.3995
    ## alpha1 0.5277
    ## beta1  0.5636
    ## shape  0.6916
    ## 
    ## Asymptotic Critical Values (10% 5% 1%)
    ## Joint Statistic:          1.28 1.47 1.88
    ## Individual Statistic:     0.35 0.47 0.75
    ## 
    ## Sign Bias Test
    ## ------------------------------------
    ##                    t-value   prob sig
    ## Sign Bias          0.35292 0.7242    
    ## Negative Sign Bias 1.23913 0.2154    
    ## Positive Sign Bias 0.04274 0.9659    
    ## Joint Effect       1.61231 0.6566    
    ## 
    ## 
    ## Adjusted Pearson Goodness-of-Fit Test:
    ## ------------------------------------
    ##   group statistic p-value(g-1)
    ## 1    20     25.15       0.1557
    ## 2    30     36.28       0.1655
    ## 3    40     35.73       0.6200
    ## 4    50     41.90       0.7540
    ## 
    ## 
    ## Elapsed time : 0.4880481

  
Next, save fitted values:  

``` r
save1 <- cbind(logret, fit.garch.t@fit$sigma, fit.garch.t@fit$z)
save1 <- data.frame(save1)
names(save1) <- c( "logret", "s", "z" )
parm1<- fit.garch.t@fit$coef
```

  
The variable save1 contains 3 columns of data:

logret is the daily log return  
s sigma is the fitted value of $\sqrt{h_{t}}$  
z is the fitted values of $\varepsilon_{t}$  

The variable parm1 contains the estimated parameters of the GARCH(1,1)
model  
  
  
Next, let’s examine acf of “z” column to check if GARCH model has
captured volatility clustering in data  

``` r
acf(save1$z)
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->  

``` r
acf(abs(save1$z))
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->  

we can now calculate VaR and ES from GARCH model by bootstrapping from
the fitted “E” (standard residual)  

``` r
set.seed(123789)
boot.garch <- ugarchboot(fit.garch.t,
                         method=c("Partial"),  # ignore parameter uncertainty
                         sampling="raw",                 # draw from standardized residuals
                         n.ahead=1,                      # 1-day ahead
                         n.bootpred=100000,              # number of simulated outcomes
                         solver="solnp")
# save simulated outcomes
rvec <- boot.garch@fseries
head(rvec)
```

    ##              [,1]
    ## [1,] -0.007178270
    ## [2,]  0.015661424
    ## [3,]  0.044561309
    ## [4,] -0.028635730
    ## [5,]  0.021260331
    ## [6,]  0.004042688

  
Calculate VaR and ES at 95% confidence level using GARCH model and
combine with previous simulation for comparison  
  

``` r
g_VaR <- quantile(rvec,0.05) 
g_ES <- mean(rvec[rvec<g_VaR])

cat("VaR is ", round(g_VaR,6), " and Expected Shortfall is ", round(g_ES,6))
```

    ## VaR is  -0.038417  and Expected Shortfall is  -0.055762

  

``` r
simulation <- c("actual", "garch scaled-t","t-distribution", "normal-distribution")
Value.at.Risk <- c(a_VaR, g_VaR, t_VaR, n_VaR)
Expected.shortfall <- c(a_ES, g_ES, t_ES, n_ES)


d.df <- data.frame(simulation, Value.at.Risk, Expected.shortfall)
d.df
```

    ##            simulation Value.at.Risk Expected.shortfall
    ## 1              actual   -0.02730072        -0.04198984
    ## 2      garch scaled-t   -0.03841740        -0.05576175
    ## 3      t-distribution   -0.02533877        -0.04152958
    ## 4 normal-distribution   -0.02901372        -0.03668049

  

### VaR and Expected during post-covid period

Next, we can perform diagnostics test with the GARCH model to see if it
will actual take in consideration for volatility clusters when
calculating value-at-risk  
  
For testing purpose, we picked the data up to 18 mar 2020 - the period
when covid-19 hit and stock market became volatile, to check the next
day VaR  

``` r
# head(msft_close)
msft_covid <- as.xts(msft_close)
msft_covid <- msft_covid["2000-01-01/2020-03-18"]
tail(msft_covid)
```

    ##            MSFT.Close
    ## 2020-03-11     153.63
    ## 2020-03-12     139.06
    ## 2020-03-13     158.83
    ## 2020-03-16     135.42
    ## 2020-03-17     146.57
    ## 2020-03-18     140.40

  
Calculating log-return for the period:  

``` r
logret_covid <- diff(log(msft_covid))[-1]
logret_covid <- as.vector(logret_covid)
covid_VaR <- quantile(logret_covid,0.05)
covid_ES <- mean(logret_covid[logret_covid<covid_VaR])
cat("VaR is ", round(covid_VaR,6), " and Expected Shortfall is ", round(covid_ES,6))
```

    ## VaR is  -0.02578  and Expected Shortfall is  -0.04167

  
The VaR and ES are well within GARCH model, however, it is far beyond
simulated VaR and ES from norm distribution and slightly exceed VaR and
ES from t-distribution simulation  

In other words, if we were using just the normal distribution or
t-distribution for estimating risk and VaR, then chances are we are
likely to underestimating the risk.  
  
With the GARCH model that taken volatility cluster into account, we can
now use this GARCH Model to calculate 1 day VaR over a period of time.  
There is a rolling VaR function built in in Garch package: ugarchroll  
  
The period that I used is 2021-07-01 to 2022-06-30  

``` r
logret <- as.xts(logret)
logret <- logret["2007-01-01/2022-06-30"] # cut off data at 2022-06-30
n2021 <- length(logret["2007-01-01/2021-06-30"])
roll.garch <- ugarchroll(spec=garch.t, data=logret, n.ahead=1, forecast.length = 1,n.start = n2021,refit.every = 1, refit.window = "recursive", calculate.VaR = TRUE, VaR.alpha = 0.05,keep.coef = TRUE)
head(roll.garch@forecast$VaR)
```

    ##              alpha(5%)      realized
    ## 2021-07-01 -0.01609842  2.580691e-03
    ## 2021-07-02 -0.01557690  2.203089e-02
    ## 2021-07-06 -0.01810857  3.605194e-05
    ## 2021-07-07 -0.01740365  8.142189e-03
    ## 2021-07-08 -0.01709371 -9.006897e-03
    ## 2021-07-09 -0.01714612  1.872620e-03

  
In comparison, I used the same period actual log-return.  
here we filter the period from Jul-2021 to Jun-2022 actual log-return
for visualization  

``` r
logret2021 <- logret["2021-07-01/2022-06-30"]
logret2021 <- as.data.frame(logret2021)
logret2021['date'] <- as.Date(rownames(logret2021))
names(logret2021)[1] <- "logret"

ggplot(logret2021, aes(x = date, y = `logret`)) +
  geom_col(color="blue")
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->  
From the rolling GARCH model, we can extract the VaR for visualization  

``` r
garch_VaR_forecast <- roll.garch@forecast$VaR
garch_VaR_forecast <- as.data.frame(garch_VaR_forecast)
garch_VaR_forecast['date'] <- as.Date(rownames(garch_VaR_forecast))

ggplot(garch_VaR_forecast) +
  geom_line(aes(x = date, y = `alpha(5%)`), color="red")
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->  
Lastly, we can combine both chart for comparison.  

``` r
graph <- merge(garch_VaR_forecast,logret2021,by="date")

ggplot(graph) +
  geom_line(aes(x = date, y = `alpha(5%)`), color="red")+
  geom_col(aes(x = date, y = `logret`),color="blue")
```

![](Financial_Risk_Project_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->  
The GARCH model are better estimating the risk by taking market
volatility into considering and it fit better during the period of high
volatiltity.  
  

``` r
# head(graph)
graph['exceed'] <- ifelse(graph$logret < graph$`alpha(5%)`, 1, 0)

(sum(graph$exceed) / nrow(graph))*100
```

    ## [1] 8.333333

However, noted that about 8% of log-return went lowered than Garch model
VaR during the high volatile period. The reason I can think of would be
volatility for post covid recovery and federal reserve fighting for
inflation with interest rate adjustment are unusual and it did not
reflected in the Garch’s model data from 2007 to 2020-Mar, hence the
variance.

``` r
# The intend for this project is to demonstrate the handling of time-series data for risk management while summarizing my learning for the risk management course's contents from Duke University. 
```
