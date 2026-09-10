---
editor_options: 
  markdown: 
    wrap: 72
---

``` r
############################################
# Austin Reszel                            #
# Time Series HW #1                        #
############################################
library(ggplot2)
library(tsbox)
#> Warning: package 'tsbox' was built under R version 4.5.3
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.5.3
#> Warning: package 'readr' was built under R version 4.5.3
#> Warning: package 'forcats' was built under R version 4.5.3
#> Warning: package 'lubridate' was built under R version 4.5.3
library(tseries)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
library(forecast)
library(fpp3)
#> Warning: package 'fpp3' was built under R version 4.5.3
#> ── Attaching packages ──────────────────────────────────────────── fpp3 1.0.3 ──
#> ✔ tsibble     1.2.0     ✔ feasts      0.5.0
#> ✔ tsibbledata 0.4.1     ✔ fable       0.5.0
#> ✔ ggtime      0.2.0
#> Warning: package 'tsibble' was built under R version 4.5.3
#> Warning: package 'tsibbledata' was built under R version 4.5.3
#> Warning: package 'ggtime' was built under R version 4.5.3
#> Warning: package 'feasts' was built under R version 4.5.3
#> Warning: package 'fabletools' was built under R version 4.5.3
#> Warning: package 'fable' was built under R version 4.5.3
#> ── Conflicts ───────────────────────────────────────────────── fpp3_conflicts ──
#> ✖ lubridate::date()    masks base::date()
#> ✖ dplyr::filter()      masks stats::filter()
#> ✖ tsibble::intersect() masks base::intersect()
#> ✖ tsibble::interval()  masks lubridate::interval()
#> ✖ dplyr::lag()         masks stats::lag()
#> ✖ tsibble::setdiff()   masks base::setdiff()
#> ✖ tsibble::union()     masks base::union()

## Load in the data ##
icnsa_all <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=ICNSA", stringsAsFactors = FALSE) 
icnsa_cur <- data.frame(date  = as.Date(icnsa_all$observation_date),
                        value = icnsa_all$ICNSA)
```

We will just use the data since 2010. Since the economy has changed
significantly since this data has been collected, some of the early data
may not reflect how the economy behaves nowadays.

```r
icnsa_cur <- icnsa_cur[icnsa_cur$date <  '2026-9-5' & icnsa_cur$date >= '2010-1-1',]


icnsa_trend <- data.frame(date  = as.Date(icnsa_cur$date),
                          value = log(icnsa_cur$value))
trend = ts_trend(icnsa_trend)
#> [time]: 'date'
ts_plot(ts_c(trend, icnsa_trend))
#> [time]: 'date'
#> [time]: 'date' 
#> [time]: 'date'
```

![](https://i.imgur.com/v4qujUz.png)<!-- -->

There appears to be negative trend. This should be accounted for in the
model. There is a clear significant spike in claims, due to the COVID
pandemic. Also, we can see a pattern of local spikes, which we saw in
the previous assignment, related to the month of the observation
(seasonality.)

``` r
icnsa_cur$month <- as.factor(month(icnsa_cur$date))
icnsa_cur |>
  ggplot(aes(x=date, y=value, group=month)) +
  geom_line(aes(col=month)) +
  facet_grid(month ~ ., scales='free')
```

![](https://i.imgur.com/b3E0TMw.png)<!-- -->

There appears to be a higher amount of initial unemployment claims in
the winter months, so I will create a dummy variable indicating if it's
winter or not. We will only use this for the simple regression model,
and while it is simplistic, it has an easy interpretation.

I also create a dummy variable to account for the entire covid recession.

Some of the values during COVID had a much higher number of claims.
On the first run of this code, there were still a significant number of very large residuals.
I create a second dummy variable to account for this peak and hopefully improve the residual
diagnostics.

``` r
icnsa_cur$winter <- icnsa_cur$month %in% c(11,12,1,2)

## Dates of covid recession were about Feb 20, 2020 to late 2020 ##
## https://en.wikipedia.org/wiki/COVID-19_recession ##
icnsa_cur$covid <- '2020-2-20' < icnsa_cur$date & 
  icnsa_cur$date < '2021-1-01'

icnsa_cur$covid_height <- icnsa_cur$date >= '2020-3-28' &
  icnsa_cur$date <= "2020-04-25"

```

We will use the log scale to help stabilize variance.
Since all of our observations are positive, this will not lead to NAs.

```r
icnsa_cur$time <- as.numeric(icnsa_cur$date - min(icnsa_cur$date))
icnsa_cur$log_val <- log(icnsa_cur$value)

```

For the covariate, I chose to use the market yield on U.S treasury securities, 
which is often seen as a key guage of other economic measures. I have also seen
this mentioned a bit in the news lately, so I thought it would be interesting to include
Since it is highly related to inflation and the national debt, I thought it might also
be connected to unemployment. 

The weekly treasury yields data ends on Saturdays  
while the initial claims data ends on Fridays. We consider
the observations that are one-date apart between these two data sets to be at the same time.
However, the weekly treasury data is released earlier, so we could use that data
to make our prediction without considering a lag.

```r
##Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity##
##Quoted on an Investment Basis## 
mrkt_yield <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=WGS10YR", stringsAsFactors = FALSE) 
mrkt_yield_cur <- data.frame(date  = as.Date(mrkt_yield$observation_date),
                              mrkt = mrkt_yield$WGS10YR)
mrkt_yield_cur <- mrkt_yield_cur[mrkt_yield_cur$date >= '2010-01-01' &
                                     mrkt_yield_cur$date < '2026-9-4',]

icnsa_cur$time <- as.numeric(icnsa_cur$date - min(icnsa_cur$date))
mrkt_yield_cur$time <- as.numeric(mrkt_yield_cur$date - min(mrkt_yield_cur$date))

all_data <- merge(icnsa_cur, mrkt_yield_cur, by = "time")
all_data$log_val <- log(all_data$value)

missing(mrkt_yield_cur)
#> [1] FALSE

## examining data before fitting model ##
ggplot(all_data, aes(x = time)) + 
  geom_line(aes(y = log_val, color = 'blue')) +
  geom_line(aes(y = mrkt, color = 'red')) +
  labs(x = "Time since first obs in 2010", y = "Values", color = "Legend") +
  theme_minimal()
```

![](https://i.imgur.com/WdjMPVw.png)<!-- -->


The decrease in market yield during covid seems to be related to the increase in
initial unemployment claims. However, this may be more a result of the pandemic
as opposed to these two measures being correlated.

```r
mrkt_yield_examine <- data.frame(time  = as.Date(all_data$date.y),
                              value = all_data$mrkt)
trend_yield = ts_trend(mrkt_yield_examine)
ts_plot(ts_c(trend_yield, mrkt_yield_examine))
```

![](https://i.imgur.com/eqqrQ6K.png)<!-- -->


Looking at the market yield, it seemed to trend down until the pandemic, and since then
it has trended upwards.

Now we look at the plots of lagged values to see if there are any patterns in the outcome and 
chosen covariate.

``` r

recent_yields <- mrkt_yield_cur |>
  filter(date >= '2015-1-1')
yields_tsibble <- as_tsibble(recent_yields)
#> Using `date` as index variable.
yields_tsibble |>
  gg_lag(mrkt, geom = "point") +
  labs(x = "lag(mrkt, k)")
```

![](https://i.imgur.com/qUe3aNe.png)<!-- -->

``` r

# This shows that previous values of market yield are extremely positively correlated #
# with the current value.##

recent_claims <- icnsa_cur |>
  filter(date >= '2015-1-1')
claims_tsibble <- as_tsibble(recent_claims)
#> Using `date` as index variable.
claims_tsibble |>
  gg_lag(value, geom= 'point') +
  labs(x = "lag(value, k")
```

![](https://i.imgur.com/uf77RBe.png)<!-- -->

It seems that recent claims are correlated somewhat linearly at small lags, but the pattern
becomes more complex as the lag increases.

``` r
# create the difference
all_data$value_diff <- c(NA, diff(log(all_data$value)))
all_data$mrkt_diff <- c(NA, diff(log(all_data$mrkt)))

ccf_data <- all_data
ccf_data <- ccf_data %>% filter(!is.na(value_diff), !is.na(mrkt_diff))

# trying to find the proper lag for the co variate
ccf_all <- ccf(ccf_data$value_diff, ccf_data$mrkt_diff, lag.max = 24, plot = TRUE,
               main = "d(log claims) vs d(log yield)")
```

![](https://i.imgur.com/Pgd3luo.png)<!-- -->

It seems like a lag of 1 has a significant correlation. This correlation
is higher than when there is no lag. We will use the previous week's 
market yield as a predictor of the current weeks initial claims.

``` r
data.frame(lag = ccf_all$lag, correlation = round(ccf_all$acf, 3))
#>    lag correlation
#> 1  -24      -0.011
#> 2  -23       0.016
#> 3  -22       0.006
#> 4  -21       0.056
#> 5  -20      -0.015
#> 6  -19      -0.047
#> 7  -18       0.002
#> 8  -17      -0.086
#> 9  -16      -0.036
#> 10 -15       0.011
#> 11 -14      -0.046
#> 12 -13      -0.060
#> 13 -12      -0.015
#> 14 -11       0.082
#> 15 -10       0.051
#> 16  -9       0.013
#> 17  -8      -0.042
#> 18  -7       0.004
#> 19  -6       0.064
#> 20  -5      -0.009
#> 21  -4      -0.099
#> 22  -3       0.041
#> 23  -2      -0.079
#> 24  -1      -0.128
#> 25   0       0.083
#> 26   1      -0.051
#> 27   2      -0.144
#> 28   3      -0.073
#> 29   4      -0.039
#> 30   5      -0.022
#> 31   6       0.019
#> 32   7       0.014
#> 33   8       0.035
#> 34   9      -0.012
#> 35  10      -0.024
#> 36  11       0.028
#> 37  12       0.002
#> 38  13       0.019
#> 39  14       0.005
#> 40  15       0.011
#> 41  16       0.062
#> 42  17      -0.056
#> 43  18      -0.035
#> 44  19      -0.039
#> 45  20       0.019
#> 46  21       0.034
#> 47  22       0.015
#> 48  23       0.009
#> 49  24       0.000

all_data$logmrkt_lag1 <- lag(log(all_data$mrkt), 1)

#fit the model
model1 <- lm(log_val ~ time + winter + covid + covid_height +
               logmrkt_lag1, data=all_data)
summary(model1)
#> 
#> Call:
#> lm(formula = log_val ~ time + winter + covid + covid_height + 
#>     logmrkt_lag1, data = all_data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.57834 -0.17156 -0.02037  0.12326  1.19013 
#> 
#> Coefficients:
#>                    Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       1.290e+01  2.903e-02 444.362  < 2e-16 ***
#> time             -9.513e-05  5.924e-06 -16.058  < 2e-16 ***
#> winterTRUE        1.905e-01  2.003e-02   9.512  < 2e-16 ***
#> covidTRUE         1.113e+00  5.932e-02  18.761  < 2e-16 ***
#> covid_heightTRUE  1.693e+00  1.311e-01  12.914  < 2e-16 ***
#> logmrkt_lag1     -1.567e-01  2.972e-02  -5.271 1.71e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2762 on 863 degrees of freedom
#>   (1 observation deleted due to missingness)
#> Multiple R-squared:  0.6723, Adjusted R-squared:  0.6704 
#> F-statistic: 354.1 on 5 and 863 DF,  p-value: < 2.2e-16

```
Above is the simple model we used to make the first prediction. From this output,
we can see that our model, whether adjusting for the number of predictors or not,
accounts for about 67% of the variation in the outcome (inital claims).

```r
# save the prediction from the model 1
goal_date = as.Date("2026-09-05")
goal_time = as.numeric(goal_date - min(all_data$date.x))
mrkt_lag_date <- max(mrkt_yield_cur$date)
mrkt_value_now <- subset(mrkt_yield_cur, date==mrkt_lag_date, select = mrkt)
cur_mrkt_lag1 <- log(mrkt_value_now$mrkt)
  
model1_pred <- data.frame(time = c(goal_time),
                         winter = c(FALSE), covid= c(FALSE), covid_height=c(FALSE),
                         log_val = c(NA), logmrkt_lag1 = cur_mrkt_lag1)
model1_pred_log <- predict(model1, model1_pred)
model1_pred <- exp(model1_pred_log)

#### now to fit the ARIMA model ####
y <- ts(icnsa_cur$log_val, frequency = 52, start = c(2010, 1))
x_covid <- matrix(as.numeric(icnsa_cur$covid), ncol = 1, dimnames = list(NULL, "covid"))
x_covid_height <- matrix(as.numeric(icnsa_cur$covid_height),
                         ncol = 1, dimnames = list(NULL, "covid_height"))
x_covs <- cbind(x_covid, x_covid_height)

# check on the stationary property #
icnsa_tsibble <- as_tsibble(icnsa_cur, index=time)

icnsa_tsibble |>
  autoplot(log_val) +
  labs(y = "Inital claims")
```

![](https://i.imgur.com/uHc5IOT.png)<!-- -->

``` r

icnsa_tsibble |>
  ACF(log_val) |>
  autoplot()
```

![](https://i.imgur.com/BQgnkLL.png)<!-- -->

``` r

icnsa_tsibble |>
  autoplot(difference(log_val)) +
  labs(y = "Change in Initial Claims from previous week")
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).
```

![](https://i.imgur.com/2zCHJ1z.png)<!-- -->

``` r

icnsa_tsibble |>
  ACF(difference(log_val)) |>
  autoplot()
```

![](https://i.imgur.com/IM41Xbe.png)<!-- -->

``` r

adf.test(icnsa_tsibble$log_val)
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  icnsa_tsibble$log_val
#> Dickey-Fuller = -3.9308, Lag order = 9, p-value = 0.01246
#> alternative hypothesis: stationary
```
The initial plot of log(initial claims) and the corresponding ACF plot create doubt that the
non-differenced outcome is stationary. Even with a significant p-value from the corresponding test,
the plots for the once differenced outcome show much more evidence for the stationary property.
So, we will use d=1 in the ARIMA model.

We also check for seasonality.

```r
## seasonal part ##
icnsa_tsibble |>
  ACF(difference(log_val), lag_max = 104) |>
  autoplot()
```
There is a significant jump at exactly 52 weeks, providing evidence for including
a seasonal part to the Arima model. It appears to have a smaller jump after 2 years, providing 
evidence to possibly include a seasonal difference in the model. I initially included this,
but it produced both larger AIC/BIC values for each considered model, as well as worse 
residual diagnostics than corresponding models without seasonal difference. So, below, the considered
models will not use seasonal differencing.

![](https://i.imgur.com/KCGGjm4.png)<!-- -->

Now, we will check the ACF and PACF plot for the once differenced log(initial claims) to help
decide plausible values of p and q.

``` r

##checking for the p,q ##
Acf(difference(icnsa_cur$log_val), lag.max = 60, main = "ACF")
```

![](https://i.imgur.com/mCY3qmd.png)<!-- -->

From this Acf plot, it is difficult to determine a value where there is a sharp cut off, since 
both 1,2 are not close to being significant.

``` r
Pacf(difference(icnsa_cur$log_val), lag.max = 60, main = "PACF")
```

![](https://i.imgur.com/qPkYSHI.png)<!-- -->

Likewise, for the Pacf plot, it is difficult to determine a value where there is a sharp cutoff.
In general, there is close to significance at 3. If we use larger values for p,q, the model
will likely be overfit and too complex. Additionally, we do not want to use p,q both as 0, since
that will be an over-simplistic random walk model. We will consider situations where p=3, q=0 or 
p=0, q=3. We also consider, for the seasonal part of the model, combinations of p=0/1, q=0/1 to
see what produces the best AIC/BIC as well as reasonable residual diagnostics. As noted above,
we removed the possibility of seasonal differencing due to worse model performance.

``` r

option1 <- Arima(y, order=c(3,1,0), seasonal=list(order=c(1,0,0), period=52),
                xreg = x_covs, method = "CSS-ML")
option2 <- Arima(y, order=c(3,1,0), seasonal=list(order=c(0,0,1), period=52),
                xreg = x_covs, method = "CSS-ML")
option3 <- Arima(y, order=c(3,1,0), seasonal=list(order=c(1,0,1), period=52),
                 xreg = x_covs, method = "CSS-ML")
option4 <- Arima(y, order=c(0,1,3), seasonal=list(order=c(1,0,0), period=52),
                 xreg = x_covs, method = "CSS-ML")
option5 <- Arima(y, order=c(0,1,3), seasonal=list(order=c(0,0,1), period=52),
                 xreg = x_covs, method = "CSS-ML")
option6 <- Arima(y, order=c(0,1,3), seasonal=list(order=c(1,0,1), period=52),
                 xreg = x_covs, method = "CSS-ML")
option1$aic
#> [1] -1073.355
option2$aic
#> [1] -1044.431
option3$aic
#> [1] -1087.681
option4$aic
#> [1] -1073.541
option5$aic
#> [1] -1044.587
option6$aic
#> [1] -1087.947

```
So, options 3 and 6 produce very similar values of AIC, which are smallest. We consider
these two in terms of residual diagnostics to choose the final model.

```r
#view the residual diagnostics
checkresiduals(option3)
```

![](https://i.imgur.com/JhrmzTi.png)<!-- -->

```         
#> 
#>  Ljung-Box test
#> 
#> data:  Residuals from Regression with ARIMA(3,1,0)(1,0,1)[52] errors
#> Q* = 128.32, df = 99, p-value = 0.02537
#> 
#> Model df: 5.   Total lags used: 104
checkresiduals(option6)
```

![](https://i.imgur.com/aQRWvqZ.png)<!-- -->

```         
#> 
#>  Ljung-Box test
#> 
#> data:  Residuals from Regression with ARIMA(0,1,3)(1,0,1)[52] errors
#> Q* = 129.78, df = 99, p-value = 0.02064
#> 
#> Model df: 5.   Total lags used: 104
```

Both of these models have some issues in terms of residual diagnostics. Although, they
were much better than any other models I considered but did not include in the write up.
There appears to still be some variation as a result of covid that was not accounted for.
I attempted to add a third dummy variable to account for the very highest values during covid,
as well as adjusting the dates. I was not able to find a way to improve the residual diagnostics
in this way, and including many dummy variables may make the model interpretation somewhat
arbitrary. So, we will choose option 6 as the final ARIMA model, since that has the lowest AIC.

Below are listed the two predictions from the models.

```r
# prediction for the arima model #
arima_fc <- forecast(option6, h = 1,
                     xreg = matrix(c(0,0), nrow = 1, ncol = 2,
                                   dimnames = list(NULL, c("covid", "covid_height"))),
                     level = 95)
model2_pred <- exp(as.numeric(arima_fc$mean))

# final result for the first model
model1_pred
#>        1 
#> 176130.3
# final result for the second model
model2_pred
#> [1] 171524
```

<sup>Created on 2026-09-10 with [reprex
v2.1.1](https://reprex.tidyverse.org)</sup>
