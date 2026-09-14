``` r
############################################
# Austin Reszel                            #
# Time Series HW #1                        #
############################################
library(reprex)
library(ggplot2)
library(tsbox)
library(tidyverse)
library(tseries)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
library(forecast)
library(fpp3)
#> ── Attaching packages ──────────────────────────────────────────── fpp3 1.0.3 ──
#> ✔ tsibble     1.2.0     ✔ feasts      0.5.0
#> ✔ tsibbledata 0.4.1     ✔ fable       0.5.0
#> ✔ ggtime      1.0.0
#> ── Conflicts ───────────────────────────────────────────────── fpp3_conflicts ──
#> ✖ lubridate::date()    masks base::date()
#> ✖ dplyr::filter()      masks stats::filter()
#> ✖ tsibble::intersect() masks base::intersect()
#> ✖ tsibble::interval()  masks lubridate::interval()
#> ✖ dplyr::lag()         masks stats::lag()
#> ✖ tsibble::setdiff()   masks base::setdiff()
#> ✖ tsibble::union()     masks base::union()
library(astsa)
#> 
#> Attaching package: 'astsa'
#> The following object is masked from 'package:forecast':
#> 
#>     gas

icnsa_all <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=ICNSA", stringsAsFactors = FALSE) 
icnsa_cur <- data.frame(date  = as.Date(icnsa_all$observation_date),
                        value = icnsa_all$ICNSA)

icnsa_cur <- icnsa_cur[icnsa_cur$date <  '2026-9-5' & icnsa_cur$date >= '2000-1-1',]
```
We will just use the data since 2000.
Since the economy has changed significantly since this data has been collected,
some of the early data may not reflect how the economy behaves nowadays.

``` r
icnsa_trend <- data.frame(date  = as.Date(icnsa_cur$date),
                          value = log(icnsa_cur$value))
trend = ts_trend(icnsa_trend)
#> [time]: 'date'
ts_plot(ts_c(trend, icnsa_trend))
#> [time]: 'date'
#> [time]: 'date' 
#> [time]: 'date'
```

![](https://i.imgur.com/2zFHdOw.png)<!-- -->

There appears to be negative trend. This should be accounted for in the model.
There is a clear significant spike in claims, due to the COVID pandemic. The 2008 financial crisis
also lead to another clear spike in claims.
Also, we can see a pattern of local spikes, which we saw in the previous assignment,
related to the month of the observation (seasonality.)

``` r

icnsa_cur$log_value <- log(icnsa_cur$value)

icnsa_cur$month <- as.factor(month(icnsa_cur$date))
icnsa_cur |>
  ggplot(aes(x=date, y=log_value, group=month)) +
  geom_line(aes(col=month)) +
  facet_grid(month ~ ., scales='free')
```

![](https://i.imgur.com/4kDGoNc.png)<!-- -->


There appears to be a higher amount of initial unemployment claims in the winter months,
so I will create a dummy variable indicating if it’s winter or not. 
We will only use this for the simple regression model,
and while it is simplistic, it has an easy interpretation.

I also create several dummy variables to account for the Covid-19 pandemic and the various spikes,
as well as one for the 2008 financial crisis.

We will use the log scale to help stabilize variance.
Since all of our observations are positive, this will not lead to NAs.

``` r


icnsa_cur$winter <- icnsa_cur$month %in% c(11,12,1,2)

## https://en.wikipedia.org/wiki/COVID-19_recession ##
icnsa_cur$covid <- '2020-4-10' < icnsa_cur$date & 
  icnsa_cur$date < '2021-1-01'

icnsa_cur$covid_height <- icnsa_cur$date >= '2020-3-01' &
  icnsa_cur$date < "2020-04-10"

icnsa_cur$post_covid <- icnsa_cur$date >= '2021-1-01' &
  icnsa_cur$date < "2022-01-01"

icnsa_cur$recession08 <- icnsa_cur$date >= '2007-12-01' &
  icnsa_cur$date < "2009-07-01"

icnsa_cur$time <- as.numeric(icnsa_cur$date - min(icnsa_cur$date))
```

For the covariate, I chose to use the market yield on U.S treasury securities,
which is often seen as a key guage of other economic measures.
I have also seen this mentioned a bit in the news lately,
so I thought it would be interesting to include.
Since it is highly related to inflation and the national debt,
I thought it might also be connected to unemployment.

The weekly treasury yields data ends on Saturdays
while the initial claims data ends on Fridays. We consider the observations
that are one-date apart between these two data sets to be at the same time.
However, the weekly treasury data is released earlier, so we could use that data to
make our prediction without considering a lag.

```r
##Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity##
##Quoted on an Investment Basis## 
mrkt_yield <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=WGS10YR", stringsAsFactors = FALSE) 
mrkt_yield_cur <- data.frame(date  = as.Date(mrkt_yield$observation_date),
                              mrkt = mrkt_yield$WGS10YR)
mrkt_yield_cur <- mrkt_yield_cur[mrkt_yield_cur$date >= '1999-12-31' &
                                     mrkt_yield_cur$date < '2026-9-4',]

icnsa_cur$time <- as.numeric(icnsa_cur$date - min(icnsa_cur$date))
mrkt_yield_cur$time <- as.numeric(mrkt_yield_cur$date - min(mrkt_yield_cur$date))

all_data <- merge(icnsa_cur, mrkt_yield_cur, by = "time")
all_data$log_value <- log(all_data$value)

missing(mrkt_yield_cur)
#> [1] FALSE

ggplot(all_data, aes(x = time)) + 
  geom_line(aes(y = log_value, color = 'blue')) +
  geom_line(aes(y = mrkt, color = 'red')) +
  labs(x = "Days since first obs in 2000", y = "Values", color = "Legend") +
  theme_minimal()
```

![](https://i.imgur.com/2JWWWmW.png)<!-- -->

The decrease in market yield during covid seems to be related to the increase
in initial unemployment claims.
However, this may be more a result of the pandemic as opposed to these
two measures being correlated.

``` r

mrkt_yield_examine <- data.frame(time  = as.Date(all_data$date.y),
                              value = all_data$mrkt)
trend_yield = ts_trend(mrkt_yield_examine)
ts_plot(ts_c(trend_yield, mrkt_yield_examine))
```

![](https://i.imgur.com/wKDOFPT.png)<!-- -->

``` r


acf(all_data$value, 75, main="Initial Claims")
```

![](https://i.imgur.com/8AegM0F.png)<!-- -->

``` r
acf(all_data$mrkt, 200, main="Treasury Yields")
```

![](https://i.imgur.com/RUXPdH5.png)<!-- -->

``` r

all_data$value_diff <- c(NA, diff(all_data$value))
all_data$mrkt_diff <- c(NA, diff(all_data$mrkt))

ccf_data <- all_data
ccf_data <- ccf_data %>% filter(!is.na(value_diff), !is.na(mrkt_diff))

acf(ccf_data$value_diff, 75, main="Differenced Initial Claims")
```

![](https://i.imgur.com/Srvnqey.png)<!-- -->

``` r
acf(ccf_data$mrkt_diff, 200, main="Differenced Treasury Yields")
```

![](https://i.imgur.com/aLLQndK.png)<!-- -->

``` r

ccf_all <- ccf(ccf_data$value_diff, ccf_data$mrkt_diff, lag.max = 24, plot = TRUE,
               main = "d(log claims) vs d(log yield)")
```

![](https://i.imgur.com/x4wO2sm.png)<!-- -->

``` r

data.frame(lag = ccf_all$lag, correlation = round(ccf_all$acf, 3))
#>    lag correlation
#> 1  -24      -0.011
#> 2  -23       0.000
#> 3  -22       0.004
#> 4  -21       0.006
#> 5  -20       0.018
#> 6  -19      -0.017
#> 7  -18       0.008
#> 8  -17      -0.019
#> 9  -16      -0.002
#> 10 -15      -0.006
#> 11 -14       0.004
#> 12 -13      -0.019
#> 13 -12      -0.014
#> 14 -11       0.013
#> 15 -10       0.016
#> 16  -9       0.010
#> 17  -8      -0.004
#> 18  -7      -0.011
#> 19  -6       0.000
#> 20  -5       0.010
#> 21  -4      -0.030
#> 22  -3       0.003
#> 23  -2      -0.013
#> 24  -1      -0.059
#> 25   0      -0.007
#> 26   1       0.032
#> 27   2      -0.072
#> 28   3      -0.095
#> 29   4      -0.035
#> 30   5       0.007
#> 31   6       0.016
#> 32   7       0.027
#> 33   8       0.010
#> 34   9      -0.008
#> 35  10       0.020
#> 36  11       0.003
#> 37  12       0.030
#> 38  13       0.012
#> 39  14       0.005
#> 40  15       0.015
#> 41  16       0.020
#> 42  17      -0.037
#> 43  18      -0.026
#> 44  19       0.002
#> 45  20       0.017
#> 46  21       0.020
#> 47  22       0.029
#> 48  23       0.048
#> 49  24      -0.004

all_data$logmrkt_lag3 <- lag(log(all_data$mrkt), 3)
```

From the above output, we can see that taking the differenced values significantly
improves the lag plots and leads to more stationary time series. We also chose to consider
a lag of 3 weeks in terms of market yields to include in our model, since that showed the 
strongest correlation.

Below, we fit the model and obtain the prediction.

``` r
#fit the model
model1 <- lm(log_value ~ time + winter + covid + covid_height + post_covid
               + recession08 + logmrkt_lag3, data=all_data)
summary(model1)
#> 
#> Call:
#> lm(formula = log_value ~ time + winter + covid + covid_height + 
#>     post_covid + recession08 + logmrkt_lag3, data = all_data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.90009 -0.15722 -0.01924  0.13370  1.51714 
#> 
#> Coefficients:
#>                    Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       1.309e+01  3.395e-02 385.566  < 2e-16 ***
#> time             -7.710e-05  2.775e-06 -27.785  < 2e-16 ***
#> winterTRUE        2.111e-01  1.462e-02  14.438  < 2e-16 ***
#> covidTRUE         1.366e+00  5.166e-02  26.452  < 2e-16 ***
#> covid_heightTRUE  1.661e+00  1.160e-01  14.318  < 2e-16 ***
#> post_covidTRUE    4.504e-01  3.983e-02  11.309  < 2e-16 ***
#> recession08TRUE   3.294e-01  2.921e-02  11.277  < 2e-16 ***
#> logmrkt_lag3     -1.661e-01  2.110e-02  -7.874 6.88e-15 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2547 on 1381 degrees of freedom
#>   (3 observations deleted due to missingness)
#> Multiple R-squared:  0.6453, Adjusted R-squared:  0.6435 
#> F-statistic: 358.9 on 7 and 1381 DF,  p-value: < 2.2e-16

# save the prediction from the model 1
goal_date = as.Date("2026-09-05")
goal_time = as.numeric(goal_date - min(all_data$date.x))

mrkt_lag_date <- as.Date("2026-08-14")
mrkt_value_lag3 <- subset(mrkt_yield_cur, date == mrkt_lag_date, select = mrkt)
cur_mrkt_lag3 <- log(mrkt_value_lag3$mrkt)
  
model1_pred_info <- data.frame(time = c(goal_time),
                         winter = c(FALSE), covid= c(FALSE), covid_height=c(FALSE), post_covid=c(FALSE),
                         recession08 = c(FALSE), log_val = c(NA), logmrkt_lag3 = cur_mrkt_lag3)
model1_pred_log <- predict(model1, model1_pred_info)
model1_pred <- exp(model1_pred_log)
```
Now, here we are going to fit the ARIMA model.

```{r}
#### now to fit the ARIMA model ####
y <- ts(icnsa_cur$log_val, frequency = 52, start = c(2000, 1))
x_covid_height <- matrix(as.numeric(icnsa_cur$covid_height),
                         ncol = 1, dimnames = list(NULL, "covid_height"))
x_covid <- matrix(as.numeric(icnsa_cur$covid), ncol = 1, dimnames = list(NULL, "covid"))
x_post_covid <- matrix(as.numeric(icnsa_cur$post_covid),
                       ncol = 1, dimnames = list(NULL, "post_covid"))
x_recession08 <- matrix(as.numeric(icnsa_cur$recession08),
                ncol = 1, dimnames = list(NULL, "recession08"))
x_covs <- cbind(x_covid_height, x_covid, x_post_covid, x_recession08)

plot.ts(diff(y, lag = 52), main = "seasonal diff of log(claims)")
```

![](https://i.imgur.com/hyX3Bfw.png)<!-- -->

``` r
plot.ts(diff(y, lag = 1), main = 'regular diff of log(claims)')
```

![](https://i.imgur.com/i6vwJuZ.png)<!-- -->

We can see that taking both the seasonal and regular difference produces what appear to be
stationary sequences. This provides evidence for using both d=1 and D=1.

``` r
        
acf(diff(y, lag = 1), lag.max = 26, main = "ACF")
```

![](https://i.imgur.com/f3w5tr5.png)<!-- -->

``` r
pacf(diff(y, lag = 1), lag.max = 26, main = "PACF")
```

![](https://i.imgur.com/qOXGLwL.png)<!-- -->

``` r

acf(diff(y, lag = 52), lag.max = 156, main = "ACF")
```

![](https://i.imgur.com/jINvsI9.png)<!-- -->

``` r
pacf(diff(y, lag = 52), lag.max = 156, main = "PACF")
```

![](https://i.imgur.com/7hkD6wp.png)<!-- -->

The ACF shows a sharp cut off after 1 lag. The Pacf plot appears to tail off gradually. This provides
evidence for a value of q=1 for the moving average part and possibly p=0. We will consider
different combinations of p=0,1 and q=1,2 in our model selection. If we chose q=2, we will keep p=0
to avoid overfitting the model.

For the seasonal differencing, we see a gradual decrease in the ACF plot until we reach 1 year, where
we then have another peak followed by a gradual decrease that ultimately levels out near 0. In the
Pacf plot, we see the plot sharply cut off, except for at the full year multiples, where peaks
gradually decrease. Since we see a decay after 1 lag in the ACF plot, we will consider p=1 and we will
consider q=0,1 since the Pacf plot appears to decay immediately. 

``` r

candidates <- list(
  `(0,1,1)(1,1,0)[52]` = Arima(y, order=c(0,1,1), seasonal=list(order=c(1,1,0), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(1,1,1)(1,1,0)[52]` = Arima(y, order=c(1,1,1), seasonal=list(order=c(1,1,0), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(0,1,2)(1,1,0)[52]` = Arima(y, order=c(0,1,2), seasonal=list(order=c(1,1,0), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(1,1,1)(1,0,0)[52]` = Arima(y, order=c(1,1,1), seasonal=list(order=c(1,0,0), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(0,1,1)(1,0,0)[52]` = Arima(y, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(0,1,1)(1,1,1)[52]` = Arima(y, order=c(0,1,1), seasonal=list(order=c(1,1,1), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(1,1,1)(1,1,1)[52]` = Arima(y, order=c(1,1,1), seasonal=list(order=c(1,1,1), period=52),
                               xreg = x_covs, method = "CSS-ML"),
  `(0,1,2)(1,1,1)[52]` = Arima(y, order=c(0,1,2), seasonal=list(order=c(1,1,1), period=52),
                               xreg = x_covs, method = "CSS-ML")
)

tibble(
  model = names(candidates),
  aicc  = map_dbl(candidates, "aicc")
) |>
  arrange(aicc)
#> # A tibble: 8 × 2
#>   model                aicc
#>   <chr>               <dbl>
#> 1 (1,1,1)(1,0,0)[52] -1967.
#> 2 (0,1,1)(1,0,0)[52] -1967.
#> 3 (0,1,1)(1,1,1)[52] -1784.
#> 4 (1,1,1)(1,1,1)[52] -1784.
#> 5 (0,1,2)(1,1,1)[52] -1782.
#> 6 (1,1,1)(1,1,0)[52] -1695.
#> 7 (0,1,2)(1,1,0)[52] -1694.
#> 8 (0,1,1)(1,1,0)[52] -1690.

```
We take the models with the 3 smallest AICC values and look at the residual diagnostics.

```r
arima1 <- Arima(y, order=c(1,1,1), seasonal=list(order=c(1,0,0), period=52),
                xreg = x_covs, method = "CSS-ML")
arima2 <- Arima(y, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=52),
                xreg = x_covs, method = "CSS-ML")
arima3 <- Arima(y, order=c(0,1,1), seasonal=list(order=c(1,1,1), period=52),
                xreg = x_covs, method = "CSS-ML")

# check on the residuals #
checkresiduals(arima1)
```

![](https://i.imgur.com/0USp6tB.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,1,1)(1,0,0)[52] errors
    #> Q* = 232.37, df = 101, p-value = 2.349e-12
    #> 
    #> Model df: 3.   Total lags used: 104
    checkresiduals(arima2)

![](https://i.imgur.com/Z7y69p0.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(0,1,1)(1,0,0)[52] errors
    #> Q* = 229.14, df = 102, p-value = 9.057e-12
    #> 
    #> Model df: 2.   Total lags used: 104
    checkresiduals(arima3)

![](https://i.imgur.com/5d7MbWR.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(0,1,1)(1,1,1)[52] errors
    #> Q* = 287.91, df = 101, p-value < 2.2e-16
    #> 
    #> Model df: 3.   Total lags used: 104

Each of these models still has some large outliers, and the Ljung-Box test strongly rejects. This
suggests we still have autocorrelation. We look for some of these extreme outliers and create
dummy variables to attempt to improve the residual diagnostics.
    
    # residuals have some extreme outliers #
    res_values <- residuals(arima1)

    res <- data.frame(
      Date = icnsa_cur$date,
      Residual = as.numeric(res_values)
    )

    extreme_outliers <- subset(res, abs(Residual) > 0.5)
    print(extreme_outliers)
    #>            Date   Residual
    #> 1056 2020-03-21  2.4693060
    #> 1057 2020-03-28  0.8044744
    #> 1108 2021-03-20 -1.3695003
    
    icnsa_cur$covid_32120 <- icnsa_cur$date == '2020-3-21'
    icnsa_cur$covid_32820 <- icnsa_cur$date == '2020-3-28'
    icnsa_cur$covid_32021 <- icnsa_cur$date == '2021-3-20'

    x_covid_32120 <- matrix(as.numeric(icnsa_cur$covid_32120),
                      ncol = 1, dimnames = list(NULL, "covid_32120"))
    x_covid_32820 <- matrix(as.numeric(icnsa_cur$covid_32820),
                             ncol = 1, dimnames = list(NULL, "covid_32820"))
    x_covid_32021 <- matrix(as.numeric(icnsa_cur$covid_32021),
                            ncol = 1, dimnames = list(NULL, "covid_32021"))
    x_covs2 <- cbind(x_covid, x_covid_height, x_post_covid, x_recession08,
                    x_covid_32820, x_covid_32120, x_covid_32021)

    candidates2 <- list(
      `(1,1,1)(1,0,0)[52]` = Arima(y, order=c(1,1,1), seasonal=list(order=c(1,0,0), period=52),
                                   xreg = x_covs2, method = "CSS-ML"),
      `(0,1,1)(1,0,0)[52]` = Arima(y, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=52),
                                   xreg = x_covs2, method = "CSS-ML"),
      `(0,1,1)(1,1,1)[52]` = Arima(y, order=c(0,1,1), seasonal=list(order=c(1,1,1), period=52),
                                   xreg = x_covs2, method = "CSS-ML")
    )

    tibble(
      model = names(candidates2),
      aicc  = map_dbl(candidates2, "aicc")
    ) |>
      arrange(aicc)
    #> # A tibble: 3 × 2
    #>   model                aicc
    #>   <chr>               <dbl>
    #> 1 (1,1,1)(1,0,0)[52] -2321.
    #> 2 (0,1,1)(1,0,0)[52] -2314.
    #> 3 (0,1,1)(1,1,1)[52] -2130.

    arima4 <- Arima(y, order=c(1,1,1), seasonal=list(order=c(1,0,0), period=52),
                    xreg = x_covs2, method = "CSS-ML")

    checkresiduals(arima4)

![](https://i.imgur.com/W5Z2vNs.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,1,1)(1,0,0)[52] errors
    #> Q* = 317.75, df = 101, p-value < 2.2e-16
    #> 
    #> Model df: 3.   Total lags used: 104

    res_values4 <- residuals(arima4)

    res4 <- data.frame(
      Date = icnsa_cur$date,
      Residual = as.numeric(res_values4)
    )

    extreme_outliers4 <- subset(res4, abs(Residual) > 1)
    print(extreme_outliers4)
    #>            Date Residual
    #> 1056 2020-03-21 1.083816
    #> 1057 2020-03-28 1.089018
    #> 1058 2020-04-04 1.102057

Adding more dummy variables to account for the outliers does improve AICC and lead to some 
less pronounced residuals, but the Ljung-Box test still strongly rejects. We will choose
to continue with the model with the smallest AICC from the first part of the model
selection, in order to avoid using a model that is too complex in terms of included
covariates. The final estimates for the regular regression and the ARIMA model are shown below:

    arima_final <- arima1

    # prediction for the arima model #
    arima_fc <- forecast(arima_final, h = 1,
                         xreg = matrix(c(0,0,0,0), nrow = 1, ncol = 4,
                                       dimnames = list(NULL, c('covid_height', 'covid',
                                                               'post_covid', 'recession08'))),
                         level = 95)
    model2_pred <- exp(as.numeric(arima_fc$mean))

    # final result for the first model
    model1_pred
    #>        1 
    #> 176743.5
    # final result for the second model
    model2_pred
    #> [1] 175384.8

<sup>Created on 2026-09-14 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
