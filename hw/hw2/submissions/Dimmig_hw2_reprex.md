``` r
library(fredr)
library(tidyverse)
library(tsibble)
#> 
#> Attaching package: 'tsibble'
#> The following object is masked from 'package:lubridate':
#> 
#>     interval
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, union
library(fable)
#> Loading required package: fabletools
library(feasts)
library(fabletools)
library(forecast)     
library(tsbox)        
library(zoo)
#> 
#> Attaching package: 'zoo'
#> The following object is masked from 'package:tsibble':
#> 
#>     index
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
library(tseries)     
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
library(lubridate)
library(tidyr)
library(fpp3)
#> ── Attaching packages ──────────────────────────────────────────── fpp3 1.0.3 ──
#> ✔ tsibbledata 0.4.1     ✔ ggtime      1.0.0
#> ── Conflicts ───────────────────────────────────────────────── fpp3_conflicts ──
#> ✖ lubridate::date()    masks base::date()
#> ✖ dplyr::filter()      masks stats::filter()
#> ✖ zoo::index()         masks tsibble::index()
#> ✖ tsibble::intersect() masks base::intersect()
#> ✖ tsibble::interval()  masks lubridate::interval()
#> ✖ dplyr::lag()         masks stats::lag()
#> ✖ tsibble::setdiff()   masks base::setdiff()
#> ✖ tsibble::union()     masks base::union()

fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=ICNSA"

icnsa_raw <- read.csv(fred_url, stringsAsFactors = FALSE)
names(icnsa_raw) <- c("date", "value")

icnsa <- data.frame(
  date  = as.Date(icnsa_raw$date),
  ICNSA = suppressWarnings(as.numeric(icnsa_raw$value))
)

print(tail(icnsa, 5))
#>            date  ICNSA
#> 3111 2026-08-15 173017
#> 3112 2026-08-22 170596
#> 3113 2026-08-29 171403
#> 3114 2026-09-05 176916
#> 3115 2026-09-12 152286


# Don't really need the data prior to 2010 (does not impact trend forecast that much,
# so removing those entries to create a smaller data set)
win_start   <- as.Date("2010-01-02")
target_date <- as.Date("2026-09-26")   # week we are forecasting

icnsa_win <- icnsa |> filter(date >= win_start, date < target_date)
cat("Modeling window:", format(min(icnsa_win$date)), "to", format(max(icnsa_win$date)),
    "(", nrow(icnsa_win), "observations )\n")
#> Modeling window: 2010-01-02 to 2026-09-12 ( 872 observations )
#Look at the initial plot to assess
ggplot(icnsa_win, aes(x = date, y = ICNSA)) +  
  geom_line() +
  ggtitle("ICNSA: Initial Claims, Not Seasonally Adjusted") +
  ylab("Claims") + xlab("Year")
```

![](https://i.imgur.com/Al7pP9t.png)<!-- -->

``` r

#let's try log adjustment
ggplot(icnsa_win, aes(x = date, y = log(ICNSA))) +  # replace date/value with your actual column names
  geom_line() +
  ggtitle("ICNSA: Initial Claims, Not Seasonally Adjusted") +
  ylab("(log) Claims") + xlab("Year")
```

![](https://i.imgur.com/YXWtL3h.png)<!-- -->

``` r

#Still a downward/negative trend, let's take the diff of the log
dlog_ICNSA <- diff(log(icnsa_win$ICNSA)) 
plot(dlog_ICNSA, type = "l")
```

![](https://i.imgur.com/A4gMnIb.png)<!-- -->

``` r

#Looks a lot better, but there is still a large spike that represents the covid window, so will create a covid dummy variable to account for this.
#Both tests tell us that our data is now stationary, so will use a d = 1 in later ARIMA model
adf.test(dlog_ICNSA)
#> Warning in adf.test(dlog_ICNSA): p-value smaller than printed p-value
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  dlog_ICNSA
#> Dickey-Fuller = -12.031, Lag order = 9, p-value = 0.01
#> alternative hypothesis: stationary
kpss.test(dlog_ICNSA)
#> Warning in kpss.test(dlog_ICNSA): p-value greater than printed p-value
#> 
#>  KPSS Test for Level Stationarity
#> 
#> data:  dlog_ICNSA
#> KPSS Level = 0.017233, Truncation lag parameter = 6, p-value = 0.1

#Adding in a COVID variable to adjust for the spike of claims
icnsa_win <- icnsa_win |>
  mutate(covid = as.integer(date >= as.Date("2020-03-14") & date <= as.Date("2021-03-31"))) |>
  mutate(log_ICNSA = log(ICNSA)) 

#Appears to have a spike annually at around xmas/new years, so adding
# in a holiday variable to account of weeks that contain Xmas and New Years
icnsa_win <- icnsa_win |>
  arrange(date) |>
  mutate(
    year = year(date),
    # for each row, does the holiday fall within the 7 days ending on this date?
    christmas_in_window = as.integer(
      (as.Date(paste0(year, "-12-25")) <= date & as.Date(paste0(year, "-12-25")) > date - 7) |
        (as.Date(paste0(year - 1, "-12-25")) <= date & as.Date(paste0(year - 1, "-12-25")) > date - 7)
    ),
    newyear_in_window = as.integer(
      (as.Date(paste0(year, "-01-01")) <= date & as.Date(paste0(year, "-01-01")) > date - 7)
    ),
    holiday_week = as.integer(christmas_in_window == 1 | newyear_in_window == 1)
  )
#Take into account the week before and after a holiday week 
icnsa_win <- icnsa_win |>
  mutate(
    holiday_lag1 = lag(holiday_week, 1),
    holiday_lead1 = lead(holiday_week, 1)
  )

#filter out NA. We know that the first date "2010-01-02" is a week lagging after xmas,
# so the first date should get a holiday_lag1 = 1. 
# don't want to eliminate last week's value from the regression model and we know that next week is not a holiday
reg_data <- icnsa_win 
reg_data <- replace_na(reg_data, list(holiday_lag1 = 1, holiday_lead1 = 0))

t0 <- min(reg_data$date)
reg_data <- reg_data |> mutate(t = as.numeric(date - t0)/7)

x_reg <- reg_data |> select(logICNSA = log_ICNSA, t, covid, holiday = holiday_week, holiday_lag1, holiday_lead1)

reg_fit <- lm(logICNSA ~ ., data = x_reg)
summary(reg_fit)
#> 
#> Call:
#> lm(formula = logICNSA ~ ., data = x_reg)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.49511 -0.14967 -0.00889  0.11742  1.70444 
#> 
#> Coefficients:
#>                 Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    1.283e+01  1.706e-02 752.097  < 2e-16 ***
#> t             -8.144e-04  3.382e-05 -24.083  < 2e-16 ***
#> covid          1.534e+00  3.501e-02  43.802  < 2e-16 ***
#> holiday       -3.861e-03  6.138e-02  -0.063     0.95    
#> holiday_lag1   4.276e-01  5.336e-02   8.014 3.58e-15 ***
#> holiday_lead1  2.338e-01  5.442e-02   4.296 1.93e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2493 on 866 degrees of freedom
#> Multiple R-squared:  0.7338, Adjusted R-squared:  0.7322 
#> F-statistic: 477.4 on 5 and 866 DF,  p-value: < 2.2e-16

#Every term besides the Holiday week is highly significant, so what would the model look like without
#the holiday week, but the lag and lead to a holiday?
x_reg2 <- reg_data |> select(logICNSA = log_ICNSA, t, covid, holiday_lag1, holiday_lead1)
reg_fit2 <- lm(logICNSA ~ ., data = x_reg2)
summary(reg_fit2)
#> 
#> Call:
#> lm(formula = logICNSA ~ ., data = x_reg2)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.49511 -0.14967 -0.00889  0.11742  1.70444 
#> 
#> Coefficients:
#>                 Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   12.8313536  0.0170510 752.530  < 2e-16 ***
#> t             -0.0008144  0.0000338 -24.097  < 2e-16 ***
#> covid          1.5337276  0.0349946  43.828  < 2e-16 ***
#> holiday_lag1   0.4256911  0.0436275   9.757  < 2e-16 ***
#> holiday_lead1  0.2318665  0.0449129   5.163 3.02e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2492 on 867 degrees of freedom
#> Multiple R-squared:  0.7338, Adjusted R-squared:  0.7325 
#> F-statistic: 597.4 on 4 and 867 DF,  p-value: < 2.2e-16

#Get similar values but with one less parameter, going to use reg_fit2 to predict
# Regression based forecast

t_target <- as.numeric(target_date - t0)/7
newdata <- as.data.frame(cbind(t = t_target, covid = 0, holiday_lag1 = 0, holiday_lead1 = 0))

reg_pred_log <- predict(reg_fit2, newdata = newdata, interval = "prediction", level = 0.95)
reg_pred <- exp(reg_pred_log)
reg_forecast <- unname(reg_pred[1, "fit"])

#ARIMA SET UP 

y <- ts(reg_data$log_ICNSA, frequency = 52, start = c(2010,1))
x_reg_covid_holiday <- as.matrix(subset(reg_data, select=c(t,covid, holiday_lag1, holiday_lead1)))

# Let's look at residuals
resid_ts <- residuals(reg_fit2)
adf.test(diff(resid_ts))
#> Warning in adf.test(diff(resid_ts)): p-value smaller than printed p-value
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  diff(resid_ts)
#> Dickey-Fuller = -12.087, Lag order = 9, p-value = 0.01
#> alternative hypothesis: stationary
kpss.test(diff(resid_ts))
#> Warning in kpss.test(diff(resid_ts)): p-value greater than printed p-value
#> 
#>  KPSS Test for Level Stationarity
#> 
#> data:  diff(resid_ts)
#> KPSS Level = 0.0072338, Truncation lag parameter = 6, p-value = 0.1

acf(resid_ts)
```

![](https://i.imgur.com/mKswlEh.png)<!-- -->

``` r
pacf(resid_ts)
```

![](https://i.imgur.com/ZkpSiSF.png)<!-- -->

``` r
ndiffs(y)
#> [1] 1
nsdiffs(y)
#> [1] 0

#ndiffs test identified a d = 1 while nsdiffs identified no seasonal differential (D= 0)
# formal test on residuals
Box.test(resid_ts, lag = 52, type = "Ljung-Box")
#> 
#>  Box-Ljung test
#> 
#> data:  resid_ts
#> X-squared = 2853.2, df = 52, p-value < 2.2e-16

#the Ljung-Box test yields a significant p-value of < 2.2e-16, leading to a rejection of the null hypothesis and concluding a correlation with the residuals 

tsdisplay(diff(resid_ts), lag.max = 156)
```

![](https://i.imgur.com/Fq57hDE.png)<!-- -->

``` r

# Appears to be a seasonal trend with decaying peaks in the ACF and PACF at lags 52,104,etc, leading to a decision to use P=1 and Q=1
tsdisplay(diff(resid_ts), lag.max = 30)
```

![](https://i.imgur.com/9eoL5A0.png)<!-- -->

``` r
#both the ACF and PACF have peaks at 1 and 2 and then stay low. This leads me to use P=2, Q=2

# Based off of the plots ACF and PACF plots, going to try a manual ARIMA model of (2,1,2)(1,0,1)
Manual_Arima <- Arima(y, order=c(2,1,2), seasonal = list(order = c(1,0,1), period = 52),
                      xreg = x_reg_covid_holiday, method = "CSS-ML")
Manual_Arima$aic
#> [1] -1095.922
checkresiduals(Manual_Arima)
```

![](https://i.imgur.com/iCwvObV.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(2,1,2)(1,0,1)[52] errors
    #> Q* = 102.49, df = 98, p-value = 0.3583
    #> 
    #> Model df: 6.   Total lags used: 104

    #My Manual_ARIMA achieved an AIC of -1096 and p-value of 0.3583 in the Ljung-Box test
    #Let's see how auto.arima does

    autoicnsa <- auto.arima(y, xreg = x_reg_covid_holiday, max.p = 5, max.d=3, max.q = 5, max.P = 5, max.D=3, max.Q = 5, trace = TRUE, approximation = TRUE)
    #> 
    #>  Fitting models using approximations to speed things up...
    #> 
    #>  Regression with ARIMA(2,0,2)(1,0,1)[52] errors : -1095.214
    #>  Regression with ARIMA(0,0,0)            errors : 58.17653
    #>  Regression with ARIMA(1,0,0)(1,0,0)[52] errors : -1084.948
    #>  Regression with ARIMA(0,0,1)(0,0,1)[52] errors : -583.3853
    #>  Regression with ARIMA(0,0,0)            errors : 5709.649
    #>  Regression with ARIMA(2,0,2)(0,0,1)[52] errors : -1082.62
    #>  Regression with ARIMA(2,0,2)(1,0,0)[52] errors : -1083.133
    #>  Regression with ARIMA(2,0,2)(2,0,1)[52] errors : Inf
    #>  Regression with ARIMA(2,0,2)(1,0,2)[52] errors : Inf
    #>  Regression with ARIMA(2,0,2)            errors : -1015.194
    #>  Regression with ARIMA(2,0,2)(0,0,2)[52] errors : Inf
    #>  Regression with ARIMA(2,0,2)(2,0,0)[52] errors : Inf
    #>  Regression with ARIMA(2,0,2)(2,0,2)[52] errors : Inf
    #>  Regression with ARIMA(1,0,2)(1,0,1)[52] errors : -1094.999
    #>  Regression with ARIMA(2,0,1)(1,0,1)[52] errors : -1096.499
    #>  Regression with ARIMA(2,0,1)(0,0,1)[52] errors : -1084.165
    #>  Regression with ARIMA(2,0,1)(1,0,0)[52] errors : -1083.842
    #>  Regression with ARIMA(2,0,1)(2,0,1)[52] errors : Inf
    #>  Regression with ARIMA(2,0,1)(1,0,2)[52] errors : Inf
    #>  Regression with ARIMA(2,0,1)            errors : -1016.669
    #>  Regression with ARIMA(2,0,1)(0,0,2)[52] errors : Inf
    #>  Regression with ARIMA(2,0,1)(2,0,0)[52] errors : Inf
    #>  Regression with ARIMA(2,0,1)(2,0,2)[52] errors : Inf
    #>  Regression with ARIMA(1,0,1)(1,0,1)[52] errors : Inf
    #>  Regression with ARIMA(2,0,0)(1,0,1)[52] errors : -1096.66
    #>  Regression with ARIMA(2,0,0)(0,0,1)[52] errors : -1086.223
    #>  Regression with ARIMA(2,0,0)(1,0,0)[52] errors : -1085.467
    #>  Regression with ARIMA(2,0,0)(2,0,1)[52] errors : Inf
    #>  Regression with ARIMA(2,0,0)(1,0,2)[52] errors : -1094.625
    #>  Regression with ARIMA(2,0,0)            errors : -1018.7
    #>  Regression with ARIMA(2,0,0)(0,0,2)[52] errors : Inf
    #>  Regression with ARIMA(2,0,0)(2,0,0)[52] errors : Inf
    #>  Regression with ARIMA(2,0,0)(2,0,2)[52] errors : Inf
    #>  Regression with ARIMA(1,0,0)(1,0,1)[52] errors : -1098.479
    #>  Regression with ARIMA(1,0,0)(0,0,1)[52] errors : -1087.521
    #>  Regression with ARIMA(1,0,0)(2,0,1)[52] errors : -1057.094
    #>  Regression with ARIMA(1,0,0)(1,0,2)[52] errors : -1096.454
    #>  Regression with ARIMA(1,0,0)            errors : -1015.918
    #>  Regression with ARIMA(1,0,0)(0,0,2)[52] errors : Inf
    #>  Regression with ARIMA(1,0,0)(2,0,0)[52] errors : Inf
    #>  Regression with ARIMA(1,0,0)(2,0,2)[52] errors : Inf
    #>  Regression with ARIMA(0,0,0)(1,0,1)[52] errors : Inf
    #>  Regression with ARIMA(0,0,1)(1,0,1)[52] errors : -586.7951
    #>  Regression with ARIMA(1,0,0)(1,0,1)[52] errors : Inf
    #> 
    #>  Now re-fitting the best model(s) without approximations...
    #> 
    #>  Regression with ARIMA(1,0,0)(1,0,1)[52] errors : -1101.566
    #> 
    #>  Best model: Regression with ARIMA(1,0,0)(1,0,1)[52] errors

    summary(autoicnsa)
    #> Series: y 
    #> Regression with ARIMA(1,0,0)(1,0,1)[52] errors 
    #> 
    #> Coefficients:
    #>          ar1    sar1     sma1  intercept       t   covid  holiday_lag1
    #>       0.9474  0.5148  -0.2203    12.9106  -8e-04  0.2481        0.1781
    #> s.e.  0.0118  0.0816   0.0898     0.2249   4e-04  0.0947        0.0350
    #>       holiday_lead1
    #>              0.0693
    #> s.e.         0.0346
    #> 
    #> sigma^2 = 0.01621:  log likelihood = 559.89
    #> AIC=-1101.77   AICc=-1101.57   BIC=-1058.84
    #> 
    #> Training set error measures:
    #>                         ME      RMSE      MAE         MPE      MAPE      MASE
    #> Training set -0.0007513378 0.1267139 0.067729 -0.01544359 0.5307888 0.2350405
    #>                      ACF1
    #> Training set -0.005538357
    checkresiduals(autoicnsa)

![](https://i.imgur.com/5xV2d0I.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,0,0)(1,0,1)[52] errors
    #> Q* = 103.14, df = 101, p-value = 0.4221
    #> 
    #> Model df: 3.   Total lags used: 104

    # using auto.arima, the ideal model predicted is ARIMA(1,0,0)(1,0,1)[52]
    # achieving an AIC of -1101.57 and p-value of 0.4221

    #AIC can not be trusted alone. The autoarima model did have a slightly smaller AIC and larger p-value. This will lead
    # me to make my forecast using the autoicnsa model of ARIMA(1,0,0)(1,0,1)[52]

    x_forecast <- matrix(c(t_target, 0, 0,0),nrow = 1, ncol = 4)

    arima_fc <- forecast(autoicnsa, h=2,
                         xreg = x_forecast, level = 95)
    #> Warning in forecast.forecast_ARIMA(autoicnsa, h = 2, xreg = x_forecast, : xreg
    #> contains different column names from the xreg used in training. Please check
    #> that the regressors are in the same order.
    arima_forecast <- exp(as.numeric(arima_fc$mean))
    arima_low <- exp(arima_fc$lower[1,1])
    arima_high <- exp(arima_fc$upper[1,1])


    cat("ICNSA Forecast for 09-26-2026 using regression model: ", reg_forecast)
    #> ICNSA Forecast for 09-26-2026 using regression model:  183569.6
    cat("ICNSA Forecast for 09-26-2026 using ARIMA(1,0,0)(1,0,1)[52] ", arima_forecast)
    #> ICNSA Forecast for 09-26-2026 using ARIMA(1,0,0)(1,0,1)[52]  150428.3
    cat("95% CI for ICNSA ARIMA Forecast: ", arima_low, "-",arima_high)
    #> 95% CI for ICNSA ARIMA Forecast:  117212.1 - 193057.6


    #####Part 2
    liquor_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MRTSSM4453USS"
    liquor_raw <- read.csv(liquor_url)
    names(liquor_raw) <- c("date", "value")
    liquor_raw$date  <- as.Date(liquor_raw$date)
    liquor_raw$value <- as.numeric(liquor_raw$value)
    liquor_raw <- na.omit(liquor_raw)

    liquor_ts <- ts(liquor_raw$value,
                    start = c(year(min(liquor_raw$date)), month(min(liquor_raw$date))),
                    frequency = 12)

    autoplot(liquor_ts) +
      ggtitle("Retail Sales: Beer, Wine, and Liquor Stores (NAICS 4453) (Seasonally Adjusted)") +
      ylab("Millions of $") + xlab("Year")

![](https://i.imgur.com/B43dvmS.png)<!-- -->

``` r


#Based on the plot above, liquor sales trend in a positive direction over the past 30 years.
#The data itself is already SEASONALLY ADJUSTED, so there is no apparent seasonal trend. Will take the log transformation 
#To create a stationary data set, we will try differentiating the data
# There is a sharp spike in sales in 2020 (COVID).

ggseasonplot(liquor_ts, year.labels = TRUE) +
  ggtitle("Seasonal plot — Beer/Wine/Liquor sales")
```

![](https://i.imgur.com/kCh3Rs0.png)<!-- -->

``` r
#This plot shows the increasing trend of sales quite nicely. It really highlights the massive jump in sales during COVID
#and that the sales continued to stay elevated afterwards. This is leading me to add a covid/post covid
#dummy variable to capture this trend.

ggmonthplot(liquor_ts)
```

![](https://i.imgur.com/k7hXqpo.png)<!-- -->

``` r
# It is important to remember that the data supplied was already seasonally adjusted. This explains why the 
# averages for all months are approximately the same. However, it might stil be useful to take into account a month
# effect when building a model.

dat <- liquor_raw |>
  mutate(
    month = factor(month(date)),
    t = row_number(),
    log_y = log(value),
    december = as.integer(month(date) == 12),
    post_covid = as.integer(date >= as.Date("2020-03-01"))
  )

#Now let's see if we can make this data stationary by differentiating the log(value)

dlogy <- diff(dat$log_y)

plot(dlogy, type = "l")
```

![](https://i.imgur.com/PwlKP7E.png)<!-- -->

``` r

adf.test(dlogy)
#> Warning in adf.test(dlogy): p-value smaller than printed p-value
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  dlogy
#> Dickey-Fuller = -7.0489, Lag order = 7, p-value = 0.01
#> alternative hypothesis: stationary
kpss.test(dlogy)
#> Warning in kpss.test(dlogy): p-value greater than printed p-value
#> 
#>  KPSS Test for Level Stationarity
#> 
#> data:  dlogy
#> KPSS Level = 0.052263, Truncation lag parameter = 5, p-value = 0.1

# the differentiated log values passed the stationary test, so will use a d = 1 for subsequent analyses

#A basic lm regression model
candidate_lm <- lm(log_y ~ t + month + post_covid, data = dat)
acf(diff(residuals(candidate_lm)), lag.max = 36, main = "ACF: regression residuals")
```

![](https://i.imgur.com/EIQhxiy.png)<!-- -->

``` r
pacf(diff(residuals(candidate_lm)), lag.max=36, main = "PACF: regression residuals")
```

![](https://i.imgur.com/9djIXzI.png)<!-- -->

``` r
checkresiduals(diff(residuals(candidate_lm)))
```

![](https://i.imgur.com/P7e8ivF.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals
    #> Q* = 110.79, df = 10, p-value < 2.2e-16
    #> 
    #> Model df: 0.   Total lags used: 10
    #looking at the ACF and PACF, there appears to be a significant peak at lag 1 for the ACF and lags 1 and 2
    #for the PACF. This suggests an AR(1) and MA(2). There also does not appear to be any real seasonal peaks at
    #12, 24, 36, so will not use a seasonal order for my arima model.

    #Building the x matrix using trend, month, and post_covid variables
    x_reg_liquor <- model.matrix(~ t + month + post_covid, data = dat)[, -1]

    candid_arima <- Arima(dat$log_y, order=c(1,0,2),
                    xreg = x_reg_liquor, method = "CSS-ML")
    candid_arima$aicc
    #> [1] -2409.408
    checkresiduals(candid_arima)

![](https://i.imgur.com/VtmLdf7.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,0,2) errors
    #> Q* = 5.1542, df = 7, p-value = 0.6412
    #> 
    #> Model df: 3.   Total lags used: 10
    #Using the traditional checkresiduals function, our p-value is 0.641 and uses a lag of 10. It worth mentioning that the minimum number of lags for seasonal data
    # should be 2 x frequency of interval (2 x 12 months = 24).However, I did not identify any notable seasonal component to my model (mainly taken into account through the month variables
    # and the fact that the data was already seasonally adjusted). On the ACF plot, there are still significant peaks at lags 12 and 24, suggesting a possible seasonal
    # AR(1)


    candid <- Arima(dat$log_y, order=c(1,0,2), seasonal = list(order=c(1,0,0), period=12), 
                    xreg = x_reg_liquor, method = "CSS-ML")
    checkresiduals(candid)

![](https://i.imgur.com/YS5xwzp.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,0,2)(1,0,0)[12] errors
    #> Q* = 5.987, df = 6, p-value = 0.4246
    #> 
    #> Model df: 4.   Total lags used: 10
    #Adding in an AR(1) seasonal affect to the ARIMA produced approximately the same AIC, but decreased our p-value to 0.4246,
    # therfore I will proceed with my original candidate. Now let's see what auto.arima finds

    automatic<- auto.arima(dat$log_y, xreg = x_reg_liquor)

    automatic$aicc
    #> [1] -2409.408
    checkresiduals(automatic)

![](https://i.imgur.com/ZL0YxCQ.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,0,2) errors
    #> Q* = 5.1542, df = 7, p-value = 0.6412
    #> 
    #> Model df: 3.   Total lags used: 10

    #The automatic model chose the same ARIMA model that I originally hypothesized. Therefore, I will use the original candidate
    #model to make my forecast

    #Forecast for December 2026 liquor sales
    tail(x_reg_liquor)
    #>       t month2 month3 month4 month5 month6 month7 month8 month9 month10 month11
    #> 410 410      1      0      0      0      0      0      0      0       0       0
    #> 411 411      0      1      0      0      0      0      0      0       0       0
    #> 412 412      0      0      1      0      0      0      0      0       0       0
    #> 413 413      0      0      0      1      0      0      0      0       0       0
    #> 414 414      0      0      0      0      1      0      0      0       0       0
    #> 415 415      0      0      0      0      0      1      0      0       0       0
    #>     month12 post_covid
    #> 410       0          1
    #> 411       0          1
    #> 412       0          1
    #> 413       0          1
    #> 414       0          1
    #> 415       0          1
    #The last entry is row 415 for July 2026. Therefore, December 2026 would be in row 420
    x_liq_forecast <- matrix(c(420,0,0,0,0,0,0,0,0,0,0,1,1), nrow=1)


    arima_liq_fc <- forecast(candid_arima, h=5,
                         xreg = x_liq_forecast, level = 95)
    #> Warning in forecast.forecast_ARIMA(candid_arima, h = 5, xreg = x_liq_forecast,
    #> : xreg contains different column names from the xreg used in training. Please
    #> check that the regressors are in the same order.
    arima_liq_forecast <- exp(as.numeric(arima_liq_fc$mean))
    arima_liq_low <- exp(arima_liq_fc$lower[1,1])
    arima_liq_high <- exp(arima_liq_fc$upper[1,1])


    cat("Final Liquor Sales Forecast (in millions of $) for December 2026 based on ARIMA(1,0,2): ", arima_liq_forecast)
    #> Final Liquor Sales Forecast (in millions of $) for December 2026 based on ARIMA(1,0,2):  6136.006
    cat("95% Confidence Interval for Forecast: ", arima_liq_low, "-", arima_liq_high)
    #> 95% Confidence Interval for Forecast:  5982.637 - 6293.307

<sup>Created on 2026-09-23 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
