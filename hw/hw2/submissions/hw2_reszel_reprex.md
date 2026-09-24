``` r
library(ggplot2)
library(forecast)
library(fpp3)
#> ── Attaching packages ──────────────────────────────────────────── fpp3 1.0.3 ──
#> ✔ tibble      3.3.1     ✔ tsibbledata 0.4.1
#> ✔ dplyr       1.2.1     ✔ ggtime      1.0.0
#> ✔ tidyr       1.3.2     ✔ feasts      0.5.0
#> ✔ lubridate   1.9.5     ✔ fable       0.5.0
#> ✔ tsibble     1.2.0
#> ── Conflicts ───────────────────────────────────────────────── fpp3_conflicts ──
#> ✖ lubridate::date()    masks base::date()
#> ✖ dplyr::filter()      masks stats::filter()
#> ✖ tsibble::intersect() masks base::intersect()
#> ✖ tsibble::interval()  masks lubridate::interval()
#> ✖ dplyr::lag()         masks stats::lag()
#> ✖ tsibble::setdiff()   masks base::setdiff()
#> ✖ tsibble::union()     masks base::union()
library(tidyverse)
library(tsbox)
library(lubridate)
library(reprex)
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
library(seasonal)
#> 
#> Attaching package: 'seasonal'
#> The following object is masked from 'package:tibble':
#> 
#>     view
library(astsa)
#> 
#> Attaching package: 'astsa'
#> The following objects are masked from 'package:seasonal':
#> 
#>     trend, unemp
#> The following object is masked from 'package:forecast':
#> 
#>     gas
```

# Problem 1

## Part 1/2

The first series to load in is the required initial claims series. As the covariates, we will use the non-seasonally adjusted ‘Continued Claims (Insured Unemployment)’ and the St. Louis Fed Financial Stress Index, which measures the “degree of financial stress in the market”. Clearly, continued unemployment claims and initial unemployment claims are correlated. The financial stress index could be a good predictor of unemployment claims, as high levels of stress about the economy may lead to companies making layoffs.

The financial stress index is measured/released one day before continued claims, so we will have the measurements 1 day apart correspond to the same value of ‘time’. We will always have the data we need to make a prediction if we use at least lag 1.

We are just going to consider the data since 2010, since the economy prior to then might be non-representative of the economy in more recent years. This could be for a variety of reasons, such as expanding technology, world events such as the 2008 financial crisis, continued offshoring/automation, etc.

``` r
initial_claims <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=ICNSA", stringsAsFactors = FALSE) 
cont_claims <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=CCNSA", stringsAsFactors = FALSE) 
STLFSI <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=STLFSI4",
                      stringsAsFactors = FALSE) 

claims_data <- left_join(initial_claims, cont_claims, by = "observation_date")

claims_data <- claims_data[claims_data$observation_date >= '2010-1-1' &
                                     claims_data$observation_date < '2026-9-26',]
claims_data$observation_date <- as.Date(claims_data$observation_date)

claims_data$time <- as.numeric(claims_data$observation_date - min(claims_data$observation_date))

STLFSI <- STLFSI[STLFSI$observation_date >= '2010-1-1' &
                                     STLFSI$observation_date < '2026-9-26',]
STLFSI$observation_date <- as.Date(STLFSI$observation_date)
STLFSI$time <- as.numeric(STLFSI$observation_date - min(STLFSI$observation_date))


claims_data <- left_join(claims_data, STLFSI, by="time")

ggplot(claims_data, aes(x = time)) + 
  geom_line(aes(y = ICNSA, color = 'blue')) +
  labs(x = "Days since first obs in 2010", y = "Values") +
  theme_minimal()
```

![](https://i.imgur.com/6LHWptT.png)<!-- -->

``` r

ggplot(claims_data, aes(x = time)) + 
  geom_line(aes(y = CCNSA, color = 'red')) +
  labs(x = "Days since first obs in 2010", y = "Values") +
  theme_minimal()
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).
```

![](https://i.imgur.com/Rlt4qNk.png)<!-- -->

``` r

ggplot(claims_data, aes(x = time)) + 
  geom_line(aes(y = STLFSI4, color = 'green')) +
  labs(x = "Days since first obs in 2010", y = "Values") +
  theme_minimal()
```

![](https://i.imgur.com/wQWMc86.png)<!-- -->
For both the main outcome and the continued claim covariate, it seems that variation in the response increases as the number of claims increases. This inclines us to take the log of both variables. Since they are both inherently strictly positive, this is not a numerical issue. There appears to be some very minor negative trend in both plots when considering the non-Covid period, so it is not a completely flat trend. Since the Financial Stress Index doesn’t appear to have drastically changing variance and is on a smaller scale, we will not take the log. Also, as on the previous homework, we will add a dummy variable to account for the massive spike during Covid. The dates for Covid come the beginning of the Covid spike and end when the trajectory follows the pre-Covid levels. Since deviation from the series regular trajectory due to Covid is so large, these covariates will help account for some of that variance.

Here are some plots to justify our use of the covariates.

``` r
claims_data$covid <- '2020-3-14' <= claims_data$observation_date.x & 
  claims_data$observation_date.x < '2021-5-01'

claims_data$logICNSA <- log(claims_data$ICNSA)
claims_data$logCCNSA <- log(claims_data$CCNSA)

ggplot(claims_data, aes(x = time)) + 
  geom_line(aes(y = logICNSA, color = 'blue')) +
  geom_line(aes(y = logCCNSA, color = 'red'))
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).
```

![](https://i.imgur.com/nNILs4K.png)<!-- -->

``` r
  labs(x = "Days since first obs in 2010", y = "Values", color = "Legend") +
  theme_minimal()
#> NULL

ggplot(claims_data, aes(x = logICNSA, y = logCCNSA)) +
  geom_point()
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
```

![](https://i.imgur.com/X28Xxju.png)<!-- -->

``` r

ggplot(claims_data, aes(x = logICNSA, y = STLFSI4)) +
  geom_point()
```

![](https://i.imgur.com/8lFl3wg.png)<!-- -->

From the regular plot of initial claims and continued claims, we can see they in general follow the same trajectory. As one increases, the other tends to increase also. The scatter plot shows a strong positive trend between the two variables. For the scatter plot of initial claims and financial stress index, there seems to be a very weak positive correlation. Once we look at the CCF plot we will see why this covariate is useful.

``` r
ccf_data <- claims_data
ccf_data <- ccf_data %>% filter(!is.na(logICNSA), !is.na(logCCNSA), !is.na(STLFSI4))


ccf_claims <- ccf(diff(ccf_data$logICNSA), diff(ccf_data$logCCNSA), lag.max = 24, plot = TRUE,
               main = "Change in log inital claims vs change in log continued claims")
```

![](https://i.imgur.com/bHcvWHN.png)<!-- -->

``` r
ccf_help <- ccf(diff(ccf_data$logICNSA), diff(ccf_data$STLFSI4), lag.max = 24, plot = TRUE,
               main = "Change in log inital claims vs change in log temporary workers")
```

![](https://i.imgur.com/MIgScU3.png)<!-- -->

``` r

claims_data$lag2_logCCNSA <- lag(claims_data$logCCNSA, 2)
claims_data$lag_STLFSI4 <- lag(claims_data$STLFSI4, 1)
```

Before looking at the CCF plot, we must recognize that the data for continued claims is one week behind the initial claims in terms of when it is released. By necessity, we must use at least two week lag for that covariate if we want to make a prediction. We see the highest level of correlation (beyond 1) considering lag 2 from the plot, so that is what we will use for the covariate.
For the financial stress index, we can see the largest peak at lag 1, so this is what we will use.

## Part 3

``` r
ols_model <- lm(logICNSA ~ lag2_logCCNSA +lag_STLFSI4 + covid, data=claims_data)
ols_residuals <- ols_model$residuals

tsdisplay(ols_residuals)
```

![](https://i.imgur.com/Y1gkFyO.png)<!-- -->

``` r
adf.test(ols_residuals)
#> Warning in adf.test(ols_residuals): p-value smaller than printed p-value
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  ols_residuals
#> Dickey-Fuller = -8.402, Lag order = 9, p-value = 0.01
#> alternative hypothesis: stationary
acf(ols_residuals, lag.max = 200)
```

![](https://i.imgur.com/DEnjiO6.png)<!-- -->

``` r

acf_r <- Acf(ols_residuals, lag.max = 4 * 52, plot = FALSE)
seas_lags <- seq(52, 4 * 52, by = 52)
data.frame(lag = seas_lags, acf = round(acf_r$acf[seas_lags + 1], 3))
#>   lag   acf
#> 1  52 0.421
#> 2 104 0.329
#> 3 156 0.264
#> 4 208 0.300

r_sd <- diff(ols_residuals, lag = 52)
acf(r_sd, lag.max=200)
```

![](https://i.imgur.com/3kLCN3j.png)<!-- -->

``` r
pacf(r_sd, lag.max=200)
```

![](https://i.imgur.com/74ehVR1.png)<!-- -->

``` r

r_sd_d <- diff(r_sd)
tsdisplay(r_sd_d)
```

![](https://i.imgur.com/wxsIYT7.png)<!-- -->

``` r
acf(r_sd_d, lag.max=200)
```

![](https://i.imgur.com/nI3w1Qx.png)<!-- -->

``` r
pacf(r_sd_d, lag.max=200)
```

![](https://i.imgur.com/rcyvqV4.png)<!-- -->
The plot of residuals appears mostly stationary, and the ADF test provides more evidence for this.
The ACF plot shows very slow decay, and amplitude seems to be greater at the seasonal marks (52 weeks, 104 weeks, and so on). After applying a seasonal differencing, the ACF/PACF still slowly decay for the seasonal lags, indicating we should take a regular difference. This fixes that issue. We then examine these final plots to decide our initial model’s p,q,P, and Q. We see a sharp cutoff in the ACF after one lag, while the PACF more slowly decays. This indicates we could try p=0, q=1, which is a MA(1) model. For the seasonal lags, the ACF shows a cutoff after 1 seasonal lag, while the PACF shows a more slow decay. So we will try P=0, Q=1. So our overall inital model will be regSARIMA(0,1,1), (0,1,1) with our indicated covariates.

## Part 4

``` r
y1 <- ts(claims_data$logICNSA, frequency = 52, start = c(2010, 1))
x_covid <- matrix(as.numeric(claims_data$covid),
                         ncol = 1, dimnames = list(NULL, "covid"))
x_logCCNSA <- matrix(claims_data$lag2_logCCNSA ,ncol=1, dimnames = list(NULL, "logCCNSA") )
x_STLFSI4 <- matrix(claims_data$lag_STLFSI4,
                         ncol=1, dimnames = list(NULL, "STLFSI4") )
x_covs <- cbind(x_covid, x_logCCNSA, x_STLFSI4)

regarima_manual <- Arima(y1, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=52),
                               xreg = x_covs, method = "CSS-ML")
```

## Part 5

First, we will try the auto ARIMA function and see what the model it produces is like.

``` r
regarima_auto <- auto.arima(y1, xreg=x_covs)
summary(regarima_auto)
#> Series: y1 
#> Regression with ARIMA(0,1,0)(1,0,0)[52] errors 
#> 
#> Coefficients:
#>         sar1   covid  logCCNSA  STLFSI4
#>       0.3585  0.1274   -0.3184   0.1376
#> s.e.  0.0329  0.0840    0.0793   0.0170
#> 
#> sigma^2 = 0.01585:  log likelihood = 539.66
#> AIC=-1069.33   AICc=-1069.26   BIC=-1045.72
#> 
#> Training set error measures:
#>                         ME      RMSE        MAE         MPE     MAPE      MASE
#> Training set -0.0008909884 0.1256774 0.07457183 -0.01330496 0.585349 0.2514367
#>                     ACF1
#> Training set -0.05236682

aicc_manual <- regarima_manual$aicc
aicc_auto <- regarima_auto$aicc

aicc_manual
#> [1] -904.5045
aicc_auto
#> [1] -1069.256

checkresiduals(regarima_manual)
```

![](https://i.imgur.com/9lrHr5X.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(0,1,1)(0,1,1)[52] errors
    #> Q* = 135.04, df = 102, p-value = 0.01588
    #> 
    #> Model df: 2.   Total lags used: 104
    checkresiduals(regarima_auto)

![](https://i.imgur.com/DB688OQ.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(0,1,0)(1,0,0)[52] errors
    #> Q* = 135.69, df = 103, p-value = 0.01707
    #> 
    #> Model df: 1.   Total lags used: 104

The model produced by RegARIMA is (0,1,0)(1,0,0), which does not agree with our model. This model is actually much simpler than the already simple model we provided. It only includes a regular difference and a AR(1) seasonal part. It produces a significantly better AICC. However, both of the models fail the Ljung-Box test. We will perform a grid search to see if we can find a model that performs better in terms of residual diagnostics.I initially included more values, but removed options that had very small p-values for the Ljung-Box test.

``` r
candidates <- list(
  c(1,0,0,1,0,0),
  c(0,1,1,1,0,0),
  c(1,0,0,1,0,1),
  c(0,1,1,1,0,1),
  c(1,1,0,0,1,1),
  c(0,1,1,0,1,1)
)

candidates <- as.matrix(candidates)

fit_one <- function(cc) {
  tryCatch(
    Arima(
      y1,
      order = c(cc[1], cc[2], cc[3]),
      seasonal = list(
        order = c(cc[4], cc[5], cc[6]),
        period = 52
      ),
      xreg = x_covs,
      method = "ML"
    ),
    error = function(e) NULL
  )
}

fits <- lapply(seq_len(nrow(candidates)), function(i) {
  fit_one(candidates[i, ])
})

fits <- lapply(candidates, fit_one)
keep <- !sapply(fits, is.null)
candidates <- candidates[keep]
fits <- fits[keep]

candidate_table <- do.call(rbind, Map(function(cc, fit) {
  lb <- Box.test(residuals(fit), lag = 104,
                  fitdf = sum(cc[c(1, 3, 4, 6)]), type = "Ljung-Box")
  data.frame(
    model = sprintf("(%d,%d,%d)(%d,%d,%d)[52]", cc[1], cc[2], cc[3], cc[4], cc[5], cc[6]),
    AICc = round(fit$aicc, 2),
    BIC  = round(fit$bic, 2),
    ljung_box_p = round(lb$p.value, 3)
  )
}, candidates, fits))

candidate_table
#>                model     AICc      BIC ljung_box_p
#> 1 (1,0,0)(1,0,0)[52] -1055.92 -1022.99       0.015
#> 2 (0,1,1)(1,0,0)[52] -1071.97 -1043.74       0.011
#> 3 (1,0,0)(1,0,1)[52] -1061.37 -1023.76       0.048
#> 4 (0,1,1)(1,0,1)[52] -1082.30 -1049.39       0.118
#> 5 (1,1,0)(0,1,1)[52]  -900.54  -872.71       0.022
#> 6 (0,1,1)(0,1,1)[52]  -904.50  -876.67       0.016

final_model_claims <- Arima(y1, order=c(0,1,1), seasonal=list(order=c(1,0,1), period=52),
                               xreg = x_covs, method = "CSS-ML")
checkresiduals(final_model_claims)
```

![](https://i.imgur.com/DEsI7hm.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(0,1,1)(1,0,1)[52] errors
    #> Q* = 118.05, df = 101, p-value = 0.1181
    #> 
    #> Model df: 3.   Total lags used: 104

    shapiro.test(final_model_claims$residuals)
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  final_model_claims$residuals
    #> W = 0.72142, p-value < 2.2e-16

We can see that the model (0,1,1)(1,0,1) has a smaller AICC than both of the models we found, and it produces a non-significant p-value for the Ljung-Box test. Since this means the model meets the assumptions of the ARIMA model given the residual diagnostic test and has a better AICC, we will use this model to make our final prediction. The actual residuals themselves seem somewhat reasonable, although there are 2 outliers likely due to Covid that are not properly accounted for. It is extremely challenging to model this Covid spike effectively. The one potential issue is that the Shapiro-Wilk test for normality of the residuals strongly rejects.

Now, we can make the final prediction for part 1:

``` r
cc_cur <- claims_data$logCCNSA[claims_data$observation_date.x == '2026-09-05']
fsi_cur <- claims_data$STLFSI4[claims_data$observation_date.y == '2026-09-11']
claims_fc <- forecast(final_model_claims, h = 1,
                     xreg = matrix(c(0,cc_cur,fsi_cur), nrow = 1, ncol = 3,
                                   dimnames = list(NULL, c('covid','logCCNSA',
                                                           'STLFSI4'))),level = 95)
model1_pred <- exp(as.numeric(claims_fc$mean))

model1_pred
#> [1] 151720.9
```

# Problem 2

First, we will load in the data and examine it.

``` r
alc_sales <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=MRTSSM4453USN",
stringsAsFactors = FALSE) 

alc_sales <- data.frame(time  = as.Date(alc_sales$observation_date),
                          value = alc_sales$MRTSSM4453USN)

ggplot(data= alc_sales, aes(x = time, y = value)) +
  geom_line() +
  labs(title = "Retail Sales: Beer, Wine, and Liquor Stores",
       y = "Sales")
```

![](https://i.imgur.com/7MRa8pb.png)<!-- -->

``` r

alc_sales$value <- log(alc_sales$value)

alc_tsibble <- alc_sales %>%
  mutate(time = yearmonth(time)) %>%
  as_tsibble(index = time) 

alc_trend <- ts_trend(alc_sales)
ts_plot(ts_c(alc_trend, alc_sales))
```

![](https://i.imgur.com/o3i75FD.png)<!-- -->

``` r

gg_season(alc_tsibble)
#> Plot variable not specified, automatically selected `y = value`
```

![](https://i.imgur.com/Ct0aFfI.png)<!-- -->

``` r

alc_sales$month <- month(alc_sales$time)

alc_sales |>
  ggplot(aes(x=time, y=value, group=month)) +
  geom_line(aes(col=month)) +
  facet_grid(month ~ ., scales='free')
```

![](https://i.imgur.com/qSCFPj1.png)<!-- -->

``` r

alc_sales$covid_spike <- alc_sales$time >= '2020-03-01'
```

First, we decided to take log of the outcome, since it seems the variation in the data is greater at later time points. This may help to stabilize the variance a bit. Since all of the outcomes are positive, this will not lead to computational issues.

From these plots, we can observe a clearly increasing trend as time increases. Some of this increasing trend is likely due to inflation over time. We could adjust this series by inflation, but we will account for this by using trend/differencing instead. Since our goal is to predict alcohol sales in December 2026, we would likely need info about inflation in the coming months, which would add a significant level of uncertainty to our forecast. There is also a sharp increase in sales in early 2020, likely due to the Covid pandemic. It makes sense that as restaurants/bars closed, more people were purchasing alcohol to drink at home. We will include a dummy variable to account for this jump.

Some of the other plots are used to consider a seasonal component. These were somewhat hard to interpret. We know, in general, that alcohol sales are highest in December. Depending on how the ACF/pACF plots look in terms of seasonality, we may include a dummy variable for December.

Now, we will examine some plots to check on the structure for the regression errors. Clearly due to the trend and seasonal structure, this time series is not stationary. We can consider seasonal or regular differencing.

``` r
ols_model2 <- lm(value ~ covid_spike, data=alc_sales)
ols_residuals2 <- ols_model2$residuals

tsdisplay(ols_residuals2)
```

![](https://i.imgur.com/nfBiN4C.png)<!-- -->

``` r

d_res <- diff(ols_residuals2)
tsdisplay(d_res, lag.max=60)
```

![](https://i.imgur.com/8SQdj4I.png)<!-- -->

``` r
acf(d_res, lag.max=60)
```

![](https://i.imgur.com/bSDX3Me.png)<!-- -->

``` r
pacf(d_res, lag.max=60)
```

![](https://i.imgur.com/7GXApXn.png)<!-- -->

``` r

sd_res <-diff(ols_residuals,12)
tsdisplay(sd_res, lag.max=60)
```

![](https://i.imgur.com/CG43jnI.png)<!-- -->

``` r

adf.test(sd_res)
#> Warning in adf.test(sd_res): p-value smaller than printed p-value
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  sd_res
#> Dickey-Fuller = -13.272, Lag order = 9, p-value = 0.01
#> alternative hypothesis: stationary
adf.test(d_res)
#> Warning in adf.test(d_res): p-value smaller than printed p-value
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  d_res
#> Dickey-Fuller = -9.8586, Lag order = 7, p-value = 0.01
#> alternative hypothesis: stationary

y_2 <- ts(alc_sales$value, frequency = 12, start = c(1992, 1))
ndiffs(y_2)
#> [1] 1
nsdiffs(y_2)
#> [1] 1

sd_d_res <- diff(sd_res)
tsdisplay(sd_d_res, lag.max=60)
```

![](https://i.imgur.com/E21GVsv.png)<!-- -->

``` r
acf(sd_d_res,lag.max=48)
```

![](https://i.imgur.com/kTC8Rp6.png)<!-- -->

``` r
pacf(sd_d_res,lag.max=48)
```

![](https://i.imgur.com/voUmeSB.png)<!-- -->
After differencing once, we see that there are very slowly decreasing peaks for the seasonal lags, indicating that seasonal differencing might be needed. Seasonal differecning on its own doesn’t seem to eliminate any of the seasonal fluctuations in the ACF plot or in the plot of residuals. Tests for recommended number of seasonal and regular differences both indicate we should take 1 differnce for each. Looking at the acf/pacf plots produced when we do that, we see a cutoff after 1 regular lag and 1 seasonal lag in the ACF. For the PACF, we see also see a cutoff after 1 lag and seasonal lags that slowly decrease. This indicates us to consider p=1, P=0, q=1, Q=1, d=1, D=1.

Here is the inital model that will be fit.

``` r
x_covid_spike <- matrix(as.numeric(alc_sales$covid_spike),
                         ncol = 1, dimnames = list(NULL, "CovidSpike"))
x_covs2 <- x_covid_spike

alc_model_manual <- Arima(y_2, order=c(1,1,1),seasonal=list(order=c(0,1,1), period=12), xreg=x_covs2, method = "CSS-ML")
summary(alc_model_manual)
#> Series: y_2 
#> Regression with ARIMA(1,1,1)(0,1,1)[12] errors 
#> 
#> Coefficients:
#>           ar1      ma1     sma1  CovidSpike
#>       -0.2564  -0.6298  -0.8241      0.1073
#> s.e.   0.0626   0.0500   0.0333      0.0167
#> 
#> sigma^2 = 0.0006466:  log likelihood = 900.99
#> AIC=-1791.99   AICc=-1791.84   BIC=-1772.01
#> 
#> Training set error measures:
#>                         ME       RMSE        MAE           MPE      MAPE
#> Training set -0.0001014101 0.02490178 0.01989024 -0.0005876572 0.2482873
#>                   MASE        ACF1
#> Training set 0.4587425 -0.02139976
```

Now we will check it using the numerical search, then compare the two models using AICc and residual diagnostics.

``` r
alc_model_auto <- auto.arima(y_2, xreg=x_covs2)
summary(alc_model_auto)
#> Series: y_2 
#> Regression with ARIMA(3,1,2)(1,1,2)[12] errors 
#> 
#> Coefficients:
#>           ar1     ar2     ar3      ma1      ma2     sar1     sma1     sma2
#>       -0.1531  0.2078  0.3955  -0.6545  -0.2058  -0.0015  -0.6134  -0.2193
#> s.e.   0.1828  0.1021  0.0748   0.1846   0.1188   0.1891   0.1820   0.1448
#>       CovidSpike
#>           0.0979
#> s.e.      0.0171
#> 
#> sigma^2 = 0.0005762:  log likelihood = 927.03
#> AIC=-1834.06   AICc=-1833.5   BIC=-1794.1
#> 
#> Training set error measures:
#>                        ME       RMSE        MAE          MPE      MAPE
#> Training set 1.474245e-05 0.02335848 0.01863481 0.0008245365 0.2321725
#>                   MASE        ACF1
#> Training set 0.4297878 0.006592797

alc_model_manual$aicc
#> [1] -1791.837
alc_model_auto$aicc
#> [1] -1833.499

checkresiduals(alc_model_manual)
```

![](https://i.imgur.com/6DvGOON.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(1,1,1)(0,1,1)[12] errors
    #> Q* = 280.92, df = 21, p-value < 2.2e-16
    #> 
    #> Model df: 3.   Total lags used: 24
    checkresiduals(alc_model_auto)

![](https://i.imgur.com/DmO3cfb.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(3,1,2)(1,1,2)[12] errors
    #> Q* = 89.969, df = 16, p-value = 2.535e-12
    #> 
    #> Model df: 8.   Total lags used: 24

The auto.arima function picks a model that is (3,1,2)(1,1,2). This model has a more complex structure in that it selects higher values for p, q, P and Q. It does have a smaller AICC value, so it may be better in that regard. Regardless, both of these models strongly reject for the Ljung-Box test. The actual ACF plot looks better for the auto.arima model. However, in both cases, there are a larger number of lags that are significant. Since independence of residuals is an important assumption of the ARIMA model, we will attempt to find a model specification that improves upon the residual diagnostics.

``` r
candidates2 <- list(
  c(1,0,1,1,1,1),
  c(2,0,0,1,1,1),
  c(2,0,1,1,1,1),
  c(2,0,2,1,1,1),
  c(1,0,1,0,1,1),
  c(2,0,0,0,1,1),
  c(2,0,1,0,1,1),
  c(2,0,2,0,1,1),
  c(3,0,1,1,1,1),
  c(3,0,1,1,1,0),
  c(3,0,1,0,1,1),
  c(1,0,1,1,1,2),
  c(2,0,0,1,1,2),
  c(2,0,1,1,1,2),
  c(2,0,2,1,1,2),
  c(1,0,1,0,1,2),
  c(2,0,0,0,1,2),
  c(2,0,1,0,1,2),
  c(2,0,2,0,1,2),
  c(3,0,1,1,1,2),
  c(3,0,1,1,1,2),
  c(3,0,1,0,1,2),
  c(1,1,1,1,1,1),
  c(2,1,0,1,1,1),
  c(2,1,1,1,1,1),
  c(2,1,2,1,1,1),
  c(1,1,1,0,1,1),
  c(2,1,0,0,1,1),
  c(2,1,1,0,1,1),
  c(2,1,2,0,1,1),
  c(3,1,1,1,1,1),
  c(3,1,1,1,1,0),
  c(3,1,1,0,1,1),
  c(1,1,1,1,1,2),
  c(2,1,0,1,1,2),
  c(2,1,1,1,1,2),
  c(2,1,2,1,1,2),
  c(1,1,1,0,1,2),
  c(2,1,0,0,1,2),
  c(2,1,1,0,1,2),
  c(2,1,2,0,1,2),
  c(3,1,1,1,1,2),
  c(3,1,1,1,1,2),
  c(3,1,1,0,1,2)
)

fit_one2 <- function(cc) {
  tryCatch(
    Arima(
      y_2,
      order = c(cc[1], cc[2], cc[3]),
      seasonal = list(
        order = c(cc[4], cc[5], cc[6]),
        period = 12
      ),
      xreg = x_covs2,
      method = "CSS-ML"
    ),
    error = function(e) NULL
  )
}

fits2 <- lapply(candidates2, fit_one2)
keep2 <- !vapply(fits2, is.null, logical(1))
candidates2 <- candidates2[keep2]
fits2 <- fits2[keep2]

candidate_table2 <- do.call(rbind, Map(function(cc, fit) {
  lb <- Box.test(residuals(fit), lag = 24,
                 fitdf = sum(cc[c(1, 3, 4, 6)]), type = "Ljung-Box")
  data.frame(
    model = sprintf("(%d,%d,%d)(%d,%d,%d)[12]", cc[1], cc[2], cc[3], cc[4], cc[5], cc[6]),
    AICc = round(fit$aicc, 2),
    BIC  = round(fit$bic, 2),
    ljung_box_p = round(lb$p.value, 3)
  )
}, candidates2, fits2))

candidate_table2[candidate_table2$ljung_box_p > 0]
#> data frame with 0 columns and 40 rows
```

None of these models produce a large p-value for the Ljung-Box test. Possibly the covariate from Covid is inappropriate. I couldn’t find any other way to improve it, and I tried other ways of modeling the covid period or including \# of weekend days per month. Here, we check on the Auto-Arima model without that covariate. I also tried a model using a fourier series instead of a seasonal component.

``` r
alc_model_nocovid <- auto.arima(y_2)
summary(alc_model_nocovid)
#> Series: y_2 
#> ARIMA(3,0,1)(1,1,2)[12] with drift 
#> 
#> Coefficients:
#>          ar1     ar2     ar3     ma1    sar1     sma1     sma2   drift
#>       0.0204  0.3776  0.4710  0.2125  0.0262  -0.6155  -0.2220  0.0033
#> s.e.  0.1033  0.0565  0.0583  0.1147  0.1807   0.1743   0.1384  0.0002
#> 
#> sigma^2 = 0.0006055:  log likelihood = 919.09
#> AIC=-1820.17   AICc=-1819.72   BIC=-1784.18
#> 
#> Training set error measures:
#>                         ME       RMSE        MAE          MPE      MAPE
#> Training set -0.0002276681 0.02400588 0.01878731 -0.003878494 0.2340337
#>                   MASE        ACF1
#> Training set 0.4333049 0.005371536
checkresiduals(alc_model_nocovid)
```

![](https://i.imgur.com/8yZsD8N.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from ARIMA(3,0,1)(1,1,2)[12] with drift
    #> Q* = 88.75, df = 17, p-value = 1.03e-11
    #> 
    #> Model df: 7.   Total lags used: 24


    fit_alt <- auto.arima(y_2, seasonal = FALSE,
                          xreg = cbind(x_covs2, fourier(y_2, K = 6)),
                          stepwise = FALSE, approximation = FALSE)
    summary(fit_alt)
    #> Series: y_2 
    #> Regression with ARIMA(5,1,0) errors 
    #> 
    #> Coefficients:
    #>           ar1      ar2      ar3      ar4     ar5   drift  CovidSpike    S1-12
    #>       -0.8394  -0.5948  -0.2116  -0.2392  -0.122  0.0029      0.1177  -0.0555
    #> s.e.   0.0488   0.0637   0.0697   0.0638   0.049  0.0004      0.0170   0.0014
    #>        C1-12    S2-12   C2-12    S3-12   C3-12    S4-12   C4-12   S5-12   C5-12
    #>       0.0094  -0.0329  0.0832  -0.0316  0.0704  -0.0128  0.0591  0.0123  0.0686
    #> s.e.  0.0014   0.0013  0.0013   0.0012  0.0012   0.0022  0.0022  0.0016  0.0016
    #>        C6-12
    #>       0.0215
    #> s.e.  0.0009
    #> 
    #> sigma^2 = 0.0006117:  log likelihood = 952.92
    #> AIC=-1867.84   AICc=-1865.91   BIC=-1791.35
    #> 
    #> Training set error measures:
    #>                         ME       RMSE        MAE           MPE      MAPE
    #> Training set -1.560523e-05 0.02416076 0.01938546 -0.0006411774 0.2417118
    #>                   MASE        ACF1
    #> Training set 0.4471004 0.006151189
    checkresiduals(fit_alt)

![](https://i.imgur.com/DAFmG0j.png)<!-- -->

    #> 
    #>  Ljung-Box test
    #> 
    #> data:  Residuals from Regression with ARIMA(5,1,0) errors
    #> Q* = 116.47, df = 19, p-value = 5.551e-16
    #> 
    #> Model df: 5.   Total lags used: 24

The AICC is only somewhat larger than the model auto.arima found when we included the covid covariate. The residual diagnostics are a bit better. Even though the Ljung-Box test strongly rejects, the ACF values are somewhat smaller in general. I also tried to remove the seasonal component and add a Fourier part to capture the seasonality, but this didn’t lead to improve residual diagnostics, although it did improve the AICC. In fact, there are very large seasonal lags for the residuals.

For my final model, I’m going to choose the auto.arima model without the covid covariate. While the residual diagnostics were still not great, this had the best blend of small AICC, a somewhat simple structure, and a better ACF plot of residuals than most of the other models. This also had an AICC that was better than the manually chosen regARIMA model.

``` r
final_alc_model <- alc_model_nocovid 

alc_fc <- forecast(final_alc_model, h = 5, level = 95)
alc_pred <- exp(as.numeric(alc_fc$mean))
```

Here is the final forecast for the alcohol sales of December 2026:

``` r
alc_prediction <- alc_pred[5]
```

So, printed out as the final answer, the first output is for the predicted claims and the second output is for the predicted alcohol sales.

``` r
model1_pred
#> [1] 151720.9
alc_prediction
#> [1] 8170.115
```

<sup>Created on 2026-09-23 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
