``` r
############################################
# Austin Reszel                            #
# Time Series HW #0                        #
############################################

library(reprex)
#> Warning: package 'reprex' was built under R version 4.5.3
library(ggplot2)
library(tsbox)
#> Warning: package 'tsbox' was built under R version 4.5.3
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.5.3
#> Warning: package 'readr' was built under R version 4.5.3
#> Warning: package 'forcats' was built under R version 4.5.3
#> Warning: package 'lubridate' was built under R version 4.5.3

icnsa_all <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=ICNSA", stringsAsFactors = FALSE) 
icnsa_all$observation_date <- as.Date(icnsa_all$observation_date)
missing(icnsa_all)
#> [1] FALSE
anyNA(icnsa_all)
#> [1] FALSE

icnsa_cur <- data.frame(date  = as.Date(icnsa_all$observation_date),
                        value = icnsa_all$ICNSA)

icnsa_cur <- icnsa_cur[icnsa_cur$date <  '2026-8-29',]
ggplot(icnsa_cur, aes(x=date, y=value)) + geom_line()
```

![](https://i.imgur.com/Zz9oa4J.png)<!-- -->

``` r

# checking on the trend #
trend = ts_trend(icnsa_cur)
#> [time]: 'date'
ts_plot(ts_c(trend, icnsa_cur))
#> [time]: 'date' 
#> [time]: 'date' 
#> [time]: 'date'
```

![](https://i.imgur.com/HpOGjs7.png)<!-- -->

``` r

icnsa_cur$month <- as.factor(month(icnsa_cur$date))
icnsa_cur |>
  ggplot(aes(x=date, y=value, group=month)) +
  geom_line(aes(col=month)) +
  facet_grid(month ~ ., scales='free')
```

![](https://i.imgur.com/NSH7916.png)<!-- -->

``` r

icnsa_cur$winter <- icnsa_cur$month %in% c(11,12,1,2)

## dates of covid from WHO ##
## https://www.nm.org/healthbeat/medical-advances/new-therapies-and-drug-trials/covid-19-pandemic-timeline ##
icnsa_cur$covid <- '2020-3-01' < icnsa_cur$date & 
  icnsa_cur$date < '2023-5-01'

icnsa_cur$time <- as.numeric(icnsa_cur$date - min(icnsa_cur$date))
icnsa_cur$log_val <- log(icnsa_cur$value)

icnsa_model <- lm(log_val ~ time + winter + covid, data=icnsa_cur)
summary(icnsa_model)
#> 
#> Call:
#> lm(formula = log_val ~ time + winter + covid, data = icnsa_cur)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.93586 -0.20523  0.00082  0.19426  2.77066 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  1.268e+01  1.347e-02 941.590  < 2e-16 ***
#> time        -6.945e-06  1.059e-06  -6.558 6.38e-11 ***
#> winterTRUE   2.509e-01  1.332e-02  18.833  < 2e-16 ***
#> covidTRUE    3.195e-01  2.972e-02  10.748  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.349 on 3108 degrees of freedom
#> Multiple R-squared:  0.1328, Adjusted R-squared:  0.132 
#> F-statistic: 158.7 on 3 and 3108 DF,  p-value: < 2.2e-16

goal_date = as.Date("2026-08-29")
goal_time = as.numeric(goal_date - min(icnsa_cur$date))

icnsa_pred <- data.frame(time = c(goal_time),
                         winter = c(FALSE), covid= c(FALSE), log_val = c(NA))
pred_log <- predict(icnsa_model, icnsa_pred)
pred <- exp(pred_log)

## FINAL PREDICTION ##
pred
#>        1 
#> 275800.8
```

<sup>Created on 2026-09-02 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
