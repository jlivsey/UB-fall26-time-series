``` r
library(seasonal)
m = seas(AirPassengers, regression.variables = "td")
summary(m)
#> 
#> Call:
#> seas(x = AirPassengers, regression.variables = "td")
#> 
#> Coefficients:
#>                 Estimate Std. Error z value Pr(>|z|)    
#> Mon            -0.001527   0.003458  -0.442   0.6588    
#> Tue            -0.007677   0.003607  -2.129   0.0333 *  
#> Wed            -0.001125   0.003465  -0.325   0.7453    
#> Thu            -0.005350   0.003425  -1.562   0.1183    
#> Fri             0.004676   0.003447   1.357   0.1749    
#> Sat             0.003025   0.003568   0.848   0.3965    
#> Easter[1]       0.017999   0.007246   2.484   0.0130 *  
#> AO1951.May      0.109256   0.019651   5.560 2.70e-08 ***
#> MA-Seasonal-12  0.500775   0.077252   6.482 9.03e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> SEATS adj.  ARIMA: (0 1 0)(0 1 1)  Obs.: 144  Transform: log
#> AICc: 949.5, BIC: 976.4  QS (no seasonality in final):    0  
#> Box-Ljung (no autocorr.): 28.05   Shapiro (normality): 0.9902
```

<sup>Created on 2026-09-01 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>