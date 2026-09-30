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

## Part 1.1

First, we load in the data and examine it.

``` r
alc_sales <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=MRTSSM4453USN",
stringsAsFactors = FALSE) 
alc_sales <- data.frame(time  = as.Date(alc_sales$observation_date),
                          value = alc_sales$MRTSSM4453USN)
alc_ts <- ts(alc_sales$value, start = c(1992, 1), frequency = 12)

autoplot(alc_ts) +
  labs(title = "Retail Sales: Beer, Wine, and Liquor Stores", x = "Time", y = "Value") +
  theme_minimal()
```

![](https://i.imgur.com/ehMGpBh.png)<!-- -->

## Part 1.2

To obtain the Covid window, we will examine the difference in sales from the same month in the previous year by taking a difference using lag 12. The window in which these values are abnormally high are considered the Covid window. We find the maximum change from the previous year before 2020, and then consider the Covid window any observations larger than that maximum change. We manually exclude one large value in 2024, which is clearly beyond the end of the Covid pandemic.

``` r
alc_diffs <- ts(diff(alc_sales$value, lag=12), start=c(1993,1), frequency=12)

autoplot(alc_diffs) +
  labs(title = "Retail Sales: Change from Previous Year", x = "Time", y = "Value") +
  theme_minimal()
```

![](https://i.imgur.com/0BWYtMz.png)<!-- -->

``` r

max_pre2023 <- max(window(alc_diffs, end = c(2020, 1)))

covid_window <- alc_diffs > max_pre2023 
covid_window
#>        Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
#> 1993 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 1994 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 1995 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 1996 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 1997 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 1998 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 1999 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2000 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2001 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2002 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2003 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2004 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2005 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2006 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2007 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2008 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2009 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2010 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2011 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2012 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2013 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2014 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2015 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2016 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2017 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2018 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2019 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2020 FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> 2021  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2022 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2023 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2024 FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
#> 2025 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2026 FALSE FALSE FALSE FALSE FALSE FALSE FALSE
idx <- which(time(covid_window) == 2024 + (7/12))
covid_window[idx] <- FALSE
```

So, the window will be considered from Feb 2020 to April 2021. We will set these observations to NA. After, we plot them and see they have been removed. We use a log of the outcome since the variance in observations seems to increase after the covid period and this will help stabilize it.

``` r
alc_sales_nocovid <- alc_sales
alc_sales_nocovid$value[alc_sales_nocovid$time >= '2020-02-01' & alc_sales_nocovid$time <= '2021-04-01'] <- NA
alc_ts_nocovid <- ts(alc_sales_nocovid$value,start=c(1992,1), frequency=12 )
ly <- log(alc_ts_nocovid)

autoplot(alc_ts_nocovid) +
  labs(title = "Retail Sales: Covid Removed", x = "Time", y = "Value") +
  theme_minimal()
```

![](https://i.imgur.com/2HjKULx.png)<!-- -->
\## Part 1.3

The structural state space model can be written as:
``` math
T_t = \phi T_{t-1} + w_{1,t}
```
``` math
S_t +S_{t-1}+...+S_{t-11}=w_{t,2}
```
Then given the overall model as $`y_t=T_t+S_t+\nu_t`$, we can write the observation equation as
``` math
y_t =\begin{pmatrix} 1 & 1 & 0 & 
\cdots & 0 \end{pmatrix}\begin{pmatrix} T_t \\ S_t \\ S_{t-1} \\ \cdots \\ S_{t-10} \end{pmatrix} + v_t 
```
and the state equation as
``` math
\begin{pmatrix}
T_t \\
S_t \\
S_{t-1} \\
... \\
S_{t-10}
\end{pmatrix}
=
\begin{bmatrix}
\phi & 0 & \cdots & 0 \\
0 & -1 & \cdots & -1 \\
0 & 1 & 0 \cdots & 0 \\
\vdots & \vdots & \vdots & \vdots \\
0 \cdots & 0 & 1 & 0
\end{bmatrix}
\begin{pmatrix}
T_{t-1} \\
S_{t-1} \\
\vdots \\
S_{t-11}
\end{pmatrix}
+
\begin{pmatrix}
w_{t,1} \\
w_{t,2} \\
0 \\
\vdots \\
0
\end{pmatrix} 
```

## Part 1.4

Here we fit the model. We set minimum and maximum values for the parameter estimates to prevent a large phi value from creating explosive behavior and from avoiding some computational issues I ran into with the other values. For our initial conditions, we choose a mu vector based off the initial year’s data. We make the elements of the initial sigma matrix relatively large to reflect some level of uncertainty in the initial states. The initial value of phi is selected close to 1 since the values of the data seem highly dependent on the previous states. The other initial conditions were chosen somewhat arbitrarily based off similar values in the lecture notes, as I was unsure of how to best determine these.

``` r
num <- length(ly)
A <- matrix(c(1, 1, rep(0,10)), nrow=1)
A2   <- array(rep(A, num), dim = c(1, 12, num))
A2[, , is.na(alc_ts_nocovid)] <- 0   

Linn = function(para){
 Phi = diag(0,12) 
 Phi[1,1] = para[1] 
 Phi[2,]=c(0,rep(-1,11));
 for (i in rep(3:12)){
   Phi[i,i-1] = 1
 }
 sQ1 = para[2]; sQ2 = para[3] 
 sQ  = diag(0,12); sQ[1,1]=sQ1; sQ[2,2]=sQ2
 sR = para[4] 
 kf = Kfilter(ly, A2, mu0, Sigma0, Phi, sQ, sR)
 return(kf$like)  
}

first <- ly[1:12]
m0 <- mean(first, na.rm = TRUE)
s  <- first - m0; s <- s - mean(s)
mu0    <- matrix(c(m0, s[12], s[11:2]), ncol = 1)
Sigma0 <- diag(var(ly, na.rm = TRUE), 12)
init.par <- c(0.99, 0.02, 0.02, 0.05)
est <- optim(init.par, Linn, method = "L-BFGS-B",
             lower = c(0.5, 1e-4, 1e-4, 1e-4),
             upper = c(1, Inf, Inf, Inf), hessian = TRUE)

SE  = sqrt(diag(solve(est$hessian)))
u   = cbind(estimate = est$par, SE)
rownames(u) = c("Phi11", "sigw1", "sigw2", "sigv")
u
#>         estimate           SE
#> Phi11 1.00000000 0.0001153593
#> sigw1 0.01147398 0.0013261153
#> sigw2 0.01336513 0.0007661364
#> sigv  0.00010000 0.0002111102
```

``` r
Phi = diag(0,12) 
Phi[1,1] = est$par[1] 
Phi[2,]=c(0,rep(-1,11));
for (i in rep(3:12)){
   Phi[i,i-1] = 1
 }
sQ       = diag(0,12)
sQ[1,1]  = est$par[2]
sQ[2,2]  = est$par[3]   
sR       = est$par[4]   
ks       = Ksmooth(ly, A2, mu0, Sigma0, Phi, sQ, sR)  
xs_log <- ts(ks$Xs[1,,],start=1992, freq=12) + ts(ks$Xs[2,,], start=1992, freq=12)
xs <- exp(xs_log)
```

## Part 1.5

First is a plot of the observed points and the estimates. Second is a plot of the estimates and the actual Covid data points that were previously removed.

``` r
tsplot(alc_ts_nocovid, type = 'o', col = 4, pch = 19, main = "Observed (points) vs. Kalman-Smoothed Esimtates")
lines(xs, col = 6, lwd = 2)
```

![](https://i.imgur.com/8Cllan4.png)<!-- -->

``` r

tsplot(
  window(alc_ts, start = c(2020, 1), end = c(2021, 4)),
  main = "Kalman-Smoothed Estimates for Missing COVID Period",
  xlab = "Date",
  ylab = "Alcohol Sales"
)
lines(xs, col = 6, lwd = 2)
```

![](https://i.imgur.com/78r2cX7.png)<!-- -->

## Part 1.6

Now, we fit the model to account for the Covid period by incorporating an explicit intervention into the model. I didn’t use the log transformed data to see if it makes any difference in the final model estimates. I use an initial small value of gamma like in the class notes.

``` r
pulse_input <- as.numeric(is.na(alc_ts_nocovid))

Linn_covid = function(para){
 Phi = diag(0,12) 
 Phi[1,1] = para[1] 
 Phi[2,]=c(0,rep(-1,11));
 for (i in rep(3:12)){
   Phi[i,i-1] = 1
 }
 sQ1 = para[2]; sQ2 = para[3] 
 sQ  = diag(0,12); sQ[1,1]=sQ1; sQ[2,2]=sQ2
 sR = para[4]
 gam = para[5]
 kf = Kfilter(alc_ts, A, mu0_covid, Sigma0_covid, Phi, sQ, sR, Gam=gam, input=pulse_input)
 return(kf$like)  
}

first_covid <- alc_ts[1:12]
m0_covid <- mean(first_covid, na.rm = TRUE)
s_covid  <- first_covid - m0_covid
s_covid <- s_covid - mean(s_covid)
mu0_covid    <- matrix(c(m0_covid, s_covid[12], s_covid[11:2]), ncol = 1)
Sigma0_covid <- diag(var(alc_ts, na.rm = TRUE), 12)
init.par_covid <- c(0.99, 0.02, 0.02, 0.05, 0.05)
est_covid <- optim(init.par_covid, Linn_covid, method = "L-BFGS-B",
             lower = c(0.5, 1e-4, 1e-4, 1e-4, -Inf),
             upper = c(1, Inf, Inf, Inf, Inf), hessian = TRUE)

SE_covid  = sqrt(diag(solve(est_covid$hessian)))
u_covid   = cbind(estimate_covid = est_covid$par, SE_covid)
rownames(u_covid) = c("Phi11", "sigw1", "sigw2", "sigv", "Gam")
u_covid
#>       estimate_covid     SE_covid
#> Phi11      1.0000000 9.114544e-04
#> sigw1     46.4470503 5.233115e+00
#> sigw2     49.0007078 3.173875e+00
#> sigv      12.8340899 1.366250e+01
#> Gam        0.9491654 6.613283e+01
```

The standard error for Gamma is much larger than the actual estimates. Maybe Gamma is not needed in the model. Let’s try it without Gamma and see if it improves.

``` r
Linn_covid2 = function(para){
 Phi = diag(0,12) 
 Phi[1,1] = para[1] 
 Phi[2,]=c(0,rep(-1,11));
 for (i in rep(3:12)){
   Phi[i,i-1] = 1
 }
 sQ1 = para[2]; sQ2 = para[3] 
 sQ  = diag(0,12); sQ[1,1]=sQ1; sQ[2,2]=sQ2
 sR = para[4]
 kf = Kfilter(alc_ts, A, mu0_covid, Sigma0_covid, Phi, sQ, sR)
 return(kf$like)  
}

init.par_covid2 <- c(0.99, 0.02, 0.02, 0.05)
est_covid2 <- optim(init.par_covid2, Linn_covid2, method = "L-BFGS-B",
             lower = c(0.5, 1e-4, 1e-4, 1e-4),
             upper = c(1, Inf, Inf, Inf), hessian = TRUE)

SE_covid2  = sqrt(diag(solve(est_covid2$hessian)))
u_covid2   = cbind(estimate_covid2 = est_covid2$par, SE_covid2)
rownames(u_covid2) = c("Phi11", "sigw1", "sigw2", "sigv")
u_covid2
#>       estimate_covid2    SE_covid2
#> Phi11         1.00000 8.221087e-04
#> sigw1        46.44381 4.240948e+00
#> sigw2        49.01141 3.167051e+00
#> sigv         12.83651 1.359793e+01

Phi_covid = diag(0,12) 
Phi_covid[1,1] = est_covid2$par[1] 
Phi_covid[2,]=c(0,rep(-1,11));
for (i in rep(3:12)){
   Phi_covid[i,i-1] = 1
 }
sQ_covid       = diag(0,12)
sQ_covid[1,1]  = est_covid2$par[2]
sQ_covid[2,2]  = est_covid2$par[3]   
sR_covid       = est_covid2$par[4]   
ks_covid       = Ksmooth(alc_ts, A2, mu0_covid, Sigma0_covid, Phi_covid, sQ_covid, sR_covid)  
xs_covid <- ts(ks_covid$Xs[1,,],start=1992, freq=12) + ts(ks_covid$Xs[2,,], start=1992, freq=12)
```

Now, let’s see how the trend differs between this and the other model we fit where the Covid observations were removed.

``` r
xs_covid_trend <- ts(ks_covid$Xs[1,,],start=1992, freq=12)
xs_trend <- exp(ts(ks$Xs[1,,], start=1992, freq=12))

tsplot(alc_ts, type = 'o', col = 4, pch = 19, main = "Observed (points) vs. Kalman-Smoothed Trend")
lines(xs_trend, col = 1, lwd = 2)
lines(xs_covid_trend, col=2, lwd=2)
```

![](https://i.imgur.com/dEppAyG.png)<!-- -->

These appear to be very similar in terms of the extracted trend. In fact, the difference between the two models (in terms of trend) seems to be nearly negligible. So, it is possible this state-space structural model is very appropriate for handling the Covid spike, regardless of how we treat those values (missing or not).

``` r
xs_dif <- xs_trend - xs_covid_trend
mean(xs_dif)
#> [1] -24.5195
```

On average, the estimates for trend only differ by about 24.5, which is small given the scale is in the 1000s. We will make our estimate using the second model, which includes the Covid values. Since forcing those abnormalities to be missing doesn’t produce much of a change in the model trend, we will stick with the full data set.

## Part 1.7

Now, we want to use this model to make the forecast for next month and year. First, we provide a plot. Second, we provide the estimates.

``` r
n.ahead = 12
y       = ts(append(alc_ts, rep(0,n.ahead)), start=1992, freq=12)
rmspe   = rep(0,n.ahead) 
x00     = ks_covid$Xf[,,num]
P00     = ks_covid$Pf[,,num]
Q       = sQ%*%t(sQ) 
R       = sR%*%t(sR)
for (m in 1:n.ahead){
       xp = Phi_covid%*%x00
       Pp = Phi_covid%*%P00%*%t(Phi_covid)+Q
      sig = A%*%Pp%*%t(A)+R
        K = Pp%*%t(A)%*%(1/sig)
      x00 = xp 
      P00 = Pp-K%*%A%*%Pp
 y[num+m] = A%*%xp
 rmspe[m] = sqrt(sig) 
}


tsplot(y, type='o', main='', ylab='Alcohol Sales')
abline(v = 2026 + 6/12, lty=3) 
```

![](https://i.imgur.com/Ts2iQqA.png)<!-- -->

``` r

fc = y[(num+1):(num+n.ahead)]
window(y, start=c(2026,7), end=c(2027,7))
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 2026                                                       6412.000 6222.981
#> 2027 5339.660 5186.019 5690.795 5760.690 6371.604 6162.930 6409.459         
#>           Sep      Oct      Nov      Dec
#> 2026 5686.091 6081.388 6308.408 7853.742
#> 2027
```

# Problem 2

## Part 2.1

Now, using the same series we would like to try some other smoothing techniques. We will use a smoothing spline. The first value for the smoothing parameter will be 0.6. Our goal is for this value of the parameter is to strike a balance between being relatively smooth/flat while also following the actual trajectory of the data.

``` r
plot(alc_ts)
spline1 <- smooth.spline(time(alc_ts), alc_ts, spar=.6)
lines(spline1, lwd=2, col=4)
```

![](https://i.imgur.com/yeTwGvb.png)<!-- -->

## Part 2.2

Now, we will add several more potential smoothing spline trend estimates using different smoothing parameters. A larger smoothing parameter will create an even more flat trend estimate, but we avoid making it too large to prevent it from being overly flat. Likewise, a smaller smoothing parameter will create a more “choppy” estimate that is sensitive to fluctuations in the data. Again, we avoid making this too small to prevent an over fit trend.

``` r
spline2 <- smooth.spline(time(alc_ts), alc_ts, spar=.9)
spline3 <- smooth.spline(time(alc_ts), alc_ts, spar=.3)
```

## Part 2.3

Now, we plot all 3 of the smoothing spline trend estimates along with the actual data to see which may be the best choice.

``` r
plot(alc_ts, ylab="Alcohol Sales (Millions of Dollars)")
lines(spline1, lwd=2, col='blue')
lines(spline2, lwd=2, col='red')
lines(spline3, lwd=2, col='green')
title("Smoothing Spline Trend Estimates")
```

![](https://i.imgur.com/QhxiviO.png)<!-- -->

## Part 2.4

I would choose the trend estimates for the smoothing spline with parameter value of 0.6 as my final estimates for trend. The trend estimates plotted in green, representing the parameter value of 0.3, are overly bumpy as this model attempts to account for some of the seasonality of the series in its trend estimates. It is unnecessary to account for this in the trend estimates. The trend estimates in blue and red (parameter of 0.6 and 0.9 respectively) appear to be extremely similar, up until the Covid period occurs. At this point, the parameter value of 0.6 better accounts for the change in the series by modeling a sharp increase in trend at that point. The parameter value of 0.9 appears to be too smooth and models the increase in trend over a much longer period of time. The parameter value of 0.6 levels out more quickly after the Covid period, indicating the change in trend was short lived, and the series has largely leveled out post-Covid. This better reflects the data, which shows a flatter trend post-Covid, but with an increased seasonal effect. On the other hand, the parameter value of 0.9 appears to show an continued increasing trend post-Covid, which does not appear to be reflected in the data.

<sup>Created on 2026-09-30 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
