---
title: "dimmig_hw3"
format: html
editor: visual
---

```{r}

library(fredr)
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(fabletools)
library(forecast)     
library(tsbox)        
library(zoo)
library(tseries)     
library(lubridate)
library(tidyr)
library(fpp3)
library(astsa)
library(reprex)

fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MRTSSM4453USN"
liquor_raw <- read.csv(fred_url)
names(liquor_raw) <- c("date", "value")
liquor_raw$date  <- as.Date(liquor_raw$date)
liquor_raw$value <- as.numeric(liquor_raw$value)
liquor_raw <- na.omit(liquor_raw)

liquor_ts <- ts(liquor_raw$value,
                start = c(year(min(liquor_raw$date)), month(min(liquor_raw$date))),
                frequency = 12)

autoplot(liquor_ts) +
  ggtitle("Retail Sales: Beer, Wine, and Liquor Stores (NAICS 4453) (Not Seasonally Adjusted)") +
  ylab("Millions of $") + xlab("Year")

liq_true <- as.numeric(liquor_ts)    # keep the true raw values of the data
time_index <- time(liquor_ts) 
num <- length(liq_true)

#removing COVID - Instead of eyeballing the covid period
y_diff <- diff(liquor_ts, lag=12) #take the seasonal diff. (this year to last year)
pre2020 <- window(y_diff, end = c(2019,12)) #before covid
thresh <- 3 * sd(pre2020, na.rm = TRUE) 
covid_cand <- window(y_diff, start = c(2020,1), end = c(2021,12)) #possible covid region
covid_flag <- abs(covid_cand - mean(pre2020)) > thresh #if any year diff. in sales minus the average year diff. from before covid
# is greater than 3 s.d., then it will be flagged as part of our covid spike
covid_months <- time(covid_cand)[covid_flag]
print(covid_flag)

#The identified COVID window is from Mar 2020 to Apr 2021 (14 months). These months were flagged by the 
# covid_flag indicator as having a change over more than 3 SD from the pre-2020 mean
# the true covid values will remain in liquor_ts and liq_true, but a new time series data set will
# be created that excludes the identified COVID window
covid_start <- c(2020, 3)
covid_end <- c(2021,4)
is_covid <- time_index >= (covid_start[1] + (covid_start[2] - 1) / 12) &
  time_index <= (covid_end[1]   + (covid_end[2] - 1) / 12)
sum(is_covid)  # number of months removed

liq_no_covid <- liq_true
liq_no_covid[is_covid] <- NA #remove the data from our identified covid months
liq_nocovid_ts <- ts(liq_no_covid, start = start(liquor_ts), frequency = 12)
ts.plot(liq_nocovid_ts)

#Create state equation
# Structural model is y_t = T_t + S_t + v_t
# Where trend T_t = phi * T_(t-1) + w_t1
# seasonal S_t = -(S_(t-1) + S_(t-2) + ... + S_(t-11)) + w_t2
# v_t is noise/error

m<- 12 
p <- m

#observation is yt = (1 1 0 0 0 0 0 0 0 0 0 0)(Tt St St-1 ... St-11)-1 +v
#state equation 
#Function to build the phi matrix (first row is phi 0 0 ... , second row is 0 1 0 0 ..., the remaining rows are 0 0 -1 (for the diagonal))
build_Phi <- function(phi_trend,m){
  phi <- matrix(0,m,m)
  phi[1,1] <- phi_trend
  phi[2, 2:m] <- -1
  for (i in 3:m) phi[i, i-1]<- 1
  phi
}

A_full <- matrix(c(1,1,rep(0, m-2)), nrow=1)
A_nocovid <- array(rep(A_full, num), dim=c(1,m, num)) #rebuild the A matrix to be the A matrix if there is data or 0 if missing (covid)
A_nocovid[ , , is_covid] <- 0 #zero the covid/missing months

#Initial state mean and covariance; does not make much of an impact considering how large the data set is
mu0<- c(liq_no_covid[1], rep(0,m-1)) 
sigma0 <- diag(c(100, rep(10, m-1))) 

# Defining likelihood function
Linn_miss <- function(para) {
  phi_trend <- para[1]
  sQ1 <- para[2]   # trend shock sd
  sQ2 <- para[3]   # seasonal shock sd
  sR  <- para[4]   # observation noise sd
  
  phi <- build_Phi(phi_trend, m)
  sQ  <- diag(0, m); sQ[1, 1] <- sQ1; sQ[2, 2] <- sQ2
  
  kf <- Kfilter(liq_nocovid_ts, A_nocovid, mu0, sigma0, phi, sQ, sR)
  kf$like
}

# Four parameters being estimated by ML: phi, sd(w1), sd(w2), sd(v)
init.par <- c(phi_trend = 1, sQ1 = 50, sQ2 = 50, sR = 50)
est_miss <- optim(init.par, Linn_miss, method = "BFGS", hessian = TRUE,
                  control = list(maxit = 500))
SE_miss <- sqrt(diag(solve(est_miss$hessian)))
cbind(estimate = est_miss$par, SE = SE_miss)

#Looking at the estimated parameters, we see a phi being close to 1, implying that the trend behaves like a 
#random walk. The seasonal standard deviation (seasonal shock) is larger than the trend shock,meaning that there
#is more seasonal shock change when compared to the trend shock change. The observation noise is practically 0, meaning
#that most/all unexplained variation in the state shocks as opposed to the measurement error.

#Now that we have an estimate for our parameters, we can rebuild the model and then call
#Ksmooth (which uses both past and later data) to get a smooth state to fill in the covid months
phi_nocovid <- build_Phi(est_miss$par[1], m)
sQ_nocovid <- diag(0,m); sQ_nocovid[1,1] <- est_miss$par[2]; sQ_nocovid[2,2] <- est_miss$par[3]
sR_nocovid <- est_miss$par[4]
ks_nocovid <- Ksmooth(liq_nocovid_ts, A_nocovid, mu0,sigma0, phi_nocovid, sQ_nocovid, sR_nocovid)

Tsm_nocovid <- ts(ks_nocovid$Xs[1,,],start=1992, freq=12) #extract trend series
Ssm_nocovid <- ts(ks_nocovid$Xs[2,,], start=1992, freq=12) # extract season series
fitted_nocovid <- Tsm_nocovid + Ssm_nocovid #model estimates of yt

imputed_covid <- window(fitted_nocovid, start=covid_start, end=covid_end) #The predicted values of sales during covid
true_covid <- window(liquor_ts, start=covid_start, end=covid_end) #true value of sales during covid

tsplot(window(liquor_ts, start = c(2018, 1), end = c(2022, 12)),
       type = "o", col = 8, pch = 19, cex = .6,
       ylab = "Millions of $",
       main = "Kalman-smoothed imputation of the removed COVID window")
lines(fitted_nocovid, col = 4, lwd = 2)
points(time(imputed_covid), imputed_covid, col = 2, pch = 19)
legend("topleft",
       legend = c("observed (true)", "smoothed fit", "imputed (COVID months)"),
       col = c(8, 4, 2), pch = c(19, NA, 19), lty = c(NA, 1, NA), lwd = c(NA, 2, NA))

cat("True vs. imputed values during the removed COVID window:\n")
print(cbind(true = as.numeric(true_covid), imputed = as.numeric(imputed_covid)))

#Now fitting using all of the data and using a level-shift variable to account for covid period

u_t <- as.numeric(is_covid)

#Introducing a gamma parameter, which adds a covariate to our observation equation.
# During COVID months, the model will expect a gamma level shift 
Linn_full <- function(para) {
  phi_trend <- para[1]
  sQ1 <- para[2]   
  sQ2 <- para[3]   
  sR  <- para[4]  
  gamma <- para[5] 
  
  phi <- build_Phi(phi_trend, m)
  sQ  <- diag(0, m); sQ[1, 1] <- sQ1; sQ[2, 2] <- sQ2
  
  kf <- Kfilter(liquor_ts, A_full, mu0, sigma0, phi, sQ, sR, Gam = gamma, input = u_t)
  kf$like
}
#Find optimal estimates
init.par2 <- c(phi_trend = 1, sQ1 = 50, sQ2 = 50, sR=50, gamma=1000)
est_full <- optim(init.par2, Linn_full, method = "BFGS", hessian = TRUE,
                  control = list(maxit = 500))
SE_full <- sqrt(diag(solve(est_full$hessian)))
cbind(estimate = est_full$par, SE = SE_full)

#Compared to the earlier estimates, our new phi, sW1, sW2 and sR are quite similar. The introduced gamma parameter, which tells us
#how much in extra sales in millions per month that needs to be added to the model during the COVID window beyond what the seasonal and 
#trend patterns predict.


#Rebuild using our estimates
Phi_full <- build_Phi(est_full$par[1], m)
sQ_full  <- diag(0, m); sQ_full[1, 1] <- est_full$par[2]; sQ_full[2, 2] <- est_full$par[3]
sR_full  <- est_full$par[4]
gamma_full <- est_full$par[5]

ks_full <- Ksmooth(liquor_ts, A_full, mu0, sigma0, Phi_full, sQ_full, sR_full,
                   Gam = gamma_full, input = u_t)
Tsm_full <- ts(ks_full$Xs[1, , ], start = start(liquor_ts), frequency = 12)

## Compare the two trend estimates
tsplot(Tsm_nocovid, col = 2, lwd = 2, ylab = "Trend (millions of $)",
       main = "Trend component: missing-data model vs. intervention model")
lines(Tsm_full, col = 4, lwd = 2)
legend("topleft",
       legend = c("COVID removed, imputed model",
                  "COVID kept, level-shift intervention model"),
       col = c(2, 4), lwd = 2)
#Plot above overlays both of the two trend estimates (one with and without covid). The only real slighlty observable difference between 
#the trend lines are in the COVID window, which makes sense considering one trend was produced with while the other was without the COVID 
#data. However, the trends are still nearly identical in nature overall.


cat("Estimated COVID level shift (gamma):", round(gamma_full, 1), "\n")

#Forecast ahead
#Decided to use the full model that was produced with the COVID window because it contains true data and not imputed values.
n.ahead = 12
liquor_fc = ts(append(liquor_ts, rep(0,n.ahead)), start=1992, freq=12 )
rmspe = rep(0,n.ahead)
x00 = ks_full$Xf[,,num]
P00 = ks_full$Pf[,,num]
Q = sQ_full%*%t(sQ_full)
R = sR_full%*%t(sR_full)

for (h in 1:n.ahead){
  xp = Phi_full%*%x00
  Pp = Phi_full%*%P00%*%t(Phi_full)+Q
  sig = A_full%*%Pp%*%t(A_full)+R
  K = Pp%*%t(A_full)%*%(1/sig)
  x00 = xp 
  P00 = Pp-K%*%A_full%*%Pp
  liquor_fc[num+h] = A_full%*%xp
  rmspe[h] = sqrt(sig) 
}

fc_next_month <-liquor_fc[num+1]
fc_next_year <- liquor_fc[num+12]

tsplot(liquor_fc, type='o', main='Forecast', ylab='Liquor Sales (millions of $)', xlim = c(2023,tsp(liquor_fc)[2]))
upp  = ts(liquor_fc[(num+1):(num+n.ahead)]+2*rmspe, start= end(liquor_ts)+c(0,1), freq=12)
low  = ts(liquor_fc[(num+1):(num+n.ahead)]-2*rmspe, start=end(liquor_ts)+ c(0,1), freq=12)
xx  = c(time(low), rev(time(upp)))
yy  = c(low, rev(upp))
polygon(xx, yy, border=8, col=gray(.5, alpha = .3))
abline(v= time(liquor_ts)[num], lty=3)



#####PART 2
plot(liquor_ts)

#Adding in a smoothing spline to smooth the data and extract a general trend. I decided to use 3 different spar values.
#small spar = 0.2 for a low bias, high variance estimate (should be quite rough). Large spar = 1.0 for a high bias, low
#variance estimate (should be pretty smooth). A spar value in between the two that is GCV determined). 
smoothing_1 <- smooth.spline(time(liquor_ts), liquor_ts, spar = 0.2)
smoothing_2 <- smooth.spline(time(liquor_ts), liquor_ts)
smoothing_3 <- smooth.spline(time(liquor_ts), liquor_ts, spar = 1.0)

trend_1 <- ts(smoothing_1$y, start = start(liquor_ts), frequency = 12)
trend_2 <- ts(smoothing_2$y, start = start(liquor_ts), frequency = 12)
trend_3 <- ts(smoothing_3$y, start = start(liquor_ts), frequency = 12)

tsplot(liquor_ts, type = "l", col = 8, pch = 19, cex = .4,
       ylab = "Millions of $", main = "Trend estimates at three smoothing levels")
lines(trend_1,  col = 2, lwd = 2)
lines(trend_2, col = 4, lwd = 2)
lines(trend_3, col = 3, lwd = 2)
legend("topleft",
       legend = c("data",
                  "spar = 0.2 ",
                  paste0("spar = ", round(smoothing_2$spar, 2), " (GCV-selected)"),
                  "spar = 1.0 "),
       col = c(8, 2, 4, 3), lwd = c(2, 2, 2, 2))

# Looking at the plot, we can see that a spar of 0.2 follows the data closely and includes the seasonal trend, thus 
# not being the best if we are looking for just trend. A spar of 1.0 is very smooth, but misses some of the COVID spike
# and continues with a steeper slope from post COVID to present. Using the estimated GCV spar of 0.79, there is a smooth 
# overall trend captured with nearly no seasonal trend. This spline captures the COVID spike and slope reduction 
# afterwards. 
# Out of these 3 spars, I would use the spar of 0.79 to model the trend of the data. A smoother spline (spar = 1.0) contained 
# bias and failed to capture the steep increase and slight flattening from the COVID window on. A 
# rougher spline (spar = 0.2)  captured the overall trend, but primarily captured the seasonality of the data.




```
