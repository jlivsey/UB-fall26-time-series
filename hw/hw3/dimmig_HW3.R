##Dimmig HW 3

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
  ggtitle("Retail Sales: Beer, Wine, and Liquor Stores (NAICS 4453) (Seasonally Adjusted)") +
  ylab("Millions of $") + xlab("Year")

liq_true <- as.numeric(liquor_ts)    # keep the TRUE values (no NAs) for later comparison
time_index <- time(liquor_ts)
num <- length(liq_true)

#removing COVID 
y_diff <- diff(liquor_ts, lag=12)
pre2020 <- window(y_diff, end = c(2019,12))
thresh <- 3 * sd(pre2020, na.rm = TRUE)
covid_cand <- window(y_diff, start = c(2020,1), end = c(2021,12))
covid_flag <- abs(covid_cand - mean(pre2020)) > thresh
covid_months <- time(covid_cand)[covid_flag]
print(covid_flag)

covid_start <- c(2020, 3)
covid_end <- c(2021,4)
is_covid <- time_index >= (covid_start[1] + (covid_start[2] - 1) / 12) &
  time_index <= (covid_end[1]   + (covid_end[2] - 1) / 12)
sum(is_covid)  # number of months removed

liq_no_covid <- liq_true
liq_no_covid[is_covid] <- NA
liq_nocovid_ts <- ts(liq_no_covid, start = start(liquor_ts), frequency = 12)
ts.plot(liq_nocovid_ts)

#Create state equation
m<- 12 #seasonal monthly data, Tt = phi(Tt-1)+wt,2, St + st-1+ ... + st-11 = wt,2
p <- m

#observation is yt = (1 1 0 0 0 0 0 0 0 0 0 0)(Tt St St-1 ... St-11)-1 +v
#state equation 

build_Phi <- function(phi_trend,m){
  phi <- matrix(0,m,m)
  phi[1,1] <- phi_trend
  phi[2, 2:m] <- -1
  for (i in 3:m) phi[i, i-1]<- 1
  phi
}

A_full <- matrix(c(1,1,rep(0, m-2)), nrow=1)
A_nocovid <- array(rep(A_full, num), dim=c(1,m, num)) #rebuild the A matrix to be the A matrix if there is data or 0 if missing
A_nocovid[ , , is_covid] <- 0 #zero the covid/,missing months

mu0<- c(liq_no_covid[1], rep(0,m-1))
sigma0 <- diag(c(100, rep(10, m-1)))

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

init.par <- c(phi_trend = 1, sQ1 = 50, sQ2 = 50, sR = 50)
est_miss <- optim(init.par, Linn_miss, method = "BFGS", hessian = TRUE,
                  control = list(maxit = 500))
SE_miss <- sqrt(diag(solve(est_miss$hessian)))
cbind(estimate = est_miss$par, SE = SE_miss)


#time to smooth
phi_nocovid <- build_Phi(est_miss$par[1], m)
sQ_nocovid <- diag(0,m); sQ_nocovid[1,1] <- est_miss$par[2]; sQ_nocovid[2,2] <- est_miss$par[3]
sR_nocovid <- est_miss$par[4]
ks_nocovid <- Ksmooth(liq_nocovid_ts, A_nocovid, mu0,sigma0, phi_nocovid, sQ_nocovid, sR_nocovid)

Tsm_nocovid <- ts(ks_nocovid$Xs[1,,],start=1992, freq=12)
Ssm_nocovid <- ts(ks_nocovid$Xs[2,,], start=1992, freq=12)
fitted_nocovid <- Tsm_nocovid + Ssm_nocovid

imputed_covid <- window(fitted_nocovid, start=covid_start, end=covid_end)
true_covid <- window(liquor_ts, start=covid_start, end=covid_end)

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

Linn_full <- function(para) {
  phi_trend <- para[1]
  sQ1 <- para[2]   # trend shock sd
  sQ2 <- para[3]   # seasonal shock sd
  sR  <- para[4]   # observation noise sd
  gamma <- para[5]
  
  phi <- build_Phi(phi_trend, m)
  sQ  <- diag(0, m); sQ[1, 1] <- sQ1; sQ[2, 2] <- sQ2
  
  kf <- Kfilter(liquor_ts, A_full, mu0, sigma0, phi, sQ, sR, Gam = gamma, input = u_t)
  kf$like
}

init.par2 <- c(phi_trend = 1, sQ1 = 50, sQ2 = 50, sR=50, gamma=1000)
est_full <- optim(init.par2, Linn_full, method = "BFGS", hessian = TRUE,
                  control = list(maxit = 500))
SE_full <- sqrt(diag(solve(est_full$hessian)))
cbind(estimate = est_full$par, SE = SE_full)

Phi_full <- build_Phi(est_full$par[1], m)
sQ_full  <- diag(0, m); sQ_full[1, 1] <- est_full$par[2]; sQ_full[2, 2] <- est_full$par[3]
sR_full  <- est_full$par[4]
gamma_full <- est_full$par[5]

ks_full <- Ksmooth(liquor_ts, A_nocovid, mu0, sigma0, Phi_full, sQ_full, sR_full,
                   Gam = gamma_full, input = u_t)
Tsm_full <- ts(ks_full$Xs[1, , ], start = start(liquor_ts), frequency = 12)

## Compare the two trend estimates
tsplot(Tsm_nocovid, col = 2, lwd = 2, ylab = "Trend (millions of $)",
       main = "Trend component: missing-data model vs. intervention model")
lines(Tsm_full, col = 4, lwd = 2)
legend("topleft",
       legend = c("Step 3-4 model (COVID removed, imputed)",
                  "Step 6 model (COVID kept, level-shift intervention)"),
       col = c(2, 4), lwd = 2)

cat("Estimated COVID level shift (gamma):", round(gamma_full, 1), "\n")

#Forecast ahead
n.ahead = 12
liquor_fc = ts(append(liquor_ts, rep(0,n.ahead)), start=1992, freq=12 )
rmspe = rep(0,n.ahead)
x00 = ks_full$Xf[,,num]
P00 = ks_full$Pf[,,num]
Q = sQ_full%*%t(sQ_full)
R = sR_full%*%t(sR_full)

for (m in 1:n.ahead){
  xp = Phi_full%*%x00
  Pp = Phi_full%*%P00%*%t(Phi_full)+Q
  sig = A_full%*%Pp%*%t(A_full)+R
  K = Pp%*%t(A_full)%*%(1/sig)
  x00 = xp 
  P00 = Pp-K%*%A_full%*%Pp
  liquor_fc[num+m] = A_full%*%xp
  rmspe[m] = sqrt(sig) 
}

fc_next_month <-liquor_fc[num+1]
fc_next_year <- liquor_fc[num+12]

tsplot(liquor_fc, type='o', main='Forecast', ylab='Trend (millions of $)', xlim = c(2023,tsp(liquor_fc)[2]))
upp  = ts(y[(num+1):(num+n.ahead)]+2*rmspe, start= end(liquor_ts)+c(0,1), freq=12)
low  = ts(y[(num+1):(num+n.ahead)]-2*rmspe, start=end(liquor_ts)+ c(0,1), freq=12)
xx  = c(time(low), rev(time(upp)))
yy  = c(low, rev(upp))
polygon(xx, yy, border=8, col=gray(.5, alpha = .3))
abline(v= time(liquor_ts)[num], lty=3)
